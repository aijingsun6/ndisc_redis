-module(ndisc_redis).
-include_lib("kernel/include/logger.hrl").
-behaviour(gen_server).

%% API
-export([
  start_link/1,
  start_link/2,
  q/0,
  q/1
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-define(SERVER, ?MODULE).
-define(ND_REDIS_ETS, ndisc_redis_ets).
-define(NODES_KEY, nodes).
-record(state, {
  cluster_key,
  node_list = [],
  is_remsh = false
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(CK) ->
  start_link(?SERVER, CK).

start_link(Name, CK) when is_atom(Name), is_binary(CK) ->
  gen_server:start_link({local, Name}, ?MODULE, [CK], []).

q() ->
  case ets:lookup(?ND_REDIS_ETS, ?NODES_KEY) of
    [{_, L}] -> L;
    [] -> []
  end.

q(RE) ->
  L = q(),
  F = fun(X) ->
    case re:run(erlang:atom_to_list(X), RE) of
      match -> true;
      {match, _} -> true;
      _ -> false
    end
      end,
  lists:filter(F, L).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([CK]) ->
  process_flag(trap_exit, true),
  ets:new(?ND_REDIS_ETS, [named_table]),
  Node = node(),
  case is_remsh_node(Node) of
    true ->
      {ok, #state{is_remsh = true, cluster_key = CK, node_list = []}};
    false ->
      {ok, _} = redis_proxy:q(["SADD", CK, erlang:atom_to_binary(Node, utf8)]),
      net_kernel:monitor_nodes(true, [{node_type, all}, nodedown_reason]),
      {ok, L} = find_nodes_redis(CK),
      {Live, Dead} = split_nodes(L, [], []),
      remove_downs(CK, Dead),
      L2 = remove_duplicate([Node | Live]),
      ets:insert(?ND_REDIS_ETS, {?NODES_KEY, L2}),
      {ok, #state{is_remsh = false, cluster_key = CK, node_list = L2}}
  end.

handle_call(Request, _From, State) ->
  ?LOG_WARNING("unhandled call msg:~w", [Request]),
  {reply, ok, State}.

handle_cast(Request, State) ->
  ?LOG_WARNING("unhandled cast msg:~w", [Request]),
  {noreply, State}.


handle_info({nodeup, Node, _InfoList} = Msg, S) ->
  ?LOG_WARNING("recv msg:~w", [Msg]),
  S2 = handle_nodeup(Node, S),
  {noreply, S2};
handle_info({nodeup, Node} = Msg, S) ->
  ?LOG_WARNING("recv msg:~w", [Msg]),
  S2 = handle_nodeup(Node, S),
  {noreply, S2};
handle_info({nodedown, Node, _InfoList} = Msg, S) ->
  ?LOG_WARNING("recv msg:~w", [Msg]),
  S2 = handle_nodedown(Node, S),
  {noreply, S2};
handle_info({nodedown, Node} = Msg, S) ->
  ?LOG_WARNING("recv msg:~w", [Msg]),
  S2 = handle_nodedown(Node, S),
  {noreply, S2};
handle_info(Info, State) ->
  ?LOG_WARNING("unhandled info msg:~w", [Info]),
  {noreply, State}.

terminate(Reason, #state{cluster_key = K}) ->
  ?LOG_INFO("terminate with ~p,~p", [K, Reason]),
  redis_proxy:q(["SREM", K, erlang:atom_to_list(node())]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

find_nodes_redis(K) ->
  case redis_proxy:q(["SMEMBERS", K]) of
    {ok, L} when is_list(L) ->
      L2 = lists:map(fun(X) -> erlang:binary_to_atom(X, utf8) end, L),
      L3 = lists:usort(L2),
      {ok, L3};
    Err ->
      Err
  end.

split_nodes([], LiveAcc, DeadAcc) ->
  {LiveAcc, DeadAcc};
split_nodes([N | L], LiveAcc, DeadAcc) ->
  case is_node_alive(N) of
    true ->
      split_nodes(L, [N | LiveAcc], DeadAcc);
    false ->
      split_nodes(L, LiveAcc, [N | DeadAcc])
  end.

is_node_alive(N) when N =:= node() ->
  true;
is_node_alive(N) ->
  case net_adm:ping(N) of
    pong -> true;
    pang -> false
  end.

is_remsh_node(Node) when Node =:= node() ->
  case catch init:get_argument(remsh) of
    {ok, _} -> true;
    _ -> false
  end;

is_remsh_node(Node) when is_atom(Node) ->
  case catch rpc:call(Node, init, get_argument, [remsh], 5000) of
    {ok, _} -> true;
    _ -> false
  end.

handle_nodeup(Node, S) when is_atom(Node) ->
  handle_nodeup([Node], S);
handle_nodeup(Nodes, #state{node_list = NL} = S) when is_list(Nodes) ->
  Nodes2 = lists:filter(fun(X) -> not is_remsh_node(X) end, Nodes),
  case Nodes2 =:= [] of
    true ->
      S;
    false ->
      NL2 = remove_duplicate(NL ++ Nodes2),
      ets:insert(?ND_REDIS_ETS, {?NODES_KEY, NL2}),
      S#state{node_list = NL2}
  end.

handle_nodedown(Node, S) when is_atom(Node) ->
  handle_nodedown([Node], S);
handle_nodedown(Nodes, #state{node_list = NL, cluster_key = CK} = S) when is_list(Nodes) ->
  Nodes2 = lists:filter(fun(X) -> not is_remsh_node(X) end, Nodes),
  case Nodes2 =:= [] of
    true ->
      S;
    false ->
      NL2 = remove_duplicate(NL -- Nodes2),
      Node = node(),
      case lists:max(Nodes2) of
        Node -> remove_downs(CK, NL2);
        _ -> pass
      end,
      ets:insert(?ND_REDIS_ETS, {?NODES_KEY, NL2}),
      S#state{node_list = NL2}
  end.

remove_duplicate(L) ->
  sets:to_list(sets:from_list(L)).

remove_downs(CK, L) ->
  L2 = lists:map(fun(X) -> erlang:atom_to_binary(X, utf8) end, L),
  redis_proxy:q(["SREM", CK | L2]).



