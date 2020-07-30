-module(ndisc_redis).
-include_lib("kernel/include/logger.hrl").
-behaviour(gen_server).

%% API
-export([
  start_link/0,
  cluster_id/0,
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
  is_master = false
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

cluster_id() ->
  {ok, V} = application:get_env(?MODULE, cluster_id),
  V.

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

init([]) ->
  ets:new(?ND_REDIS_ETS, [named_table]),
  K = cluster_nodes_key(),
  Node = node(),
  {ok, _} = redis_q(["SADD", K, erlang:atom_to_list(Node)]),
  {ok, NL} = find_nodes(K),
  ets:insert(?ND_REDIS_ETS, {?NODES_KEY, NL}),
  IsMaster = is_master_node(Node),
  case IsMaster of
    true ->
      net_kernel:monitor_nodes(true, [{node_type, all}, nodedown_reason]),
      {NL2, DeadList} = split_nodes(NL, [], []),
      [redis_q(["SREM", K, erlang:atom_to_list(X)]) || X <- DeadList],
      {ok, #state{cluster_key = K, node_list = NL2, is_master = IsMaster}};
    false ->
      ML = lists:filter(fun is_master_node/1, NL),
      true = lists:any(fun(X) -> net_adm:ping(X) =:= pong end, ML),
      {ok, #state{cluster_key = K, node_list = NL, is_master = IsMaster}}
  end.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({nodeup, Master, Node} = Msg, S) ->
  ?LOG_WARNING("recv cast msg:~w", [Msg]),
  S2 =
    case is_master_node(Master) of
      true -> handle_nodeup(Node, S);
      false -> S
    end,
  {noreply, S2};
handle_cast({nodedown, Master, Node} = Msg, S) ->
  ?LOG_WARNING("recv cast msg:~w", [Msg]),
  S2 =
    case is_master_node(Master) of
      true -> handle_nodedown(Node, S);
      false -> S
    end,
  {noreply, S2};
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
  ?LOG_WARNING("unhandled msg:~w", [Info]),
  {noreply, State}.

terminate(Reason, #state{cluster_key = K}) ->
  ?LOG_INFO("terminate with ~p", [Reason]),
  redis_q(["SREM", K, erlang:atom_to_list(node())]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

redis_q(L) ->
  redis_proxy:q(L).

cluster_nodes_key() ->
  ClusterId = cluster_id(),
  lists:flatten(io_lib:format("~p_~ts", [?MODULE, ClusterId])).

find_nodes(K) ->
  case redis_q(["SMEMBERS", K]) of
    {ok, L} ->
      L2 = lists:map(fun(X) -> erlang:binary_to_atom(X, utf8) end, L),
      L3 = lists:usort(L2),
      {ok, L3};
    Err ->
      Err
  end.

split_nodes([], LiveAcc, DeadAcc) -> {LiveAcc, DeadAcc};
split_nodes([N | L], LiveAcc, DeadAcc) when N =:= node() ->
  split_nodes(L, [N | LiveAcc], DeadAcc);
split_nodes([N | L], LiveAcc, DeadAcc) ->
  case net_adm:ping(N) of
    pong -> split_nodes(L, [N | LiveAcc], DeadAcc);
    pang -> split_nodes(L, LiveAcc, [N | DeadAcc])
  end.

is_master_node(Node) when is_atom(Node) ->
  case re:run(erlang:atom_to_list(Node), lists:flatten(io_lib:format("^~p@*", [?MODULE]))) of
    match -> true;
    {match, _} -> true;
    _ -> false
  end.

is_remsh_node(Node) when is_atom(Node) ->
  case catch rpc:call(Node, init, get_argument, [remsh], 5000) of
    {ok, _} -> true;
    _ -> false
  end.

handle_nodeup(Node, S) when is_atom(Node) ->
  handle_nodeup([Node], S);
handle_nodeup(Nodes, #state{is_master = Master, node_list = NL} = S) when is_list(Nodes) ->
  Nodes2 = lists:filter(fun(X) -> not is_remsh_node(X) end, Nodes),
  case Nodes2 =:= [] of
    true ->
      S;
    false ->
      NL2 = lists:usort(NL ++ Nodes2),
      ets:insert(?ND_REDIS_ETS, {?NODES_KEY, NL2}),
      case Master of
        true ->
          notify_node_change({nodeup, node(), Nodes2}, NL2);
        false ->
          pass
      end,
      S#state{node_list = NL2}
  end.

handle_nodedown(Node, S) when is_atom(Node) ->
  handle_nodedown([Node], S);
handle_nodedown(Nodes, #state{is_master = Master, node_list = NL, cluster_key = K} = S) when is_list(Nodes) ->
  NL2 = lists:usort(NL -- Nodes),
  case NL2 =:= NL of
    true ->
      S;
    false ->
      ets:insert(?ND_REDIS_ETS, {?NODES_KEY, NL2}),
      case Master of
        true ->
          [redis_q(["SREM", K, erlang:atom_to_list(X)]) || X <- Nodes],
          notify_node_change({nodedown, node(), Nodes}, NL2);
        false ->
          pass
      end,
      S#state{node_list = NL2}
  end.

notify_node_change(Msg, L) ->
  L2 = lists:filter(fun(X) -> not is_master_node(X) end, L),
  lists:foreach(fun(X) -> gen_server:cast({?SERVER, X}, Msg) end, L2).


