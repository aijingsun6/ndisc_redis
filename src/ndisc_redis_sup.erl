-module(ndisc_redis_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  SupFlags = #{strategy => one_for_one, intensity => 10, period => 60},
  {ok, CK} = application:get_env(ndisc_redis, cluster_id),
  ChildSpecs = [{ndisc_redis, {ndisc_redis, start_link, [CK]}, permanent, 5000, worker, [ndisc_redis]}],
  {ok, {SupFlags, ChildSpecs}}.
