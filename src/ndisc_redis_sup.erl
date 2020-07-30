-module(ndisc_redis_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  SupFlags = #{strategy => one_for_one, intensity => 10, period => 60},
  ChildSpecs = [{ndisc_redis, {ndisc_redis, start_link, []}, permanent, 5000, worker, [ndisc_redis]}],
  {ok, {SupFlags, ChildSpecs}}.
