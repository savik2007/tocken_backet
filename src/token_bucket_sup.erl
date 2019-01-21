-module(token_bucket_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [{token_bucket,{token_bucket, start_link, []}, permanent, 1000, worker, [token_bucket]}],
	{ok, {{one_for_one, 1, 1000}, Procs}}.
