-module(xpro_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	XNestManager = {xnest_manager,
	         {xnest_manager, start_link, []},
	         permanent, 5000, worker, [xnest_manager]
	},
	NickName = {nickname,
	         {nickname, start_link, []},
	         permanent, 5000, worker, [nickname]
	},
	
	Procs = [XNestManager, NickName],
	{ok, {{one_for_one, 1, 5}, Procs}}.
