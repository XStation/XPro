-module(analyze).

-export([start/0, stop/0, sample/0, do/0]).

-define(DATAFILE, "/dev/shm/percept2.txt").
%http://refactoringtools.github.io/percept2/
%trace_profile_option() = procs_basic | ports_basic | procs | ports | schedulers | running | message | migration | garbage_collection | s_group | all | {callgraph, [module_name()]}


start() ->
	percept2:profile(?DATAFILE, [procs_basic, message, procs, {callgraph, [xnest, xnest_manager, ets]}]).

stop() ->
	percept2:stop_profile().

do() ->
	percept2:analyze([?DATAFILE]),
	percept2:start_webserver(8888).

sample() ->
	percept2_sampling:start([all], 60000, ".").

