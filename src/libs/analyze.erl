-module(analyze).

-export([start/0, stop/0]).

%http://refactoringtools.github.io/percept2/
%trace_profile_option() = procs_basic | ports_basic | procs | ports | schedulers | running | message | migration | garbage_collection | s_group | all | {callgraph, [module_name()]}

start() ->
	File = "/tmp/percept2.txt",
	percept2:profile(File, [procs_basic, message,{callgraph, [xnest, xnest_manager]]).
stop() ->
	File = "/tmp/percept2.txt",
	percept2:stop_profile(),
	percept2:analyze([File]),
	percept2:start_webserver(8888).
