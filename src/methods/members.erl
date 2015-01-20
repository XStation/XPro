-module(members).
-include("../lager.hrl").

%-behaviour(xgate_http_handler).


-export([do/1]).


%% type()
-type props() :: list().



%% @doc 执行方法
%% @private
-spec do(props()) -> term().
do(Params) ->
	XNestName = proplists:get_value(<<"xnest">>, Params),
lager:info("xnestname :~p", [XNestName]),
        {ok, XNestPid} = xnest_manager:get_xnest(XNestName),
        {ok, Members} = xnest:members(XNestPid),
		[[pid2binary(Pid), Name] || {Pid, Name} <- Members].



%% @doc pid to binary
-spec pid2binary(pid()) -> binary().
pid2binary(Pid) -> list_to_binary(pid_to_list(Pid)).
