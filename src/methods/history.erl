-module(history).
-include("../lager.hrl").

%-behaviour(xgate_http_handler).


-export([do/1]).


%% type()
-type props() :: list().



%% @doc 执行方法
%% @private
-spec do(props()) -> term().
do(Params) ->
lager:info("before xnestname :~p", [Params]),
	XNestName = proplists:get_value(<<"xnest">>, Params),
	Count = proplists:get_value(<<"count">>, Params),
lager:info("xnestname :~p, history count:~p", [XNestName, Count]),
        {ok, XNestPid} = xnest_manager:get_xnest(XNestName),
        {ok, History} = xnest:history(XNestPid, Count),
lager:info("hisotry:~p", History),
        History.
