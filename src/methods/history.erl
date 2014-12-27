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
	XNestName = proplists:get_value(<<"xnest">>, Params),
	Count = proplists:get_value(<<"count">>, Params),
        {ok, XNestPid} = xnest_manager:get_xnest(XNestName),
        {ok, History} = xnest:history(XNestPid, Count),
        History.
