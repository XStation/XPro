-module(member_count).
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
lager:info("xnestname :~p", [XNestName]),
	{ok, XNestPid} = xnest_manager:get_xnest(XNestName),
	{ok, Count} = xnest:status(XNestPid, client_counts),
	Count.
