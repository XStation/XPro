-module(xgate_http_handler).
-behaviour(cowboy_http_handler).
-include("lager.hrl").

%%API

%%http handler
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

%% @private
init(_, Req, _Opts) ->
	{ok, Req, #state{}}.

%% @private
handle(Req, State=#state{}) ->
	{Method, Req1} = cowboy_req:method(Req),
lager:info("~p", [Method]),

	{ok, RequestBody, Req2} = cowboy_req:body(Req1),
lager:info("~p", [RequestBody]),

	DecodeBody = jsx:decode(RequestBody),
lager:info("~p", [DecodeBody]),

	RequestId = proplists:get_value(<<"request_id">>, DecodeBody),
	RequestMethod = proplists:get_value(<<"request">>, DecodeBody),
	Xnest = proplists:get_value(<<"xnest">>, DecodeBody),
	Result = case RequestMethod of 
		<<"members">> -> members(Xnest);
		_ -> <<"needtoimplement">>
	end,

	Response = make_response(RequestId, Xnest, Result),

	{ok, Req3} = cowboy_req:reply(200, [], Response, Req2),
	{ok, Req3, State}.

%% @private
terminate(_Reason, _Req, _State) ->
	ok.




%%===================Internal functions===============


members(XNestName) ->
	{ok, XNestPid} = xnest_manager:get_xnest(XNestName),
	{ok, Members} = xnest:members(XNestPid),
	Members.

make_response(Request_id, XnestName, Response) ->
	ResponseList = [{<<"request_id">>, Request_id}, {<<"xnest">>, XnestName}, {<<"response">>, Response}],
	jsx:encode(ResponseList).
