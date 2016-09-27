-module(xgate_http_handler).
-behaviour(cowboy_http_handler).
-include("lager.hrl").


%%http handler
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

%% type()
-type json() :: jsx:json_text().


%% callback 
%-callback beforedo(Params :: list()) -> NewParams::list().
-callback do(Params::list()) -> Result::any().
%-callback afterdo(Result::any()) -> NewResult::any().



%% @private
init(_, Req, _Opts) ->
	{ok, Req, #state{}}.

%% @private
handle(Req, State=#state{}) ->
	{HttpMethod, Req1} = cowboy_req:method(Req),
lager:info("http method: ~p", [HttpMethod]),

	{ok, HttpBody, Req2} = cowboy_req:body(Req1),
lager:info(" http body: ~p", [HttpBody]),

	DecodeBody = jsx:decode(HttpBody),
lager:info("decode body:~p", [DecodeBody]),

	RequestId = proplists:get_value(<<"request_id">>, DecodeBody),
	RequestMethod = proplists:get_value(<<"method">>, DecodeBody),
	RequestParams = proplists:get_value(<<"params">>, DecodeBody),
	Result = exec_method(RequestMethod, RequestParams),
lager:info("exec result: ~p", [Result]),
	Response = make_response(RequestId, Result),
lager:info("response: ~p", [Response]),
	Response = make_response(RequestId, Result),
	{ok, Req3} = cowboy_req:reply(200, [], Response, Req2),
	{ok, Req3, State}.

%% @private
terminate(_Reason, _Req, _State) ->
	ok.




%%===================Internal functions===============
%% @doc execute a method 
-spec exec_method(binary(), list()) -> term().
exec_method(Method, Params) ->
	Module = dispatch:module(Method),
	Result = case Module of 
		<<"404">> -> <<"404">>;
		_ -> Module:do(Params)
	end,
	Result.

%% @doc generate json response 
-spec make_response(binary(), term()) -> json().
make_response(Request_id, Result) ->
	ResponseList = [{<<"request_id">>, Request_id},  {<<"result">>, Result}],
	jsx:encode(ResponseList).
