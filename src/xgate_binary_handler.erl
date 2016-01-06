-module(xgate_binary_handler).
-behaviour(cowboy_http_handler).
-include("lager.hrl").


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
lager:info("Req: ~p", [Req]),
	{Bucket, Req1} = cowboy_req:binding(bucket, Req, <<"">>),
	case Bucket of 
		<<>> -> 
			{ok, Req2} = cowboy_req:reply(400, [], <<"Bad Request">>, Req1),
			{ok, Req2, State};
		_ -> 
			{Key, Req2} = cowboy_req:binding(key, Req1, <<"">>),
			{ok, Req3} = case riak_worker:sync_get(<<"default">>, Bucket, Key) of 
				{error,notfound} ->				
						cowboy_req:reply(404, [{<<"Content-Type">>, <<"text/html">>}], <<"<h1>404 NOT FOUND</h1>are you kidding me?">>, Req2);
				Binary ->
						cowboy_req:reply(200, [], Binary, Req2)
			end,
			{ok, Req3, State}
	end.

%% @private
terminate(_Reason, _Req, _State) ->
	ok.



