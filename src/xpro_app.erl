-module(xpro_app).
-behaviour(application).

-export([start/0,start/2]).
-export([stop/1]).


start() ->
	application:start(xpro).
start(_Type, _Args) ->
	lager:start(),
	application:start(sasl),
	application:start(crypto),
	application:start(inets),
	application:start(ranch),
	application:start(cowlib),
	application:start(cowboy),

	Dispatch = cowboy_router:compile([
		{'_', [
			{"/xgate/websocket/:xnest_name", xgate_websocket_handler, []},
			{"/xgate/http", xgate_http_handler, []},
			{"/:bucket/:key", xgate_binary_handler, []},
			{"/[...]", cowboy_static, {priv_dir, 'xpro', <<"">>}}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 10, [{port, 8080}],
		[{env, [{dispatch, Dispatch}]}]),

	PrivDir = code:priv_dir(xpro),
	{ok, _} = cowboy:start_https(https, 10, 
				[{port, 8443}
				%, {cacertfile, PrivDir ++ "/../ssl/cowboy-ca.crt"}
				%, {certfile, PrivDir ++ "/../ssl/server.crt"}
				%, {keyfile, PrivDir ++ "/../ssl/server.key"}],
				, {cacertfile, PrivDir ++ "/../ssl/1_cross_Intermediate.crt"}
				, {certfile, PrivDir ++ "/../ssl/3_user_xpro.im.crt"}
				, {keyfile, PrivDir ++ "/../ssl/4_user_xpro.im.key"}],
				[{env, [{dispatch, Dispatch}]}]
			),
	

	xpro_sup:start_link().

stop(_State) ->
	ok.
