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
	application:start(ranch),
	application:start(cowlib),
	application:start(cowboy),

	Dispatch = cowboy_router:compile([
		{'_', [
			{"/xchat", xchat_websocket_handler, []},
			{"/[...]", cowboy_static, {priv_dir, 'XPro', <<"">>}}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 10, [{port, 8080}],
		[{env, [{dispatch, Dispatch}]}]),
	xpro_sup:start_link().

stop(_State) ->
	ok.
