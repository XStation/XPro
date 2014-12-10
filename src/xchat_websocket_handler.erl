-module(xchat_websocket_handler).
-include("lager.hrl").
%-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).



-export([send_self/1]).

init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
	erlang:start_timer(0, self(), <<"HI">>),
	lager:warning("self:~p", [self()]),
	lager:warning("req:~p", [Req]),
	{ok, Req, {} }.


websocket_handle({text, <<"SEND#", ToUserid/binary>>}, Req, State) ->
    case pid_manager:get_pid(<<"RECEIVE#", ToUserid/binary>>) of 
        {ok, Pid} ->
            NewState = {<<"SEND#">>, Pid},
            pid_manager:add_pid({NewState, self()}),%% 把发送流的进程插入到ets, 目前没有太大意义
lager:warning("111111111: ~p", [NewState]),
	        {reply, {text, <<"GO">>}, Req, NewState};
        {error, Reason} ->
            {reply, {text, Reason}, Req, State}
    end;

websocket_handle({text, Msg}, Req, {<<"SEND#">>, ToPid}) ->
lager:warning("msg to Pid:~p", [ToPid]),
    ToPid ! {text, Msg},
	{ok, Req, {<<"SEND#">>, ToPid}};


websocket_handle({binary, Bin}, Req, {<<"SEND#">>, ToPid}) ->
    ToPid ! {binary, Bin},
lager:warning("binary to Pid:~p", [ToPid]),
	{ok, Req, {<<"SEND#">>, ToPid}};

websocket_handle({binary, _Bin}, Req, State) ->
	{ok, Req, State};

websocket_handle(_Data, Req, State) ->
lager:warning("any data :~p", [_Data]),
	lager:warning("req:~p", [Req]),
	{ok, Req, State}.




websocket_info({timeout, _Ref, Msg}, Req, State) ->
	{reply, {text, Msg}, Req, State};


websocket_info({text, Msg}, Req, State) ->
	{reply, {text, Msg}, Req, State};

websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.


%%===================Internal functions===============
parse_xnest_name(Req) ->
	ok.


join_xnest(XNestName) ->
	ok.

send_self(Pid) ->
	Pid ! {text, <<"Iam god!">>}.
	
