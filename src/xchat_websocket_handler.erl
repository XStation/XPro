-module(xchat_websocket_handler).
-include("lager.hrl").


%% API
-export([send_self/1]).

%% websocket handler
-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).


-record(state, {
	xnest_name = <<>> :: binary(),
	xnest_pid  = <<>> :: pid()
}).


%% type()
-type req() :: cowboy_http:req().


%% @private
init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.

%% @private
websocket_init(_TransportName, Req, _Opts) ->
	{XNestName, NewReq} = parse_xnest_name(Req),
	{ok, XNestPid, JoinResult} = join_xnest(XNestName),
	State = #state{
		xnest_name	= XNestName,
		xnest_pid	= XNestPid
	},
	{ok, NewReq, State }.



%% @private
%% @doc Receive Message from client and send it to XNest
websocket_handle({text, Msg}, Req, State ) ->
	XNestPid = State#state.xnest_pid,
	xnest:input(XNestPid, {self(), text, Msg}),			%% Use xnest API to send message
	{ok, Req, State};

%% @private
%% @doc Receive Message from client and send it to XNest, But unused
websocket_handle({binary, Bin}, Req, State) ->
	XNestPid = State#state.xnest_pid,
	xnest:input(XNestPid, {self(), binary, Bin}),		%% Use xnest API to send message
	{ok, Req, State};

%% @private
websocket_handle(_Data, Req, State) ->
	{ok, Req, State}.


%% @private
%% @doc Receive Message from xnest and send it to client
websocket_info({FromPid, text, Msg}, Req, State) ->
	%ResponseMsg  = {{<<"from">>, FromPid}, {<<"msg">>, Msg}},
	_FromPid = list_to_binary(pid_to_list(FromPid)),
	ResponseMsg  = <<_FromPid/binary, ":", Msg/binary>>,
	{reply, {text, ResponseMsg}, Req, State};

%% @private
%% @doc Receive timeout event , But now unused
websocket_info({timeout, _Ref, Msg}, Req, State) ->
	{reply, {text, Msg}, Req, State};

%% @private
websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

%% @private
%% @doc leave xnest when websocket down!
websocket_terminate(_Reason, _Req, State) ->
	XNestPid = State#state.xnest_pid,
	xnest:leave(XNestPid, self()),
	ok.


%%===================Internal functions===============

%% @doc Parse xnest Name from HTTP Req
-spec parse_xnest_name(req()) -> {binary(), req()}.
parse_xnest_name(Req) ->
	cowboy_req:qs_val(<<"xnest">>, Req, <<>>).

%% @doc Join a xnest 
-spec join_xnest(binary()) -> {ok, pid()}.
join_xnest(XNestName) ->
	{ok, XNestPid} = xnest_manager:get_xnest(XNestName),   %% I can know what format will be return by xnest_manager
	{ok, JoinResult} = xnest:join(Pid, self()),
	send_welcome(self(), JoinResult),
	{ok, XNestPid}.

%% @doc send join message to client
-spec send_welcome(pid(), binary()) -> any().
send_welcome(SelfPid, WelcomeMsg) ->
	SelfPid ! {SelfPid, text, WelComeMsg}.
	

