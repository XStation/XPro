-module(xgate_websocket_handler).
-include("lager.hrl").


%% API


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
-type json() :: jsx:json_text().


%% @private
init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.

%% @private
websocket_init(_TransportName, Req, _Opts) ->
	{XNestName, NewReq} = parse_xnest_name(Req),
	{ok, XNestPid} = join_xnest(XNestName),
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
	ResponseMsg = make_response(FromPid, State#state.xnest_name, Msg),
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
lager:warning("~p leave xnest ~p", [self(), XNestPid]),
	Msg = <<"leave">>,
	xnest:input(XNestPid, {self(), text, Msg}),			%% Use xnest API to send leave message
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
	{ok, JoinResult} = xnest:join(XNestPid, self()),
	xnest:input(XNestPid, {self(), text, JoinResult}),			%% Use xnest API to send join message
	{ok, XNestPid}.


%% @doc generate response 
-spec make_response(pid(), binary(), binary()) -> json().
make_response(FromPid, Xnest, Msg) ->
	Time = time2binary(erlang:localtime()),
	From = list_to_binary(pid_to_list(FromPid)),
	Msg_ = [ {<<"from">>, From}
		,{<<"xnest">>, Xnest}
		,{<<"payload">>, Msg}
		,{<<"send_time">>, Time}
	],
	Response = jsx:encode(Msg_),
lager:info("~p", [Response]),
	Response.

%% @doc time to binary
-spec time2binary(tuple()) -> binary().
time2binary({{Y, M, D}, {H, I, S}}) ->
	Y_ = integer_to_binary(Y),
	M_ = integer_to_binary(M),
	D_ = integer_to_binary(D),
	H_ = integer_to_binary(H),
	I_ = integer_to_binary(I),
	S_ = integer_to_binary(S),
	<<Y_/binary, "-", M_/binary, "-", D_/binary, " ", H_/binary, ":", I_/binary, ":", S_/binary>>.
