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
	nickname	= <<>> :: binary(),
	xnest_name	= <<>> :: binary(),
	xnest_pid	= <<>> :: pid()
}).


%% type()
-type req() :: cowboy_http:req().
-type json() :: jsx:json_text().


%% @private
init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket};

%% @private
init({ssl, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.



%% @private
websocket_init(_TransportName, Req, _Opts) ->
	{XNestName, Req1} = parse_xnest_name(Req),
	{NickName, Req2} = parse_nickname(Req1),
	self() ! {'self'},		%% send a command to self 
	{ok, XNestPid} = join_xnest(XNestName, NickName),
	State = #state{
		nickname	= NickName,
		xnest_name	= XNestName,
		xnest_pid	= XNestPid
	},
	Req3 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req2),
	self() ! {'member_count'},		%% send a command to self 
	self() ! {'members'},		%% send a command to self 
	{ok, Req3, State}.



%% @private
%% @doc Receive Message from client and send it to XNest
websocket_handle({text, Msg}, Req, State ) ->
	lager:info("ip: ~p send a message:~ts", [element(8, Req), Msg]),
	parse_msg(Msg, State),			%%parse  message and do it with type
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
%% @doc Receive command from self, and send self message (like pid) to js client 
websocket_info({'self'}, Req, State) ->
	ResponseMsg = make_response(self(), State#state.xnest_name, {'self', pid2binary(self())}),
	{reply, {text, ResponseMsg}, Req, State};


%% @private
%% @doc Receive members command from self 
websocket_info({'members'}, Req, State) ->
	XNestPid = State#state.xnest_pid,
	{ok, _Members} = xnest:members(XNestPid),
	Members = [ [{<<"pid">>, pid2binary(Pid)}, {<<"nickname">>, Name}] ||{Pid, Name} <- _Members],
	ResponseMsg = make_response(self(), State#state.xnest_name, {'members', Members}),
	{reply, {text, ResponseMsg}, Req, State};


%% @private
%% @doc Receive member_count command from self 
websocket_info({'member_count'}, Req, State) ->
	XNestPid = State#state.xnest_pid,
	{ok, Count} = xnest:status(XNestPid, client_counts),
	ResponseMsg = make_response(self(), State#state.xnest_name, {'member_count', Count}),
	{reply, {text, ResponseMsg}, Req, State};

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
	Msg = <<"leave">>,
	xnest:input(XNestPid, {self(), text, {'leave', Msg}}),			%% Use xnest API to send leave message
	xnest:leave(XNestPid, self()),
	ok.


%%===================Internal functions===============

%% @doc Parse xnest Name from HTTP Req
-spec parse_xnest_name(req()) -> {binary(), req()}.
parse_xnest_name(Req) ->
	cowboy_req:binding(xnest_name, Req, <<"default">>).

%% @doc Parse client nickName from HTTP Req
-spec parse_nickname(req()) -> {binary(), req()}.
parse_nickname(Req) ->
	%%cowboy_req:qs_val(<<"nickname">>, Req, unicode:characters_to_binary("潜水员", unicode, utf8)).
	{NickName, _Req} = case cowboy_req:qs_val(<<"nickname">>, Req, <<>>) of
		{<<>>, Req1} ->
			cowboy_req:cookie(<<"nickname">>, Req1, nickname:rand());
		{Nick, Req1} -> 
			{Nick, Req1}
	end,
	% set nickname to cookie
	Req_ = cowboy_req:set_resp_cookie(<<"nickname">>, NickName, [], _Req),
	{NickName, Req_}.


%% @doc Join a xnest 
-spec join_xnest(binary(), binary()) -> {ok, pid()}.
join_xnest(XNestName, NickName) ->
	{ok, XNestPid} = xnest_manager:get_xnest(XNestName),   %% I can know what format will be return by xnest_manager
	{ok, _JoinResult} = xnest:join(XNestPid, self(), NickName),
	xnest:input(XNestPid, {self(), text, {'join', NickName}}),			%% Use xnest API to send join message
	{ok, XNestPid}.


%% @doc generate response 
-spec make_response(pid(), binary(), {binary(), binary()}) -> json().
make_response(FromPid, Xnest, {Type, Msg}) ->
	Time = time2binary(erlang:localtime()),
	From = pid2binary(FromPid),
	Frame = [ {<<"from">>, From}
		,{<<"xnest">>, Xnest}
		,{<<"type">>, Type}
		,{<<"payload">>, Msg}
		,{<<"send_time">>, Time}
	],
	Response = jsx:encode(Frame),
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

%% @doc pid to binary
-spec pid2binary(pid()) -> binary().
pid2binary(Pid) -> list_to_binary(pid_to_list(Pid)).


%% @doc change nickname 
-spec change_nickname(binary(), any()) -> ok.
change_nickname(NewName, State) -> 
	XNestPid = State#state.xnest_pid,
    {ok, _Result} = xnest:change_binding(XNestPid, self(), NewName),		%% use xnest API change_binding to change name 
	xnest:input(XNestPid, {self(), text, {'changename', NewName}}),			%% Use xnest API to send changename message
	ok.


%% @doc parse_msg
%% @doc you can do something here in particular 
-spec parse_msg(binary(), any()) -> any().
parse_msg(RawMessage, State) -> 
	Result = binary:split(RawMessage, <<":">>),
	%lager:info("Result of split : ~p", [Result]),
	case Result of 
		[<<"@changename">>, Name] ->
			change_nickname(Name, State);
		_ ->
			XNestPid = State#state.xnest_pid,
			xnest:input(XNestPid, {self(), text, {'normal', RawMessage}})		%% Use xnest API to send message
	end.







