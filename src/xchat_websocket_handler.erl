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
-export_type([req/0]).



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
    XNestPid ! {self(), {text, Msg}},
	{ok, Req, State};

%% @private
%% @doc Receive Message from client and send it to XNest, But unused
websocket_handle({binary, Bin}, Req, State) ->
	XNestPid = State#state.xnest_pid,
    XNestPid ! {self(), {binary, Bin}},
	{ok, Req, State};

%% @private
websocket_handle(_Data, Req, State) ->
	{ok, Req, State}.


%% @private
%% @doc Receive Message from xnest and send it to client
websocket_info({_FromPid, {text, Msg}}, Req, State) ->
	%ResponseMsg  = {{<<"from">>, FromPid}, {<<"msg">>, Msg}},
	ResponseMsg  = Msg,
	{reply, {text, ResponseMsg}, Req, State};

%% @private
%% @doc Receive timeout event , But now unused
websocket_info({timeout, _Ref, Msg}, Req, State) ->
	{reply, {text, Msg}, Req, State};

%% @private
websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

%% @private
websocket_terminate(_Reason, _Req, _State) ->
	ok.


%%===================Internal functions===============

%% @doc Parse xnest Name from HTTP Req
-spec parse_xnest_name(req()) -> {binary(), req()}.
parse_xnest_name(Req) ->
	cowboy_req:qs_val(<<"xnest">>, Req, <<>>).

%% @doc Join a xnest 
-spec join_xnest(binary()) -> {ok, pid()}.
join_xnest(_XNestName) ->
	%{ok, Pid} = xnest_manager:join_xnest(XNestName),   %% I can know what format will be return by xnest_manager
	%{ok, Pid}.
	{ok, self()}.


%% @doc A test function 
send_self(Pid) ->
	Pid ! {text, <<"Iam god!">>}.
