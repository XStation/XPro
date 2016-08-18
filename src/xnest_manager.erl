-module(xnest_manager).
-include("lager.hrl").
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([get_xnest/1, get_xnest/2]).
-export([get_xnest_count/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).


-define(TAB, ?MODULE).
-define(PID_INDEX, pid_to_name).

-record(state, {
}).

%%------------------------------------------------------------------------------------------------
%% API.
%%------------------------------------------------------------------------------------------------

%% @doc Start the xnest_manager gen_server.
-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Get the xnest by name, a new one will be created if the specfied is not exists.
-spec get_xnest(binary()) -> {ok, pid()}.
get_xnest(XNestName) ->
	get_xnest(XNestName, []).


%% @doc Get the xnest by name, a new one will be created if the specfied is not exists.
-spec get_xnest(binary(), list()) -> {ok, pid()}.
get_xnest(XNestName, Options) ->
	XNestPid = gen_server:call(?MODULE, {get_xnest, XNestName, Options}),
	{ok, XNestPid}.



%% @doc Get current xnest number.
-spec get_xnest_count() -> integer().
get_xnest_count() ->
	0.

%%------------------------------------------------------------------------------------------------
%% gen_server.
%%------------------------------------------------------------------------------------------------

%% @private
init([]) ->
	?TAB = ets:new(?TAB, [private, named_table]),
	?PID_INDEX = ets:new(?PID_INDEX, [private, named_table]),
	process_flag(trap_exit, true),
	{ok, #state{}}.

%% @private
handle_call({get_xnest, XNestName, Options}, _From,  State) ->
	XNest = case ets:lookup(?TAB, XNestName) of
		[{_XNestName, ExistXNest}|_] ->
			ExistXNest;
		_ ->
			{ok, NewXNest} = xnest:start_link(XNestName, Options),
	        {ok, RobotPid} = xnest_robot:start_link(),
			xnest_robot:add_robot_to_xnest(RobotPid, NewXNest),
			ets:insert_new(?TAB, {XNestName, NewXNest}),
			ets:insert_new(?PID_INDEX, {NewXNest, XNestName}),
			NewXNest	
	end,
	{reply, XNest, State};


handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

%% @private
handle_cast(_Msg, State) ->
	{noreply, State}.

%% @private
handle_info({'EXIT', FromPid, normal}, State) ->
	remove_xnest(FromPid),
        {noreply, State};
handle_info({'EXIT', FromPid, _Reason}, State) ->
	%% Temporarilly. Unnormal exit should be processed by 'ETS-TRANSFER'.
	remove_xnest(FromPid),
        {noreply, State};
handle_info({'ETS-TRANSFER', _Tid, FromPid, _HeirData}, State) ->
	remove_xnest(FromPid),
	%% Use tid start a new xnest.
	{noreply, State};
handle_info(_Info, State) ->
	{noreply, State}.

%% @private
terminate(_Reason, _State) ->
	ets:delete(?TAB),
	ets:delete(?PID_INDEX),
	ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%------------------------------------------------------------------------------------------------
%% Internal.
%%------------------------------------------------------------------------------------------------

remove_xnest(Pid) ->
	case ets:lookup(?PID_INDEX, Pid) of
		[{_XNest, XNestName}] ->
			ets:delete(?TAB, XNestName),
			ets:delete(?PID_INDEX, Pid);
		_ -> ok
	end.
