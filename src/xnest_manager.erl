-module(xnest_manager).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([join_xnest/1]).
-export([get_xnest_count/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).


-define(TAB, ?MODULE).

-type monitors() :: [{reference(), binary()}].
-record(state, {
	monitors = [] :: monitors()
}).

%%------------------------------------------------------------------------------------------------
%% API.
%%------------------------------------------------------------------------------------------------

%% @doc Start the xnest_manager gen_server.
-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link(?MODULE, [], []).

%% @doc Join the xnest, a new one will be created if the specfied xnest is not exists.
-spec join_xnest(binary()) -> {ok, pid()}.
join_xnest(XNestName) ->
	gen_server:call(?MODULE, {join_xnest, XNestName}).

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
	{ok, #state{}}.

%% @private
handle_call({join_xnext, XNestName}, _From,  State=#state{monitors=Monitors}) ->
	{XNest, NewMonitors} = case ets:lookup(?TAB, XNestName) of
		[{XNestName, ExistXNest}|_] ->
			%[TODO] join exist xnest here
			{ExistXNest, Monitors};
		_ ->
			%[TODO] create and join new xnest here
			NewXNest = "new xnest pid",
			MonitorRef = erlang:monitor(process, NewXNest),
			{NewXNest, [{MonitorRef, XNestName} | Monitors]}	
	end,
	{reply, XNest, State#state{monitors=NewMonitors}};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

%% @private
handle_cast(_Msg, State) ->
	{noreply, State}.

%% @private
handle_info({'DOWN', MonitorRef, process, _Pid, _}, State=#state{monitors=Monitors}) ->
	{_, XNestName} = lists:keyfind(MonitorRef, 1, Monitors),
	true = ets:delete(?TAB, XNestName),
	NewMonitors = lists:keydelete(MonitorRef, 1, Monitors),
	{noreply, State#state{monitors=NewMonitors}};
handle_info(_Info, State) ->
	{noreply, State}.

%% @private
terminate(_Reason, _State) ->
	ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%------------------------------------------------------------------------------------------------
%% Internal.
%%------------------------------------------------------------------------------------------------

% Internal functions should be here.

