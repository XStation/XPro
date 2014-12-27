-module(xnest).
-behaviour(gen_server).

%% API.
-export([start_link/0, stop/1]).
-export([join/2, leave/2, input/2, status/1, status/2, members/1, history/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
    clients
}).
-record(client_info, {
    pid
}).

-include("lager.hrl").

%% API.

%% @doc Start the xnest gen_server.
-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link(?MODULE, [], []).
%% @doc Start the xnest gen_server.
-spec stop(pid()) -> {ok, stopped}.
stop(XNestPid) ->
	gen_server:call(XNestPid, {stop}).

%% @doc Join a xnest.
-spec join(pid(), pid()) -> {ok, binary()} | {error, binary()}.
join(XNestPid, ClientPid) ->
    gen_server:call(XNestPid, {join, ClientPid}).

%% @doc Leave a xnest.
-spec leave(pid(), pid()) -> {ok, binary()} | {error, binary()}.
leave(XNestPid, ClientPid) ->
    gen_server:call(XNestPid, {leave, ClientPid}).

%% @doc Leave a xnest.
-spec input(pid(), {pid(), atom(), binary()}) -> {ok, binary()} | {error, binary()}.
input(XNestPid, {From, text, Message}) ->
    gen_server:cast(XNestPid, {From, text, Message}).

%% @doc Get the number of clients in a xnest.
-spec status(pid(), atom()) -> {ok, binary()} | {error, binary()}.
status(XNestPid, client_counts) ->
    gen_server:call(XNestPid, {status, client_counts}).

%% @doc Get the total status of a xnest.
-spec status(pid())-> {ok, binary()} | {error, binary()}.
status(XNestPid) ->
    gen_server:call(XNestPid, {status}).

%% @doc Get members of a xnest.
-spec members(pid())-> {ok, list()} | {error, binary()}.
members(XNestPid) ->
    gen_server:call(XNestPid, {members}).

%% @doc Get history of a xnest.
-spec history(pid(), number())-> {ok, list()} | {error, binary()}.
history(_XNestPid, _Count) ->
    %%可能直接调用hisotry模块的函数
    FadeHistory = [[{<<"from">>, <<"<0.1.0">>}, {<<"payload">>, <<"我来了">>}, {<<"send_time">>, <<"2014-12-27">>}] 
		  ,[{<<"from">>, <<"<0.1.0">>}, {<<"payload">>, <<"我来了">>}, {<<"send_time">>, <<"2014-12-27">>}]
		  ],
    %%gen_server:call(XNestPid, {history, Count}).
lager:error("~p", [FadeHistory]),
    {ok, FadeHistory}.





%% gen_server.
init([]) ->
    Clients = ets:new(xnest, [set]),
	{ok, #state{clients=Clients}}.

%handle_call
handle_call({join, ClientPid}, _From, State) ->
    Clients = State#state.clients,

    NewClient = {ClientPid, #client_info{pid = ClientPid}},
    ClientPidStr = pid_to_list(ClientPid),

    %%ets:insert return only true, consider to add try/catch
    ets:insert(Clients, NewClient),
    JoinedResult = {ok, iolist_to_binary([ClientPidStr, " is successfully joined the xnest!"])},

    {reply, JoinedResult, State};

handle_call({leave, ClientPid}, _From, State) ->
    Clients = State#state.clients,
    ClientPidStr = pid_to_list(ClientPid),

    ets:delete(Clients, ClientPid),
    LeftResult = {ok, iolist_to_binary([ClientPidStr, " is successfully left the xnest!"])},

    {reply, LeftResult, State};

handle_call({status, client_counts}, _From, State) ->
    ClientCOunts = ets:info(State#state.clients, size),
	{reply, {ok, ClientCOunts}, State};
handle_call({status}, _From, State) ->
	{reply, {ok, State}, State};
handle_call({members}, _From, State) ->
	Members = [[{<<"pid">>, <<"pidvalue">>}, {<<"name">>,<<"namevalue">>}], [{<<"pid">>,<<"pid2">>}, {<<"name">>,<<"username2">>}]], %%needtodo: get real data
	{reply, {ok, Members}, State};
    
    
handle_call({stop}, _From, State) ->
	{stop, normal, {ok, stopped}, State};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

%handle_cast
handle_cast({From, text, Message}, State) ->
    %%TBD, use lists:foldr for deploy message is not effective, we should think about another way.
    Clients = State#state.clients,
    DeployFun = fun({Client, _ClientInfo}, AccIn) ->
        case is_process_alive(Client) of
            true ->
                Client ! {From, text, Message},
                AccIn;
            false ->
                %%TBD, here should inform manager client ws is dead
                ets:delete(Clients, Client),
                [Client|AccIn]
        end
    end,

    {_DeployMsgResult, NewState} = try
        %%TBD, here change sync to asyn simply by spawn, consider it again.
        %_DeadClients = ets:foldr(DeployFun, [], Clients),
        spawn(ets, foldr, [DeployFun, [], Clients]),
        {{ok, <<"Message successfully deployed!">>}, State}
    catch _:_ ->
        {{error, <<"Error occured when deploy message!">>}, State}
    end,
	{noreply, NewState};

handle_cast(_Msg, State) ->
	{noreply, State}.

%handle_info
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
