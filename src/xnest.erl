-module(xnest).
-behaviour(gen_server).

%% API.
-export([start_link/1, stop/1]).
-export([join/2, join/3, leave/2, change_binding/3, input/2, status/1, status/2, members/1, history/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
    xnest_name,
    history,
    clients,
    idel_time
}).
-record(client_info, {
    pid,
    bindings
}).

-include("lager.hrl").

-define(TTL, 5000).
-define(IDEL_TIMEOUT, 600000).
-define(HISTORY_LEN, 10).

%% API.

%% @doc Start the xnest gen_server.
-spec start_link(binary()) -> {ok, pid()}.
start_link(XnestName) ->
	gen_server:start_link(?MODULE, [XnestName], []).


%% @doc Start the xnest gen_server.
-spec stop(pid()) -> {ok, stopped}.
stop(XNestPid) ->
	gen_server:call(XNestPid, {stop}).


%% @doc Join a xnest.
-spec join(pid(), pid()) -> {ok, binary()} | {error, binary()}.
join(XNestPid, ClientPid) ->
    join(XNestPid, ClientPid, <<"">>).

-spec join(pid(), pid(), term()) -> {ok, binary()} | {error, binary()}.
join(XNestPid, ClientPid, Bindings) ->
    gen_server:call(XNestPid, {join, ClientPid, Bindings}).


%% @doc Leave a xnest.
-spec leave(pid(), pid()) -> {ok, binary()} | {error, binary()}.
leave(XNestPid, ClientPid) ->
    gen_server:call(XNestPid, {leave, ClientPid}).


%% @doc change bindings.
-spec change_binding(pid(), pid(), term()) -> {ok, binary()} | {error, binary()}.
change_binding(XNestPid, ClientPid, Bindings) ->
    gen_server:call(XNestPid, {'change_binding', ClientPid, Bindings}).



%% @doc Leave a xnest.
-spec input(pid(), {pid(), atom(), any()}) -> {ok, binary()} | {error, binary()}.
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
history(XNestPid, Cursor) ->
    %%可能直接调用hisotry模块的函数
    %%FadeHistory = [[{<<"from">>, <<"<0.1.0">>}, {<<"payload">>, <<"我来了">>}, {<<"send_time">>, <<"2014-12-27">>}] 
    %%    	  ,[{<<"from">>, <<"<0.1.0">>}, {<<"payload">>, <<"我来了">>}, {<<"send_time">>, <<"2014-12-27">>}]
    %%    	  ],
    %%{ok, FadeHistory}.
    gen_server:call(XNestPid, {history, Cursor}).


%% gen_server.
init([XnestName]) ->
    History = [],
    Clients = ets:new(xnest, [set, public]),
    erlang:send_after(?TTL, self(), {tick}),
    {ok, #state{xnest_name=XnestName, history=History, clients=Clients, idel_time=0}}.

%handle_call
handle_call({join, ClientPid}, _From, State) ->
    Clients = State#state.clients,

    NewClient = {ClientPid, #client_info{pid = ClientPid}},
    %ClientPidStr = pid_to_list(ClientPid),

    %%ets:insert return only true, consider to add try/catch
    ets:insert(Clients, NewClient),
    %JoinedResult = {ok, iolist_to_binary([ClientPidStr, " is successfully joined the xnest!"])},
    JoinedResult = {ok, <<" is successfully joined the xnest!">>},

    {reply, JoinedResult, State};

handle_call({join, ClientPid, Bindings}, _From, State) ->
    Clients = State#state.clients,
    NewClient = {ClientPid, #client_info{pid = ClientPid, bindings = Bindings}},
    %ClientPidStr = pid_to_list(ClientPid),
    %%ets:insert return only true, consider to add try/catch
    ets:insert(Clients, NewClient),
    %JoinedResult = {ok, iolist_to_binary([ClientPidStr, " is successfully joined the xnest!"])},
    JoinedResult = {ok, <<" is successfully joined the xnest!">>},
    {reply, JoinedResult, State};


handle_call({'change_binding', ClientPid, Bindings}, _From, State) ->
    Clients = State#state.clients,
    NewClient = {ClientPid, #client_info{pid = ClientPid, bindings = Bindings}},
    %ClientPidStr = pid_to_list(ClientPid),
    %%ets:insert return only true, consider to add try/catch
    ets:insert(Clients, NewClient),			%% insert a binding will be replaced, because ets table's type  is "set" 
    %JoinedResult = {ok, iolist_to_binary([ClientPidStr, " is successfully joined the xnest!"])},
    Result = {ok, <<" change binding successfully!">>},
    {reply, Result, State};


handle_call({history, Cursor}, _From, State) ->
    %History = State#state.history, % no need to reverse
	History = try
		xhistory:fetch(State#state.xnest_name, Cursor)
	catch _:_ ->
		lager:error("fetch history from riak error, xnestname ~p", [State#state.xnest_name]),
		[]
	end,
    {reply, {ok, lists:reverse(History)}, State};

handle_call({leave, ClientPid}, _From, State) ->
    Clients = State#state.clients,
    %ClientPidStr = pid_to_list(ClientPid),

    ets:delete(Clients, ClientPid),
    %LeftResult = {ok, iolist_to_binary([ClientPidStr, " is successfully left the xnest!"])},
    LeftResult = {ok, <<" successfully left the xnest!">>},

    {reply, LeftResult, State};

handle_call({status, client_counts}, _From, State) ->
    ClientCounts = ets:info(State#state.clients, size),
	{reply, {ok, ClientCounts}, State};
handle_call({status}, _From, State) ->
	{reply, {ok, State}, State};
handle_call({members}, _From, State) ->
    ClientsEts = State#state.clients,
    Clients = ets:tab2list(ClientsEts),
    Members = [ {ClientInfo#client_info.pid, ClientInfo#client_info.bindings} || {_ClientPid, ClientInfo} <- Clients],
	%Members = [[{<<"pid">>, <<"pidvalue">>}, {<<"name">>,<<"namevalue">>}], [{<<"pid">>,<<"pid3">>}, {<<"name">>,<<"username2">>}]], %%needtodo: get real data
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

%%=========================
        %%TBD, here change sync to asyn simply by spawn, consider it again.
        %_DeadClients = ets:foldr(DeployFun, [], Clients),
        spawn(ets, foldl, [DeployFun, [], Clients]),
    	{_DeployMsgResult, NewState} = {{ok, <<"Message successfully deployed!">>}, State},
%=========================
%%%=========================
%    {_DeployMsgResult, NewState} = try
%        %%TBD, here change sync to asyn simply by spawn, consider it again.
%        %_DeadClients = ets:foldr(DeployFun, [], Clients),
%        spawn(ets, foldl, [DeployFun, [], Clients]),
%        {{ok, <<"Message successfully deployed!">>}, State}
%    catch _:_ ->
%        {{error, <<"Error occured when deploy message!">>}, State}
%    end,
%%=========================

%%==========临时存放的记录, 以后移植到riak中去===========
    History = State#state.history,
    NewHistory = case Message of
		%only put in the message body, exclude Type
        {'normal', Msg} ->
            SubHistory = case length(History) =:= ?HISTORY_LEN of
                true ->
                    [ _H|T ] = History,
                    T;
                false ->
                    History
            end,
            {Y, M, D} = date(),
            Date = list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B", [Y, M, D])),
			TermMsg = [{<<"from">>, list_to_binary(pid_to_list(From))}, {<<"payload">>, Msg}, {<<"send_time">>, Date}],
			%%****临时插入一行代码, 写入记录到riak中************
			try
				xhistory:store(State#state.xnest_name, TermMsg)
			catch _:_ ->
				lager:error("store history to riak error!!!")
			end,
			%%**************************************************
            lists:append(SubHistory, [TermMsg]);
        _ ->
            History
    end,
%%=================================

    {noreply, NewState#state{history=NewHistory}};

handle_cast(_Msg, State) ->
	{noreply, State}.

%handle_info
handle_info({tick}, State) ->
    ClientCounts = ets:info(State#state.clients, size),
    IdelTimeout = State#state.idel_time,
    NewIdelTimeout = case ClientCounts of
        1 -> IdelTimeout + ?TTL;
        _ -> 0
    end,

    case NewIdelTimeout > ?IDEL_TIMEOUT of
        true -> 
            {stop, normal, State};
        false -> 
            erlang:send_after(?TTL, self(), {tick}),
            {noreply, State#state{idel_time=NewIdelTimeout}}
    end;

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
