-module(xnest).
-behaviour(gen_server).

%% API.
-export([start_link/0, stop/1]).
-export([join/2, leave/2, input/2, status/1, status/2]).

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

%% gen_server.
init([]) ->
	{ok, #state{clients=[]}}.

%handle_call
handle_call({join, ClientPid}, _From, State) ->
    ClientPidStr = pid_to_list(ClientPid),
    Clients = State#state.clients,
    {JoinedResult, NewState} = case lists:member(ClientPid, Clients) of
        true -> 
            ResultBin = iolist_to_binary([ClientPidStr, " is already in the xnest!"]),
            {{ok, ResultBin}, State};
        false ->
            NewClients = [ClientPid | Clients],
            ResultBin = iolist_to_binary([ClientPidStr, " is successfully joined the xnest!"]),
            {{ok, ResultBin}, State#state{clients = NewClients}}
    end,
    {reply, JoinedResult, NewState};
handle_call({leave, ClientPid}, _From, State) ->
    ClientPidStr = pid_to_list(ClientPid),
    Clients = State#state.clients,
    {LeftResult, NewState} = case lists:member(ClientPid, Clients) of
        true ->
            NewClients = lists:delete(ClientPid, Clients),
            ResultBin = iolist_to_binary([ClientPidStr, " is successfully left the xnest!"]),
            {{ok, ResultBin}, State#state{clients = NewClients}};
        false ->
            ResultBin = iolist_to_binary([ClientPidStr, " is not in the xnest!"]),
            {{ok, ResultBin}, State}
    end,
    {reply, LeftResult, NewState};

handle_call({status, client_counts}, _From, State) ->
    ClientCOunts = length(State#state.clients),
	{reply, {ok, ClientCOunts}, State};
handle_call({status}, _From, State) ->
	{reply, {ok, State}, State};
    
handle_call({stop}, _From, State) ->
	{stop, normal, {ok, stopped}, State};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

%handle_cast
handle_cast({From, text, Message}, State) ->
    %%TBD, use lists:foldr for deploy message is not effective, we should think about another way.
    Clients = State#state.clients,
    Fun = fun(Client, AccIn) ->
        case is_process_alive(Client) of
            true ->
                Client ! {From, text, Message},
                [Client | AccIn];
            false ->
                %%TBD, here should inform manager client ws is dead
                AccIn
        end
    end,

    {_DeployMsgResult, NewClients} = try
        NewClients_ = lists:foldr(Fun, [], Clients),
        {{ok, <<"Message successfully deployed!">>}, NewClients_}
    catch _:_ ->
        {{error, <<"Error occured when deploy message!">>}, Clients}
    end,
	{noreply, State#state{clients = NewClients}};

handle_cast(_Msg, State) ->
	{noreply, State}.

%handle_info
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
