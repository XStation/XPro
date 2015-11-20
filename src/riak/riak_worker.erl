-include("../lager.hrl").
-module(riak_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([sync_set/4, sync_get/3, async_set/4]).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {conn}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Args) ->
    process_flag(trap_exit, true),
    Hostname = proplists:get_value(hostname, Args),
    Port     = proplists:get_value(port, Args),
    {ok, Conn} = riakc_pb_socket:start_link(Hostname, Port),
    {ok, #state{conn=Conn}}.

handle_call({set, Object}, _From, #state{conn=Conn}=State) ->
    %T1 = os:timestamp(),
    Ret = riakc_pb_socket:put(Conn, Object),
    %T2 = os:timestamp(),
    %Diff = timer:now_diff(T1, T2),
    %io:format("(~p)", [Ret]),
    %io:format("."),
    {reply, Ret, State};

handle_call({get, {Type, Bucket, Key}}, _From, #state{conn=Conn}=State) ->
	Result = case riakc_pb_socket:get(Conn, {Type, Bucket}, Key) of 
    	{ok, Obj} -> 
    		Value = riakc_obj:get_value(Obj),
			Value;
		_Other -> 
			_Other
	end,
    %lager:info("value from bucket ~p, key ~p", [ Bucket, Key]),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, nomatchfunc, State}.


handle_cast({set, Object}, #state{conn=Conn}=State) ->
    ok = riakc_pb_socket:put(Conn, Object),
    poolboy:checkin(local, self()),
    {noreply, State};

handle_cast({get, {Bucket, Key}}, #state{conn=Conn}=State) ->
    {ok, Obj} = riakc_pb_socket:get(Conn, Bucket, Key),
    riakc_obj:get_value(Obj),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(Fun, #state{conn=Conn}=State) ->
    Fun(Conn),
    {noreply, State};

handle_info(_Info, State) ->
lager:warning("receive something ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, #state{conn=_Conn}) ->
    %gen_tcp:close(Conn),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%=======api function ===========
sync_set(Type, Bucket, Key, Value)->
    RiakObject = riakc_obj:new({Type, Bucket}, Key, Value),
    Pid = poolboy:checkout(local),
    Ret = gen_server:call(Pid, {set, RiakObject}),
    poolboy:checkin(local, Pid),
    Ret.

async_set(Type, Bucket, Key, Value)->
    RiakObject = riakc_obj:new({Type, Bucket}, Key, Value),
    Pid = poolboy:checkout(local),
    gen_server:cast(Pid, {set, RiakObject}).




sync_get(Type, Bucket, Key)->
    Pid = poolboy:checkout(local),
    Value = gen_server:call(Pid, {get, {Type, Bucket, Key}}),
    poolboy:checkin(local, Pid),
    Value.

