-module(nickname).
-include("lager.hrl").
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([rand/0, reload/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
	gbtree,
	length = 0
}).

%% API.
rand() ->
	gen_server:call(?MODULE, rand).
reload() ->
	?MODULE ! load_name.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server.

init([]) ->
	self() ! load_name,
	{ok, []}.

handle_call(rand, _From, State) ->
	Tree = State#state.gbtree,
	Length = State#state.length,
    {_, _, T} = now(),
    Key = (T rem Length)+1,
	Name = gb_trees:get(Key, Tree),
    Name_ = unicode:characters_to_binary(Name, unicode, utf8),
	{reply, Name_, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.


handle_info(load_name, _State) ->
	{Length, Tree} = loadName(),
	{noreply, #state{gbtree=Tree, length=Length}};

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%%=============internal functions=====================

loadName() ->
	FilePath = "/opt/apps/xpro/priv/nicknames",
lager:info("loadname...."),
	{ok, Contents} = file:read_file(FilePath),
	Names = binary:split(Contents, <<"\n">>, [global]),
	_Tree = gb_trees:empty(),
	{Length, Tree} = loop_insert(1, Names, _Tree),
	{Length, Tree}.

loop_insert(Key, [], Tree) -> 
	Length = Key -1,
	{Length, Tree};

loop_insert(Key, [Value|Names], Tree)->
	Tree1 = gb_trees:enter(Key, Value, Tree),
	loop_insert(Key+1, Names, Tree1).

