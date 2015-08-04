-module(xnest_robot).
-include("lager.hrl").
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([add_robot_to_xnest/2]).
-export([get_answer/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
    xnest
}).

%%------------------------------------------------------------------------------------------------
%% API.
%%------------------------------------------------------------------------------------------------

%% @doc Start the xnest_manager gen_server.
-spec start_link() -> {ok, pid()}.
start_link() -> gen_server:start_link(?MODULE, [], []).

%% @doc Add robot to specified xnest.
-spec add_robot_to_xnest(pid(), pid()) -> ok. 
add_robot_to_xnest(RobotPid, XNestPid) ->
    gen_server:call(RobotPid, {add_robot_to_xnest, XNestPid}).

%%------------------------------------------------------------------------------------------------
%% gen_server.
%%------------------------------------------------------------------------------------------------

%% @private
init([]) ->
    {ok, #state{}}.

%% @private
handle_call({add_robot_to_xnest, XNestPid}, _From, State) ->
    NickName = "robot",
    {ok, _JoinResult} = xnest:join(XNestPid, self(), NickName),
    {reply, ok, State#state{xnest=XNestPid}};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info({_From, text, {normal, Msg}}, State) ->
    case Msg of
        <<"@robot", Question/binary>> ->
            Answer = get_answer(Question),
	    lager:info("question:~p, answer:~p", [Question, Answer]),
            xnest:input(State#state.xnest, {self(), text, {normal, Answer}});
	_ ->
	    noreply
    end,
    {noreply, State};
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
get_answer(Question_) ->
    Question = trim(Question_),
    Url = "http://apis.baidu.com/turing/turing/turing",
    Qs = "?key=ae177163c8d16dc28787f8e8badd6e7c&info=" ++ binary_to_list(Question),
    Answer = case httpc:request(get, {Url++Qs, [{"apikey", "864770ce98248d2562b8fecd73e69ef3"}]}, [], []) of
        {ok, {{_Version, 200, _Phrase}, _Headers, Body}} ->
            DecodedBody = jsx:decode(list_to_binary(Body)),
	    Code = proplists:get_value(<<"code">>, DecodedBody),
	    case Code of
	        100000 ->
		    proplists:get_value(<<"text">>, DecodedBody);
		_ ->
		    DecodedBody
            end;
	_ ->
	    <<"I am dead.">>
    end,
    Answer.


trim(Bin) when is_binary(Bin) -> 
    << << X >> || <<X>> <= Bin, not is_whitespace(X) >>; 
trim(Str) when is_list(Str) -> 
    [X || X <- Str, not is_whitespace(X)]. 

is_whitespace($\s)-> true; 
is_whitespace($\t)-> true; 
is_whitespace($\n)-> true; 
is_whitespace($\r)-> true; 
is_whitespace(_Else) -> false. 
