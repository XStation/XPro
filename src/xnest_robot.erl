-module(xnest_robot).
-include("lager.hrl").
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([add_robot_to_xnest/2]).
-export([get_answer/2]).

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
    NickName = unicode:characters_to_binary("小M机器人"),
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
        Answer = get_answer(Question, _From),
	    lager:info("question:~ts, answer:~ts", [Question, Answer]),
        xnest:input(State#state.xnest, {self(), text, {normal, Answer}});
     <<"@", Question/binary>> ->
        Answer = get_answer(Question, _From),
	    lager:info("question:~ts, answer:~ts", [Question, Answer]),
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
get_answer(Question_, SessionId) ->
lager:info("~p", [SessionId]),
	Session = case is_pid(SessionId) of
		true -> list_to_binary(pid_to_list(SessionId));
		_ -> SessionId
	end,
    Question = trim(Question_),
    Url = "https://xiaodu.baidu.com/ws",
	_Qs = <<"{
\"sample_name\": \"bear_brain_wireless\",
\"isNewUser\": \"0\",
\"request_type\": \"20\",
\"longitude\": \"1.268496257425E7\",
\"request_uid\": \"E60A0298D473929BA4538CBD40471C95\",
\"app_ver\": \"2.0.0\",
\"service_id\": \"5\",
\"query_type\": \"0\",
\"from_client\": \"sdk\",
\"request_query\": \"">>,
	Q = Question,
	Qs_ = <<"\",
\"cuid\": \"", Session/binary,"\",
\"operation_system\": \"android\",
\"latitude\": \"2558831.054261\",
\"request_from\": \"0\",
\"appid\": \"650DEBC2B99A4dA4\",
\"appkey\": \"2F4B662AF2064323A16122D702160F15\",
\"sdk_ui\": \"yes\",
\"channel_from\":\"\"}">>,
	Qs = <<_Qs/binary, Q/binary, Qs_/binary>>,


    Answer = case httpc:request(post, {Url, [], "application/json", Qs}, [{ssl, [{fail_if_no_peer_cert, false}]}], []) of
        {ok, {{_Version, 200, _Phrase}, _Headers, Body}} ->
            DecodedBody = jsx:decode(list_to_binary(Body)),
	    	Code = proplists:get_value(<<"status">>, DecodedBody),
	    	case Code of
	        	0 ->
		    		parse_view(DecodedBody);
				_ ->
		    		lager:info("~p", [DecodedBody]),
	    			<<" Sorry, I Don't Understand! ">>
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

parse_view(ResponseBody) ->
	Result = proplists:get_value(<<"result">>, ResponseBody),
	Views = proplists:get_value(<<"views">>, Result),

	GetThumb = fun(Y) ->
		Thumb = proplists:get_value(<<"thumb">>, Y),
		<<"<img src='", Thumb/binary ,"'/>">>
	end,

	GetList = fun(Y) ->
		Title = proplists:get_value(<<"title">>, Y),
		Summary = proplists:get_value(<<"summary">>, Y),
		Url = proplists:get_value(<<"url">>, Y),
		Detail = unicode:characters_to_binary("详情", unicode, utf8),
		A = <<"<a href='", Url/binary, "'>", Detail/binary, "</a>">>,
		<<Title/binary, "</br>", Summary/binary, "</br>", A/binary>>
	end,

	GetContent = fun(X) ->
		Type = proplists:get_value(<<"type">>, X),
		case Type of 
			<<"txt">> -> proplists:get_value(<<"content">>, X);
			<<"image">> -> 
				List = proplists:get_value(<<"list">>, X),
				[ GetThumb(Item) || Item <- List];
			<<"list">> -> 
				List = proplists:get_value(<<"list">>, X),
				[ GetList(Item) || Item <- List]
		end
				
	end,
	Content = lists:flatten([GetContent(View) || View <- Views]),
	Concat = fun(Elem, AccIn) ->
		<<Elem/binary, "</br></br>", AccIn/binary>>
	end,
	lists:foldl(Concat, <<>>, Content).
	
