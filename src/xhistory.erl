-module(xhistory).
-include("lager.hrl").

-export([store/2, fetch/2]).


store(Xnest, Msg) ->
	%{{Y, M, D}, {H, I, S}} = erlang:localtime(),
	%Y_ = integer_to_binary(Y),
	%M_ = integer_to_binary(M),
	%D_ = integer_to_binary(D),
	%H_ = integer_to_binary(H),
	%I_ = integer_to_binary(I),
	%S_ = integer_to_binary(S),
	%Key = <<Y_/binary,"-",M_/binary,"-", D_/binary, " ", H_/binary, ":", I_/binary,":",S_/binary>>,
	%Value = <<Y_/binary,"-",M_/binary,"-", D_/binary, " ", H_/binary, ":", I_/binary,":",S_/binary,"||", UserName/binary,"||", Msg/binary>>,
	LastIndex= last_index(Xnest), 
%lager:info("last index is :~p", [LastIndex]),
	History = ?MODULE:fetch(Xnest, LastIndex),
	
	Len = length(History),
%lager:info("history length:~p", [Len]),
	if 
		Len > 50 -> update_index(Xnest, LastIndex);
		true -> nothing
	end,
		
	riak_worker:async_set(<<"default">>, Xnest, LastIndex, erlang:term_to_binary([Msg|History])).

fetch(Xnest, Cursor) when is_integer(Cursor) ->
	LastIndex = binary_to_integer(last_index(Xnest)),
	Tmp = LastIndex - Cursor,
	ReversCursor = case Tmp > 0 of
		true -> Tmp;
		_ -> 1
	end,
		
	fetch(Xnest, integer_to_binary(ReversCursor));

fetch(Xnest, Cursor)->
	case riak_worker:sync_get(<<"default">>, Xnest, Cursor) of 
		{error, notfound} -> [];
		Other -> erlang:binary_to_term(Other)
	end.


last_index(Xnest) ->
	Index = riak_worker:sync_get(<<"default">>, Xnest, <<"index">>),
	case Index of 
		{error, notfound} -> 
			riak_worker:async_set(<<"default">>, Xnest, <<"index">>, <<"1">>),
			<<"1">>;
		_ -> Index
	end.

update_index(Xnest, LastIndex) ->
	Value = integer_to_binary(binary_to_integer(LastIndex)+1),
	riak_worker:async_set(<<"default">>, Xnest, <<"index">>, Value).
