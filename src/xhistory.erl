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
lager:warning("last index is ~p", [LastIndex]),
	RawHistory = fetch(Xnest, LastIndex),
	History = erlang:binary_to_term(RawHistory),
	lager:warning("raw history ~p and to binary ~p", [RawHistory, History]),
	Len = length(History),
	lager:warning("history len ~p", [Len]),
	if 
		Len > 50 -> update_index(Xnest, LastIndex);
		true -> nothing
	end,
		
	riak_worker:async_set(<<"default">>, Xnest, LastIndex, erlang:term_to_binary([Msg|History])).


fetch(Xnest, Cursor) ->
	case riak_worker:sync_get(<<"default">>, Xnest, Cursor) of 
		{error, notfound} -> [<<>>];
		_Other -> _Other
	end.


last_index(Xnest) ->
	lager:warning("1111"),
	Index = riak_worker:sync_get(<<"default">>, Xnest, <<"index">>),
	case Index of 
		{error, notfound} -> 
			riak_worker:async_set(<<"default">>, Xnest, <<"index">>, <<"1">>),
			<<"1">>;
		_ -> Index
	end.

update_index(Xnest, LastIndex) ->
lager:warning("update index"),
	Value = integer_to_binary(binary_to_integer(LastIndex)+1),
	riak_worker:async_set(<<"default">>, Xnest, <<"index">>, Value).
