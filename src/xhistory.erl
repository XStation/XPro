-module(xhistory).
-include("lager.hrl").

-export([store/3, fetch/2]).


store(Xnest, UserName, Msg) ->
	{{Y, M, D}, {H, I, S}} = erlang:localtime(),
	Y_ = integer_to_binary(Y),
	M_ = integer_to_binary(M),
	D_ = integer_to_binary(D),
	H_ = integer_to_binary(H),
	I_ = integer_to_binary(I),
	S_ = integer_to_binary(S),
	Key = <<Y_/binary,"-",M_/binary,"-", D_/binary, " ", H_/binary, ":", I_/binary,":",S_/binary>>,
	Value = <<Y_/binary,"-",M_/binary,"-", D_/binary, " ", H_/binary, ":", I_/binary,":",S_/binary,"||", UserName/binary,"||", Msg/binary>>,
	riakc_worker:async_set(<<"default">>, Xnest, Key, Value).


fetch(Xnest, <<>>) ->
	fetch(Xnest, erlang:localtime());

fetch(Xnest, DateTime) ->
	{{Y, M, D}, {H, _, _}} = DateTime,
	Y_ = integer_to_binary(Y),
	M_ = integer_to_binary(M),
	D_ = integer_to_binary(D),
	H_ = integer_to_binary(H),
	Key = <<Y_/binary,"-",M_/binary,"-", D_/binary, " ", H_/binary>>,
	riakc_worker:sync_get(<<"default">>, Xnest, Key).
