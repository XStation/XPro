-module(dispatch).
-export([module/1]).


%% @doc 配置可访问的method
-spec module(binary()) -> atom().

module(<<"members">>) ->
	members;
module(<<"member_count">>) ->
	member_count;
module(<<"history">>) ->
	history;
module(_) ->
	<<"404">>.
