%%%-------------------------------------------------------------------
%%% @author savik
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jan 2019 21:22
%%%-------------------------------------------------------------------
-module(mnesia_driver).
-author("savik").

%% API
-export([
  start/0,
  find_user/1,
  add_user/2,
  select_for_period/1,
  select_last/0,
  counter/0,
  remove_fields/1
  ]).

-record(token_bucket, {user_id, insert_at}).

-spec(start() -> ok).
start() ->
  io:format("~nStarting mnesia~n"),
  mnesia:start(),
  io:format("~nCreating table token_bucket~n"),
  mnesia:create_table(token_bucket, [{attributes, record_info(fields, token_bucket)}]),
  mnesia:wait_for_tables([token_bucket], 5000),
  mnesia:info().

-spec(find_user(UserId::term()) -> tuple()).
find_user(UserId) ->
  Query =
    fun() ->
      mnesia:match_object({token_bucket, UserId, '_'})
    end,
  mnesia:transaction(Query).

-spec(add_user(UserId::term(), InsertTime::integer() ) -> tuple()).
add_user(UserId, InsertTime) ->
  Row = #token_bucket{user_id=UserId, insert_at=InsertTime},
  F = fun() ->
    mnesia:write(Row)
      end,
  mnesia:transaction(F).

-spec(select_for_period(Period::integer() ) -> tuple()).
select_for_period(Period) ->
  MatchHead = #token_bucket{user_id='$1', insert_at='$2'},
  Guard = {'>', '$2', Period},
  Result = {user_id, insert_at},
  mnesia:select(token_bucket,[{MatchHead, [Guard], [Result]}]).

-spec(counter() -> integer()).
counter() ->
  Query =
    fun() ->
      mnesia:match_object({token_bucket, '_', '_'})
    end,
  {atomic, Results} = mnesia:transaction(Query),
  erlang:length(Results).

-spec(select_last() -> tuple()).
select_last() ->
  F = fun() ->
    mnesia:last(token_bucket)
      end,
  case mnesia:transaction(F) of
    {atomic,'$end_of_table'} -> {ok, empty};
    {atomic, UserId} ->
      {atomic, Res} = find_user(UserId),
      LastInserts = lists:map(fun({_, _, LastInsert}) -> LastInsert end, Res),
      {ok, lists:max(LastInserts)};
    Other -> {other, Other}
  end .

-spec(remove_fields(Period::integer()) -> ok).
remove_fields(Period) ->
  MatchHead = #token_bucket{user_id='$1', insert_at='$2'},
  Guard = {'>', '$2', Period},
  Query = fun() -> mnesia:select(token_bucket, [{MatchHead,[Guard],['$_']}]) end,
  case mnesia:transaction(Query) of
    {atomic, Results} ->
      F = fun() -> lists:foreach(fun(x) -> mnesia:delete({token_bucket, x}) end, Results) end,
      mnesia:transaction(F);
    Error ->
      io:format("~nremoved fields error: ~n~p~n", [Error])
  end.


