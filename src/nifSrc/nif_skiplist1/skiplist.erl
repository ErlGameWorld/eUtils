%%%-------------------------------------------------------------------
%%% @author fox
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%% indexable skip list, can insert duplicated value
%%% @end
%%% Created : 16. 十月 2017 14:16
%%%-------------------------------------------------------------------
-module(skiplist).
-author("fox").

%% API
-export([
   init/1,
   new/0, free/1,
   insert/3, delete/3, update/4,
   to_list/1,
   range/3, range_with_score/3, range_by_score/3,
   index_of_score/2, at/2,
   size/1]).

-type skiplist() :: non_neg_integer().
-type score() :: integer().
-type value() :: integer().

-spec init(Path) -> ok | Error when
   Path :: string(),
   Error :: any().
init(Path) ->
   erlang:load_nif(Path, 0).

-spec new() -> skiplist().
new() ->
   erlang:nif_error(undef).

-spec free(skiplist()) -> ok.
free(_List) ->
   erlang:nif_error(undef).

-spec insert(skiplist(), score(), value()) -> 0.
insert(_List, _Score, _Value) ->
   erlang:nif_error(undef).

-spec delete(skiplist(), score(), value()) ->
   0 | 1. %% 0 success, 1 fail
delete(_List, _Score, _Value) ->
   erlang:nif_error(undef).

-spec update(skiplist(), score(), value(), score()) -> 0.
update(_List, _Score, _Value, _OldScore) ->
   erlang:nif_error(undef).

-spec to_list(skiplist()) -> [{score(), value()}].
to_list(_List) ->
   erlang:nif_error(undef).

-spec size(skiplist()) -> non_neg_integer().
size(_List) ->
   erlang:nif_error(undef).

-spec index_of_score(skiplist(), score()) -> non_neg_integer().
index_of_score(_List, _Score) ->
   erlang:nif_error(undef).

-spec at(skiplist(), non_neg_integer()) -> error | {score(), value()}.
at(_List, _Index) ->
   erlang:nif_error(undef).

-spec range(skiplist(), non_neg_integer(), non_neg_integer()) -> [value()].
range(_List, _Start, _Len) ->
   erlang:nif_error(undef).

-spec range_with_score(skiplist(), non_neg_integer(), non_neg_integer()) ->
   [{score(), value()}].
range_with_score(_List, _Start, _Len) ->
   erlang:nif_error(undef).

-spec range_by_score(skiplist(), score(), score()) ->
   {StartIndex :: non_neg_integer(), [{score(), value()}]}.
range_by_score(_List, _Score1, _Score2) ->
   erlang:nif_error(undef).
