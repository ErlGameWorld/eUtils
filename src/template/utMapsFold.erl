-module(utMapsFold).

-export([fold/1, diff/2, diffKeys/2]).

fold(Map) ->
   fold(maps:iterator(Map), []).

fold(Iterator, AccList) ->
   case maps:next(Iterator) of
      {Key, Value, NextIter} ->
         fold(NextIter, [{Key, Value} | AccList]);
      none ->
         AccList
   end.


diff(Map1, Map2) ->
   case map_size(Map1) =< map_size(Map2) of
      true ->
         diff_1(maps:next(maps:iterator(Map1)), Map1, Map2);
      _ ->
         diff_2(maps:next(maps:iterator(Map2)), Map1)
   end.

diff_1(none, Map1, _Map2) ->
   Map1;
diff_1({Key, _Value, NextIter}, Map1, Map2) ->
   case maps:is_key(Key, Map2) of
      true ->
         diff_1(maps:next(NextIter), maps:remove(Key, Map1), Map2);
      _ ->
         diff_1(maps:next(NextIter), Map1, Map2)
   end.

diff_2(none, Map1) ->
   Map1;
diff_2({Key, _Value, NextIter}, Map1) ->
   diff_2(maps:next(NextIter), maps:remove(Key, Map1)).

diffKeys(Map1, Map2) ->
   case map_size(Map1) =< map_size(Map2) of
      true ->
         diffKeys_1(maps:next(maps:iterator(Map1)), Map2, []);
      _ ->
         diffKeys_2(maps:next(maps:iterator(Map2)), Map1)
   end.

diffKeys_1(none, _Map2, Acc) ->
   Acc;
diffKeys_1({Key, _Value, NextIter}, Map2, Acc) ->
   case maps:is_key(Key, Map2) of
      true ->
         diffKeys_1(maps:next(NextIter), Map2, Acc);
      _ ->
         diffKeys_1(maps:next(NextIter), Map2, [Key | Acc])
   end.

diffKeys_2(none, Map1) ->
   maps:to_list(Map1);
diffKeys_2({Key, _Value, NextIter}, Map1) ->
   diffKeys_2(maps:next(NextIter), maps:remove(Key, Map1)).
