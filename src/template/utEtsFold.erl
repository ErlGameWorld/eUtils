-module(utEtsFold).

%% 请注意，对于表类型ordered_set和由单个 ETS 函数调用(如select/2)完成的遍历(一次调用就返回了全部结果没有Next 迭代器了)，不需要 safe_fixtable/ 2 。

-export([rank/5, fold/2]).

rank(TabId, PageInfo, Page, Limit, TotalCnt) ->
   %% 遍历所有tab
   MS =
      case PageInfo == 0 orelse PageInfo of
         true ->
            ets:fun2ms(fun(T) -> T end);
         "" ->
            ets:fun2ms(fun(T) -> T end);
         _ ->
            try binary_to_term(list_to_binary(PageInfo)) of
               KeyTerm when KeyTerm == <<"">>; KeyTerm == "" ->
                  ets:fun2ms(fun(T) -> T end);
               KeyTerm ->
                  ets:fun2ms(fun({K, _V} = T) when K < KeyTerm -> T end)
            catch _C:_R ->
               ets:fun2ms(fun(T) -> T end)
            end
      end,

   case ets:select_reverse(TabId, MS, Limit) of
      '$end_of_table' ->
         {true, {<<"">>, []}};
      {AllyIds, '$end_of_table'} ->
         {true, makeRankData(AllyIds)};
      {AllyIds, _NextKey} ->
         Length = length(AllyIds),
         if
            Length < Limit ->
               {true, makeRankData(AllyIds)};
            true ->
               NewCurCnt = Page * Limit + Length,
               case NewCurCnt >= TotalCnt of
                  true ->
                     {true, makeRankData(AllyIds)};
                  _ ->
                     {false, makeRankData(AllyIds)}
               end
         end
   end.

makeRankData(AllyIds) ->
   makeRankData(AllyIds, <<"">>, []).

makeRankData([], LastKey, Acc) ->
   case LastKey of
      <<"">> ->
         {LastKey, lists:reverse(Acc)};
      _ ->
         {erlang:term_to_binary(LastKey), lists:reverse(Acc)}
   end;
makeRankData([{CurKey, Uuid} | AllyIds], LastKey, Acc) ->
   case ets:lookup(ets_pub_rank_info, Uuid) of
      [OneData] ->
         makeRankData(AllyIds, CurKey, [OneData | Acc]);
      _ ->
         makeRankData(AllyIds, LastKey, Acc)
   end.


fold(TabId, Limit) ->
   ets:safe_fixtable(TabId, true),
   try
      case ets:match_object(TabId, '$1', Limit) of
         '$end_of_table' ->
            ok;
         {ValueList, NextKey} ->
            matchDoFun(ValueList),
            continueNext(NextKey)
      end
   after
      ets:safe_fixtable(TabId, false)
   end.

continueNext(NextKey) ->
   case ets:match_object(NextKey) of
      '$end_of_table' ->
         ok;
      {ValueList, NewNextKey} ->
         matchDoFun(ValueList),
         continueNext(NewNextKey)
   end.

matchDoFun(_ValueList) ->
   %% do something
   ok.
