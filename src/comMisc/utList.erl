-module(utList).

-define(TRUE, true).
-define(FALSE, false).
-define(BREAK, break).
-define(BREAK(Value), {?BREAK, Value}).
-define(CONTINUE, continue).
-define(CONTINUE(Value), {?CONTINUE, Value}).
-define(UNDEFINED, undefined).

-compile([export_all, nowarn_export_all]).

while(Fun, CurData) ->
   case Fun(CurData) of
      ?BREAK -> CurData;
      ?BREAK(BreakData) -> BreakData;
      ?CONTINUE ->
         while(Fun, CurData);
      ?CONTINUE(Continue) ->
         while(Fun, Continue)
   end.

%% For loop.
for(N, Fun) ->
   for(1, N, 1, Fun).

for(Start, End, Fun) ->
   for(Start, End, 1, Fun).

for(End, End, _Step, Fun) ->
   Fun(End);
for(Start, End, Step, Fun) ->
   Fun(Start),
   for(Start + Step, End, Step, Fun).


%% 带返回状态的for循环  @return {ok, State}
forWithState(Index, Max, _Fun, State) when Index > Max ->
   {ok, State};
forWithState(Max, Max, Fun, State) ->
   Fun(Max, State);
forWithState(Index, Max, Fun, State) ->
   {ok, NewState} = Fun(Index, State),
   forWithState(Index + 1, Max, Fun, NewState).

forWhile(Fun, State) ->
   forWhile(?CONTINUE, Fun, State).
forWhile(?BREAK, _Fun, State) ->
   State;
forWhile(?CONTINUE, Fun, State) ->
   {Next, NewState} = Fun(State),
   forWhile(Next, Fun, NewState).

%% 可以重复获取
randFormList([]) ->
   false;
randFormList(List) ->
   lists:nth(rand:uniform_s(1, length(List)), List).

%% 不能重复获取
randFormListOnce([]) ->
   {false, []};
randFormListOnce(List) ->
   Nth = rand:uniform_s(1, length(List)),
   NthData = lists:nth(Nth, List),
   {NthData, lists:delete(NthData, List)}.

randDelete(RemainList) ->
   Pos = rand:uniform_s(1, length(RemainList)),
   remove_nth(Pos, RemainList).

randDelete(DelCount, List) ->
   FunDel =
      fun(_, RemainList) ->
         RemainList(RemainList)
      end,
   lists:foldl(FunDel, List, lists:seq(1, DelCount)).

%%随机从集合中选出指定个数的元素length(List) >= Num
%%[1,2,3,4,5,6,7,8,9]中选出三个不同的数字[1,2,4]
getRandFromList(Num, List) ->
   ListSize = length(List),
   FunDel =
      fun(N, List1) ->
         Random = rand:uniform_s(1, (ListSize - N + 1)),
         Elem = lists:nth(Random, List1),
         lists:delete(Elem, List1)
      end,
   Result = lists:foldl(FunDel, List, lists:seq(1, Num)),
   List -- Result.


%%打乱列表函数  List =[] 返回 打乱后列表 List2
confuseList(List) ->
   confuseList(List, []).

confuseList(List, Result) ->
   confuseList(List, rand:uniform_s(1, length(List)), Result).

confuseList(_List, 0, Result) ->
   Result;
confuseList(List, Nth, Result) ->
   {NthData, Remain} = extract_member(rand:uniform_s(1, Nth), List),
   confuseList(Remain, length(Remain), [NthData | Result]).

%%提取列表中第Nth个元素，{NthData, RemainList}
extract_member(Nth, List) ->
   extract_member(Nth, List, []).

extract_member(1, List1, List2) ->
   case List1 of
      [R] ->
         {R, List2};
      [R | L] ->
         {R, List2 ++ L}
   end;

extract_member(Nth, [R | List1], List2) ->
   extract_member(Nth - 1, List1, List2 ++ [R]).

%%根数Tuple元素数量，把List转成Tuple 返回：{TupleList, RemainList}
list_to_tuple(List, TupleCount) ->
   list_to_tuple(List, TupleCount, []).
list_to_tuple(List, TupleCount, Result) when length(List) >= TupleCount ->
   {List1, List2} = lists:split(TupleCount, List),
   list_to_tuple(List2, TupleCount, [list_to_tuple(List1) | Result]);
list_to_tuple(List, _TupleCount, Result) ->
   {lists:reverse(Result), List}.

%%计算元素在列表中出现几次(5, [1,2,3,5,5,5,3]) ----> 3
repeatCount(Element, List) ->
   repeatCount(Element, List, 0).
repeatCount(Element, List, Count) ->
   case lists:member(Element, List) of
      false -> Count;
      true ->
         repeatCount(Element, lists:delete(Element, List), Count + 1)
   end.

remove_nth(RemainList, Pos) ->
   Head = lists:sublist(RemainList, 1, Pos - 1),
   End = lists:sublist(RemainList, Pos + 1, length(RemainList)),
   Head ++ End.

%%删除指定元素返回：(5, [1,2,4,4,5,5,4,5])   -> [1,2,4,4,4]
remove_element(Element, List) ->
   case lists:member(Element, List) of
      false -> List;
      true -> remove_element(Element, lists:delete(Element, List))
   end.

%%删除重复元素 [1,2,3,2,3,4] ---> [1,2,3,4]
remove_repeat(List) ->
   remove_repeat(List, []).

remove_repeat([], Result) ->
   Result;
remove_repeat([L | List], Result) ->
   case lists:member(L, Result) of
      false -> remove_repeat(List, Result ++ [L]);
      true -> remove_repeat(List, Result)
   end.


%%根据Key去查List中元素的Nth位相等的匹配到的元素Fun(Element)返回
find_elements(Key, Nth, List, Fun) ->
   InnerFun = fun(Element) ->
      case element(Nth, Element) =:= Key of
         true ->
            {true, Fun(Element)};
         false ->
            false
      end
              end,
   lists:filtermap(InnerFun, List).

%%对list进行去重，排序
%%Replicat 0不去重，1去重
%%Sort 0不排序，1排序
filter_list(List, Replicat, Sort) ->
   if Replicat == 0 andalso Sort == 0 ->
      List;
      true ->
         if Replicat == 1 andalso Sort == 1 ->
            lists:usort(List);
            true ->
               if Sort == 1 ->
                  lists:sort(List);
                  true ->
                     lists:reverse(filter_replicat(List, []))
               end
         end
   end.

%%list去重
filter_replicat([], List) ->
   List;
filter_replicat([H | Rest], List) ->
   Bool = lists:member(H, List),
   List1 =
      if Bool == true ->
         [[] | List];
         true ->
            [H | List]
      end,
   List2 = lists:filter(fun(T) -> T =/= [] end, List1),
   filter_replicat(Rest, List2).

%%交集
get_intersection_list(A, B) when is_list(A) andalso is_list(B) ->
   lists:filter(fun(X) -> lists:member(X, A) end, B).

%%并集
get_unite_list(A, B) when is_list(A) andalso is_list(B) ->
   C = A ++ B,
   lists:usort(C).

%%差集
get_subtract_list(A, B) when is_list(A) andalso is_list(B) ->
   Insection = get_intersection_list(A, B),
   Unite = get_unite_list(A, B),
   lists:filter(fun(X) -> lists:member(X, Insection) =:= false end, Unite).


merge(L) ->
   merge(L, []).
merge([{Key, Value} | T], AccList) ->
   NewAccList =
      case lists:keyfind(Key, 1, AccList) of
         false ->
            [{Key, Value} | AccList];
         {Key, OldValue} ->
            lists:keyreplace(Key, 1, AccList, {Key, OldValue + Value})
      end,
   merge(T, NewAccList);
merge([], AccList) ->
   AccList.
