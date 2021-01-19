-module(utTestRand).
%%该测试用于测试 全服每天随机某个值 检查改算法的随机性 主要用于测试该算法的随机平均概率值

-compile([export_all, nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).

tt0(Month, Len) ->
   erase(),
   randDay(31, Len, Month).



randDay(0, Len, Month) ->
   print(Len),
   ok;
randDay(CurDay, Len, Month) ->
   TemRand = 2019 + Month * 167 + CurDay * 313 + 2 * 131,
   N = TemRand rem Len + 1,
   io:format("IMY************randDay  ~p ~p ~n", [CurDay, N]),
   add(N),
   randDay(CurDay - 1, Len, Month).

tt1(Day, Len) ->
   erase(),
   randDay1(Day, Len).



randDay1(0, Len) ->
   print(Len),
   ok;
randDay1(CurDay, Len) ->
   TemRand = CurDay * 11,
   N = TemRand rem Len + 1,
   io:format("IMY************randDay  ~p ~p ~n", [CurDay, N]),
   add(N),
   randDay1(CurDay - 1, Len).

add(N) ->
   case erlang:get(N) of
      undefined ->
         erlang:put(N, 1);
      V ->
         erlang:put(N, 1 + V)
   end.

print(0) ->
   ok;
print(N) ->
   io:format("IMY************  ~p ~p ~n", [N, erlang:get(N)]),
   print(N - 1).



