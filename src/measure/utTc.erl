-module(utTc).

-compile(inline).
-compile({inline_size, 128}).

-export([
   ts/4
   , tm/5
]).

%% 单进程循环测试：LoopTimes是循环次数
%% utTc:ts(LoopTimes, Module, Function, ArgsList).
%% 多进程并发测试：SpawnProcessesCount是并发的进程数 LoopTimes是循环次数
%% utTc:tm(ProcessesCount, LoopTimes, Module, Function, ArgsList).

doTc(M, F, A) ->
   {Microsecond, _} = timer:tc(M, F, A),
   Microsecond.

distribution(List, Aver) ->
   distribution(List, Aver, 0, 0).
distribution([H | T], Aver, Greater, Less) ->
   case H > Aver of
      true ->
         distribution(T, Aver, Greater + 1, Less);
      false ->
         distribution(T, Aver, Greater, Less + 1)
   end;
distribution([], _Aver, Greater, Less) ->
   {Greater, Less}.

%% ===================================================================
%% test: one process test N times
%% ===================================================================
ts(LoopTime, M, F, A) ->
   {Max, Min, Sum, Aver, Greater, Less} = loopTs(LoopTime, M, F, A, LoopTime, 0, 0, 0, []),
   io:format("=====================~n"),
   io:format("execute Args:~p~n", [A]),
   io:format("execute Fun :~p~n", [F]),
   io:format("execute Mod :~p~n", [M]),
   io:format("execute LoopTime:~p~n", [LoopTime]),
   io:format("MaxTime: ~10s(us) ~10s(s)~n", [integer_to_binary(Max), float_to_binary(Max / 1000000, [{decimals, 6}, compact])]),
   io:format("MinTime: ~10s(us) ~10s(s)~n", [integer_to_binary(Min), float_to_binary(Min / 1000000, [{decimals, 6}, compact])]),
   io:format("SumTime: ~10s(us) ~10s(s)~n", [integer_to_binary(Sum), float_to_binary(Sum / 1000000, [{decimals, 6}, compact])]),
   io:format("AvgTime: ~10s(us) ~10s(s)~n", [float_to_binary(Aver, [{decimals, 6}, compact]), float_to_binary(Aver / 1000000, [{decimals, 6}, compact])]),
   io:format("Grar   : ~10s(cn) ~10s(~s)~n", [integer_to_binary(Greater), float_to_binary(Greater / LoopTime, [{decimals, 2}]), <<"%">>]),
   io:format("Less   : ~10s(cn) ~10s(~s)~n", [integer_to_binary(Less), float_to_binary(Less / LoopTime, [{decimals, 2}]), <<"%">>]),
   io:format("=====================~n").


loopTs(0, _M, _F, _A, LoopTime, Max, Min, Sum, List) ->
   Aver = Sum / LoopTime,
   {Greater, Less} = distribution(List, Aver),
   {Max, Min, Sum, Aver, Greater, Less};
loopTs(Index, M, F, A, LoopTime, Max, Min, Sum, List) ->
   Microsecond = doTc(M, F, A),
   NewSum = Sum + Microsecond,
   if
      Max == 0 ->
         NewMax = NewMin = Microsecond;
      Max < Microsecond ->
         NewMax = Microsecond,
         NewMin = Min;
      Min > Microsecond ->
         NewMax = Max,
         NewMin = Microsecond;
      true ->
         NewMax = Max,
         NewMin = Min
   end,
   loopTs(Index - 1, M, F, A, LoopTime, NewMax, NewMin, NewSum, [Microsecond | List]).


%% ===================================================================
%% Concurrency test: N processes each test one time
%% ===================================================================

tm(ProcCnt, LoopTime, M, F, A) ->
   loopSpawn(ProcCnt, M, F, A, self(), LoopTime),
   {Max, Min, Sum, Aver, Greater, Less} = collector(ProcCnt, 0, 0, 0, ProcCnt, []),
   io:format("=====================~n"),
   io:format("execute Args:~p~n", [A]),
   io:format("execute Fun :~p~n", [F]),
   io:format("execute Mod :~p~n", [M]),
   io:format("execute LoopTime:~p~n", [LoopTime]),
   io:format("execute ProcCnts:~p~n", [ProcCnt]),
   io:format("MaxTime: ~10s(us) ~10s(s)~n", [integer_to_binary(Max), float_to_binary(Max / 1000000, [{decimals, 6}, compact])]),
   io:format("MinTime: ~10s(us) ~10s(s)~n", [integer_to_binary(Min), float_to_binary(Min / 1000000, [{decimals, 6}, compact])]),
   io:format("SumTime: ~10s(us) ~10s(s)~n", [integer_to_binary(Sum), float_to_binary(Sum / 1000000, [{decimals, 6}, compact])]),
   io:format("AvgTime: ~10s(us) ~10s(s)~n", [float_to_binary(Aver, [{decimals, 6}, compact]), float_to_binary(Aver / 1000000, [{decimals, 6}, compact])]),
   io:format("Grar   : ~10s(cn) ~10s(~s)~n", [integer_to_binary(Greater), float_to_binary(Greater / LoopTime, [{decimals, 2}]), <<"%">>]),
   io:format("Less   : ~10s(cn) ~10s(~s)~n", [integer_to_binary(Less), float_to_binary(Less / LoopTime, [{decimals, 2}]), <<"%">>]),
   io:format("=====================~n").


loopSpawn(0, _, _, _, _, _) ->
   ok;
loopSpawn(ProcCnt, M, F, A, CollectorPid, LoopTime) ->
   spawn_link(fun() -> worker(LoopTime, M, F, A, CollectorPid) end),
   loopSpawn(ProcCnt - 1, M, F, A, CollectorPid, LoopTime).

collector(0, Max, Min, Sum, ProcCnt, List) ->
   Aver = Sum / ProcCnt,
   {Greater, Less} = distribution(List, Aver),
   {Max, Min, Sum, Aver, Greater, Less};
collector(Index, Max, Min, Sum, ProcCnt, List) ->
   receive
      {result, Microsecond} ->
         NewSum = Sum + Microsecond,
         if
            Max == 0 ->
               NewMax = NewMin = Microsecond;
            Max < Microsecond ->
               NewMax = Microsecond,
               NewMin = Min;
            Min > Microsecond ->
               NewMax = Max,
               NewMin = Microsecond;
            true ->
               NewMax = Max,
               NewMin = Min
         end,
         collector(Index - 1, NewMax, NewMin, NewSum, ProcCnt, [Microsecond | List])
   after 1800000 ->
      io:format("execute time out~n"),
      ok
   end.

worker(LoopTime, M, F, A, CollectorPid) ->
   SumTime = loopTm(LoopTime, M, F, A, 0),
   CollectorPid ! {result, SumTime}.

loopTm(0, _, _, _, SumTime) ->
   SumTime;
loopTm(LoopTime, M, F, A, SumTime) ->
   Microsecond = doTc(M, F, A),
   loopTm(LoopTime - 1, M, F, A, SumTime + Microsecond).


