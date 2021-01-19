-module(utListPdTs).
-compile([export_all, nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).
-record(tempCnt, {
   insert = []
   , read = []
   , update = []
   , for = []
   , delete = []
}).

-define(Cnt, 12).
-define(V_NUM, [8, 16, 32, 64, 128, 256, 516, 1024, 2048]).
-define(DsList, [utPdTs, utListTs]).



start() ->
   %%erlang:process_flag(trap_exit, true),
   erlang:erase(),
   printLog("Ts benchmark...", []),

   runTs(?DsList, ?V_NUM),
   printLog("Ts benchmark...Over calculate the AVG~n", []),
   runAvg(?DsList, ?V_NUM).


runTs([Ds | T], VNumList) ->
   printTitle(),
   runNum(VNumList, Ds),
   runTs(T, VNumList);
runTs([], _VNumList) ->
   ok.

runNum([Num | T], Ds) ->
   runCnt(?Cnt, Num, Ds),
   runNum(T, Ds);
runNum([], _Ds) ->
   ok.

runCnt(0, Num, Ds) ->
   ok;
runCnt(Cnt, Num, Ds) ->
   runExe(Num, Ds),
   runCnt(Cnt - 1, Num, Ds).

runExe(Num, Ds) ->
   Pid = erlang:spawn_link(Ds, start, [Num, self()]),
   receive
      {over, Pid, Insert, Read, Delete} ->
         storeStatistics(Ds, Num, Insert, Read, Delete),
         {_, DsName} = lists:split(2, atom_to_list(Ds)),
         printLog("~-10.s ~8.s ~12.s ~12.s ~10.s ~12.s ~12.s ~12.s ~n",
         [DsName, integer_to_list(Num), timeToStr(Insert), calcPer(Insert, Num), timeToStr(Read), calcPer(Read, Num), timeToStr(Delete), calcPer(Delete, Num)]);

      {'EXIT', Pid, normal} ->
         ok;
      _ShutDown ->
         io:format("Ts test shutDown ~p ~p ~p ~n", [Ds, Num, _ShutDown])
   end.

runAvg([Ds | T], VNumList) ->
   printAvg(),
   runCal(VNumList, Ds),
   runAvg(T, VNumList);
runAvg([], _VNumList) ->
   ok.

runCal([Num | T], Ds) ->
   #tempCnt{insert = InsertList, read = ReadList, delete = DeleteList} = getStatistics(Ds, Num),
   {_, DsName} = lists:split(2, atom_to_list(Ds)),
   AvgI = calcAvg(InsertList, Num),
   AvgR = calcAvg(ReadList, Num),
   AvgD = calcAvg(DeleteList, Num),
   AvgAll = calcAvgAll(InsertList, ReadList, DeleteList, Num),
   printLog("~-10.s ~8.s ~12.s ~12.s ~12.s ~12.s~n",
      [DsName, integer_to_list(Num), AvgI, AvgR, AvgD, AvgAll]),
   runCal(T, Ds);
runCal([], _Ds) ->
   ok.

-define(S, 1000000000).
-define(MS, 1000000).
-define(US, 1000).
-define(NS, 1).

timeToStr(not_support) ->
   <<"noSupport">>;
timeToStr(skip) ->
   <<"skip">>;
% timeToStr(Time) when Time > ?S ->
%    float_to_list(Time / ?S, [{decimals, 2}]) ++ "s";
% timeToStr(Time) when Time > ?MS ->
%    float_to_list(Time / ?MS, [{decimals, 2}]) ++ "ms";
% timeToStr(Time) when Time > ?US ->
%    float_to_list(Time / ?US, [{decimals, 2}]) ++ "us";
timeToStr(Time) ->
   integer_to_list(Time) ++ "ns".

calcPer(not_support, _Num) ->
   <<"notSupport">>;
calcPer(skip, _Num) ->
   <<"skip">>;
calcPer(Time, Num) ->
   float_to_list(Time / Num, [{decimals, 2}]) ++ "ns".

calcAvg([not_support | _], Num) ->
   <<"notSupport">>;
calcAvg([skip | _], Num) ->
   <<"skip">>;
calcAvg(CntList, Num) ->
   %% 去掉最大值与最小值 然后求平均值
   AvgCnt = ?Cnt - 2,
   SortList = lists:sort(CntList),
   AvgList = lists:sublist(SortList, 2, AvgCnt),
   float_to_list(lists:sum(AvgList) / AvgCnt, [{decimals, 2}]) ++ "ns".

calcAvgAll(InsertList, ReadList, DeleteList, Num) ->
   %% 去掉最大值与最小值 然后求平均值
   AvgCnt = ?Cnt - 2,
   SortList1 = lists:sort(InsertList),
   AvgList1 = lists:sublist(SortList1, 2, AvgCnt),
   SortList2 = lists:sort(ReadList),
   AvgList2 = lists:sublist(SortList2, 2, AvgCnt),
   SortList3 = lists:sort(DeleteList),
   AvgList3 = lists:sublist(SortList3, 2, AvgCnt),
   float_to_list((lists:sum(AvgList1) + lists:sum(AvgList2) + lists:sum(AvgList3))  / AvgCnt, [{decimals, 2}]) ++ "ns".

storeStatistics(Ds, Num, Insert, Read, Delete) ->
   #tempCnt{insert = InsertList, read = ReadList, delete = DeleteList} =
      case erlang:get({Ds, Num}) of
         undefined ->
            #tempCnt{};
         TempCnt ->
            TempCnt
      end,
   NewTempCnt = #tempCnt{insert = [Insert | InsertList], read = [Read | ReadList], delete = [Delete | DeleteList]},
   erlang:put({Ds, Num}, NewTempCnt).

getStatistics(Ds, Num) ->
   erlang:get({Ds, Num}).

printTitle() ->
   printLog("~n~-10.s ~8.s ~12.s ~12.s ~10.s ~12.s ~12.s ~12.s ~n",
      ["DsName", "V_Num", "insert", "insert/per", "read", "read/per", "delete", "delete/per"]),
   printLog("~s ~n", [[$= || _ <- lists:seq(1, 80)]]).

printAvg() ->
   printLog("~n~-10.s ~8.s ~12.s ~12.s ~12.s ~12.s~n", ["DsName", "V_Num", "insert/per", "read/per", "delete/per", "totalUse"]),
   printLog("~s ~n", [[$= || _ <- lists:seq(1, 64)]]).

printLog(Format, Args) ->
   % {ok, File} = file:open("src/docs/erlang-DsBenchMark.txt", [write, append]),
   % io:format(File, Format, Args),
   % file:close(File).
   io:format(Format, Args).


makeV(N) ->
   {N, test, [list, 123, 456.789, "test"], {23231, "gggggg"}, <<"12345678901234567890">>}.

