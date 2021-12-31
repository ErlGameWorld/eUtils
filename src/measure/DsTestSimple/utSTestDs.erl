-module(utSTestDs).
-compile([export_all, nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).
-record(tempCnt, {
   insert = []
   , read = []
   , update = []
   , for = []
   , delete = []
   , size = []
}).

%-define(V_NUM, [8, 16, 32, 64, 128, 256, 516, 1024, 2048, 4096, 8192, 16384, 32768, 65536, 131072, 524288, 1048576]).
-define(V_NUM, [8, 16, 32, 64, 128, 256, 516, 1024, 2048, 4096, 8192, 16384]).
% -define(DsList, [utSPdDs, utSTupleDs, utSListsDs, utSMapsDs, utSQueueDs, utSUtFifoDs, utSUtLifoDs, utSEnlfqDs, utSeLfqDs, utSEtsSetDs, utSEtsOrdDs, utSArrayDs, utSDictDs, utSGb_treesDs, utSSetsDs, utSGb_setsDs, utSOrddictDs, utSOrdsetsDs]).
-define(DsList, [utSPdDs, utSTupleDs, utSMapsDs, utSQueueDs, utSUtFifoDs, utSUtLifoDs, utSFwQueueDs, utSShqDs, utSShq2Ds]).

-define(Cnt, 12).

start() ->
   %%erlang:process_flag(trap_exit, true),
   erlang:erase(),
   printLog("Ds benchmark...", []),

   runDs(?DsList, ?V_NUM),
   printLog("Ds benchmark...Over calculate the AVG~n", []),
   runAvg(?DsList, ?V_NUM).


runDs([Ds | T], VNumList) ->
   printTitle(),
   runNum(VNumList, Ds),
   runDs(T, VNumList);
runDs([], _VNumList) ->
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
      {over, Pid, InsertTU, ReadTU, UpdateTU, ForTU, DeleteTU, TermSize} ->
         Insert = cvrTUnit(InsertTU),
         Read = cvrTUnit(ReadTU),
         Update = cvrTUnit(UpdateTU),
         For = cvrTUnit(ForTU),
         Delete = cvrTUnit(DeleteTU),
         storeStatistics(Ds, Num, Insert, Read, Update, For, Delete, TermSize),
         {_, DsName} = lists:split(2, atom_to_list(Ds)),
         printLog("~-10.s ~8.s ~12.s ~12.s ~10.s ~12.s ~10.s ~14.s ~10.s ~12.s ~12.s ~12.s ~12.s ~n",
            [DsName, integer_to_list(Num), timeToStr(Insert), calcPer(Insert, Num), timeToStr(Read), calcPer(Read, Num), timeToStr(Update), calcPer(Update, Num), timeToStr(For), calcPer(For, Num), timeToStr(Delete), calcPer(Delete, Num), sizeToStr(TermSize)]);
      {'EXIT', Pid, normal} ->
         ok;
      _ShutDown ->
         io:format("Ds test shutDown ~p ~p ~p ~n", [Ds, Num, _ShutDown])
   end.

runAvg([Ds | T], VNumList) ->
   printAvg(),
   runCal(VNumList, Ds),
   runAvg(T, VNumList);
runAvg([], _VNumList) ->
   ok.

runCal([Num | T], Ds) ->
   #tempCnt{insert = InsertList, read = ReadList, update = UpdateList, for = ForList, delete = DeleteList, size = SizeList} = getStatistics(Ds, Num),
   {_, DsName} = lists:split(2, atom_to_list(Ds)),
   printLog("~-10.s ~8.s ~12.s ~12.s ~14.s ~12.s ~12.s ~12.s~n",
      [DsName, integer_to_list(Num), calcAvgT(InsertList, Num), calcAvgT(ReadList, Num), calcAvgT(UpdateList, Num), calcAvgT(ForList, Num), calcAvgT(DeleteList, Num), calcAvgS(SizeList, Num)]),
   runCal(T, Ds);
runCal([], _Ds) ->
   ok.

-define(S, 1000000000).
-define(MS, 1000000).
-define(US, 1000).
-define(NS, 1).

cvrTUnit(not_support) ->
   not_support;
cvrTUnit(skip) ->
   skip;
cvrTUnit(Time) ->
   utTc:cvrTimeUnit(Time, native, nanosecond).

timeToStr(not_support) ->
   <<"noSupport">>;
timeToStr(skip) ->
   <<"skip">>;
timeToStr(Time) when Time > ?S ->
   float_to_list(Time / ?S, [{decimals, 2}]) ++ "s";
timeToStr(Time) when Time > ?MS ->
   float_to_list(Time / ?MS, [{decimals, 2}]) ++ "ms";
timeToStr(Time) when Time > ?US ->
   float_to_list(Time / ?US, [{decimals, 2}]) ++ "us";
timeToStr(Time) ->
   integer_to_list(Time) ++ "ns".

sizeToStr(no_size) ->
   <<"noSize">>;
sizeToStr(TermSize) ->
   integer_to_list(TermSize).

calcPer(not_support, _Num) ->
   <<"notSupport">>;
calcPer(skip, _Num) ->
   <<"skip">>;
calcPer(Time, Num) ->
   float_to_list(Time / Num, [{decimals, 2}]) ++ "ns".

calcAvgS([no_size | _], Num) ->
   <<"noSize">>;
calcAvgS(CntList, Num) ->
   %% 去掉最大值与最小值 然后求平均值
   AvgCnt = ?Cnt - 2,
   SortList = lists:sort(CntList),
   AvgList = lists:sublist(SortList, 2, AvgCnt),
   integer_to_list(erlang:trunc(lists:sum(AvgList) / AvgCnt)).

calcAvgT([not_support | _], Num) ->
   <<"notSupport">>;
calcAvgT([skip | _], Num) ->
   <<"skip">>;
calcAvgT(CntList, Num) ->
   %% 去掉最大值与最小值 然后求平均值
   AvgCnt = ?Cnt - 2,
   SortList = lists:sort(CntList),
   AvgList = lists:sublist(SortList, 2, AvgCnt),
   float_to_list(lists:sum(AvgList) / AvgCnt / Num, [{decimals, 2}]) ++ "ns".

storeStatistics(Ds, Num, Insert, Read, Update, For, Delete, TermSize) ->
   #tempCnt{insert = InsertList, read = ReadList, update = UpdateList, for = ForList, delete = DeleteList, size = SizeList} =
      case erlang:get({Ds, Num}) of
         undefined ->
            #tempCnt{};
         TempCnt ->
            TempCnt
      end,
   NewTempCnt = #tempCnt{insert = [Insert | InsertList], read = [Read | ReadList], update = [Update | UpdateList], for = [For | ForList], delete = [Delete | DeleteList], size = [TermSize | SizeList]},
   erlang:put({Ds, Num}, NewTempCnt).

getStatistics(Ds, Num) ->
   erlang:get({Ds, Num}).

printTitle() ->
   printLog("~n~-10.s ~8.s ~12.s ~12.s ~10.s ~12.s ~10.s ~14.s ~10.s ~12.s ~12.s ~12.s ~12.s ~n",
      ["DsName", "V_Num", "insert", "insert/per", "read", "read/per", "update", "update/per", "for", "for/per", "delete", "delete/per", "termSize"]),
   printLog("~s ~n", [[$= || _ <- lists:seq(1, 145)]]).

printAvg() ->
   printLog("~n~-10.s ~8.s ~12.s ~12.s ~14.s ~12.s ~12.s ~12.s~n",
      ["DsName", "V_Num", "insert/per", "read/per", "update/per", "for/per", "delete/per", "termSize"]),
   printLog("~s ~n", [[$= || _ <- lists:seq(1, 85)]]).

printLog(Format, Args) ->
   % {ok, File} = file:open("src/docs/erlang-DsBenchMark.txt", [write, append]),
   % io:format(File, Format, Args),
   % file:close(File).
   io:format(Format, Args).


makeK(N) ->
   case N rem 4 of
      0 ->
         N;
      1 ->
         {N, <<"test-testDs">>};
      2 ->
         {N, test};
      3 ->
         [N, test]
   end.

makeV(N) ->
   case N rem 4 of
      0 ->
         N;
      1 ->
         {N, <<"test-testDs">>};
      2 ->
         {N, 8787.87878, <<"test-testDs">>};
      3 ->
         {N, test, [list, 123, 456.789, "test"], {23231, "gggggg"}, <<"12345678901234567890">>}
   end.

makeV2(N) ->
   case N rem 4 of
      0 ->
         {N, 8787.87878, <<"test-testDs">>};
      1 ->
         {N, test, [list, 123, 456.789, "test"], {23231, "gggggg"}, <<"12345678901234567890">>};
      2 ->
         N;
      3 ->
         {N, <<"test-testDs">>}
   end.

makeRandV() ->
   utGenTerm:any().

