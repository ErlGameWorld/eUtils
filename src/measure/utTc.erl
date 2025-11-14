-module(utTc).

-compile(inline).
-compile({inline_size, 128}).

-export([
	tc/1
	, tc/2
	, tc/3
	, tc/4
	, tc/5
	, tm/4
	, tm/5
	, tmc/4
	, tmc/5
	
	, testTime/1
	, testMem/1
	, testOk/0
	, testCrash/0
]).

-type args() :: Fun :: function() | {Fun :: function(), Args :: [term()]} | {M :: module(), F :: atom(), Args :: [term()]}.

%% Measure the execution time (in nanoseconds) for Fun().
-spec tc(Fun :: function()) -> {TimeUsed :: integer(), Value :: term()}.
tc(F) ->
	T1 = erlang:monotonic_time(),
	Val = F(),
	T2 = erlang:monotonic_time(),
	TimeUsed = erlang:convert_time_unit(T2 - T1, native, nanosecond),
	{TimeUsed, Val}.

%% Measure the execution time (in nanoseconds) for Fun(Args).
-spec tc(Fun :: function(), Arguments :: args()) -> {TimeUsed :: integer(), Value :: term()}.
tc(F, A) ->
	Args = getArgs(A),
	T1 = erlang:monotonic_time(),
	Val = apply(F, Args),
	T2 = erlang:monotonic_time(),
	TimeUsed = erlang:convert_time_unit(T2 - T1, native, nanosecond),
	{TimeUsed, Val}.

%% Measure the execution time (in nanoseconds) for an MFA.
-spec tc(Module :: module(), Function :: atom(), Arguments :: args()) -> {TimeUsed :: integer(), Value :: term()}.
tc(M, F, A) ->
	Args = getArgs(A),
	T1 = erlang:monotonic_time(),
	Val = apply(M, F, Args),
	T2 = erlang:monotonic_time(),
	TimeUsed = erlang:convert_time_unit(T2 - T1, native, nanosecond),
	{TimeUsed, Val}.

%% 单进程循环测试：LoopTimes是循环次数
%% utTc:tc(LoopTimes, Module, Function, ArgsList).
%% 多进程并发测试：SpawnProcessesCount是并发的进程数 LoopTimes是循环次数
%% utTc:tc(ProcessesCount, LoopTimes, Module, Function, ArgsList).
%% ===================================================================
%% test: one process test N times
%% ===================================================================
tc(LoopTime, M, F, A) ->
	{Min, Max, Sum, TimeList} = loopSTc(LoopTime, M, F, A, LoopTime, none, none, 0, []),
	Aver = Sum / LoopTime,
	{Greater, Less} = distribution(TimeList, Aver),
	Percentiles = percentiles(TimeList, [50, 90, 95, 99]),
	io:format("==========================================~n"),
	ArgsStr = formatArgs(A),
	io:format("execute ~p:~p(~s).~n", [M, F, ArgsStr]),
	io:format("execute LoopTime:~p~n", [LoopTime]),
	io:format("MinTime: ~15s(ns) ~15s(s)~n", [integer_to_binary(Min), float_to_binary(Min / 1000000000, [{decimals, 2}, compact])]),
	io:format("MaxTime: ~15s(ns) ~15s(s)~n", [integer_to_binary(Max), float_to_binary(Max / 1000000000, [{decimals, 2}, compact])]),
	io:format("SumTime: ~15s(ns) ~15s(s)~n", [integer_to_binary(Sum), float_to_binary(Sum / 1000000000, [{decimals, 2}, compact])]),
	io:format("AvgTime: ~15s(ns) ~15s(s)~n", [float_to_binary(float(Aver), [{decimals, 2}, compact]), float_to_binary(Aver / 1000000000, [{decimals, 2}, compact])]),
	io:format("Grar   : ~15s(cn) ~15s(~s)~n", [integer_to_binary(Greater), float_to_binary(Greater / LoopTime, [{decimals, 2}]), <<"%">>]),
	io:format("Less   : ~15s(cn) ~15s(~s)~n", [integer_to_binary(Less), float_to_binary(Less / LoopTime, [{decimals, 2}]), <<"%">>]),
	io:format("P50    : ~15s(ns) ~15s(s)~n", [integer_to_binary(lists:nth(1, Percentiles)), float_to_binary(lists:nth(1, Percentiles) / 1000000000, [{decimals, 2}, compact])]),
	io:format("P90    : ~15s(ns) ~15s(s)~n", [integer_to_binary(lists:nth(2, Percentiles)), float_to_binary(lists:nth(2, Percentiles) / 1000000000, [{decimals, 2}, compact])]),
	io:format("P95    : ~15s(ns) ~15s(s)~n", [integer_to_binary(lists:nth(3, Percentiles)), float_to_binary(lists:nth(3, Percentiles) / 1000000000, [{decimals, 2}, compact])]),
	io:format("P99    : ~15s(ns) ~15s(s)~n", [integer_to_binary(lists:nth(4, Percentiles)), float_to_binary(lists:nth(4, Percentiles) / 1000000000, [{decimals, 2}, compact])]),
	io:format("==========================================~n").

%% ===================================================================
%% Concurrency test: N processes each test one time
%% ===================================================================

tc(ProcCnt, LoopTime, M, F, A) ->
	AllAid = loopSpawn(ProcCnt, M, F, A, self(), LoopTime, []),
	startTest(tcTest, AllAid),
	{TProcCnt, Min, Max, Sum, TimeList} = tcCollector(ProcCnt, 0, none, none, 0, []),
	clearDown(),
	CalcProcCnt = case TProcCnt == 0 of true -> 1; _ -> TProcCnt end,
	Aver = Sum / CalcProcCnt,
	{Greater, Less} = distribution(TimeList, Aver),
	Percentiles = percentiles(TimeList, [50, 90, 95, 99]),
	io:format("==========================================~n"),
	ArgsStr = formatArgs(A),
	io:format("execute ~p:~p(~s).~n", [M, F, ArgsStr]),
	io:format("execute LoopTime:~p~n", [LoopTime]),
	io:format("execute ProcCnts:~p success:~p~n", [ProcCnt, TProcCnt]),
	io:format("PMinTime: ~15s(ns) ~15s(s)~n", [integer_to_binary(Min), float_to_binary(Min / 1000000000, [{decimals, 2}, compact])]),
	io:format("PMaxTime: ~15s(ns) ~15s(s)~n", [integer_to_binary(Max), float_to_binary(Max / 1000000000, [{decimals, 2}, compact])]),
	io:format("PSumTime: ~15s(ns) ~15s(s)~n", [integer_to_binary(Sum), float_to_binary(Sum / 1000000000, [{decimals, 2}, compact])]),
	io:format("PAvgTime: ~15s(ns) ~15s(s)~n", [float_to_binary(float(Aver), [{decimals, 2}, compact]), float_to_binary(Aver / 1000000000, [{decimals, 2}, compact])]),
	io:format("FAvgTime: ~15s(ns) ~15s(s)~n", [float_to_binary(Aver / LoopTime, [{decimals, 2}, compact]), float_to_binary(Aver / LoopTime / 1000000000, [{decimals, 2}, compact])]),
	io:format("PGrar   : ~15s(cn) ~15s(~s)~n", [integer_to_binary(Greater), float_to_binary(Greater / CalcProcCnt, [{decimals, 2}]), <<"%">>]),
	io:format("PLess   : ~15s(cn) ~15s(~s)~n", [integer_to_binary(Less), float_to_binary(Less / CalcProcCnt, [{decimals, 2}]), <<"%">>]),
	io:format("P50     : ~15s(ns) ~15s(s)~n", [integer_to_binary(lists:nth(1, Percentiles)), float_to_binary(lists:nth(1, Percentiles) / 1000000000, [{decimals, 2}, compact])]),
	io:format("P90     : ~15s(ns) ~15s(s)~n", [integer_to_binary(lists:nth(2, Percentiles)), float_to_binary(lists:nth(2, Percentiles) / 1000000000, [{decimals, 2}, compact])]),
	io:format("P95     : ~15s(ns) ~15s(s)~n", [integer_to_binary(lists:nth(3, Percentiles)), float_to_binary(lists:nth(3, Percentiles) / 1000000000, [{decimals, 2}, compact])]),
	io:format("P99     : ~15s(ns) ~15s(s)~n", [integer_to_binary(lists:nth(4, Percentiles)), float_to_binary(lists:nth(4, Percentiles) / 1000000000, [{decimals, 2}, compact])]),
	io:format("==========================================~n").

%% ===================================================================
%% Memory usage measurement functions
%% ===================================================================

tm(LoopTime, M, F, A) ->
	%% 强制垃圾回收，确保测量前内存状态干净
	garbage_collect(),
	%% 测量基准内存使用
	{BeforePMem, BeforeVmMem} = getMem(),
	%% 执行函数并测量内存峰值
	{PMin, PMax, PSum, VmMin, VmMax, VmSum} = loopSTm(LoopTime, M, F, A, LoopTime, none, none, 0, none, none, 0),
	%% 再次强制垃圾回收，测量最终内存使用
	garbage_collect(),
	{AfterPMem, AfterVmMem} = getMem(),
	%% 计算统计信息
	AvgPMem = PSum / LoopTime,
	AvgVmMem = VmSum / LoopTime,
	
	io:format("==========================================~n"),
	ArgsStr = formatArgs(A),
	io:format("execute ~p:~p(~s).~n", [M, F, ArgsStr]),
	io:format("LoopTime: ~p~n", [LoopTime]),
	io:format("Base  memory(process|vm total): ~15s(bytes) ~15s(bytes)~n", [integer_to_binary(BeforePMem), integer_to_binary(BeforeVmMem)]),
	io:format("Final memory(process|vm total): ~15s(bytes) ~15s(bytes)~n", [integer_to_binary(AfterPMem), integer_to_binary(AfterVmMem)]),
	io:format("Memory delta(process|vm total): ~15s(bytes) ~15s(bytes)~n", [integer_to_binary(AfterPMem - BeforePMem), integer_to_binary(AfterVmMem - BeforeVmMem)]),
	io:format("Min process memory            : ~15s(bytes)~n", [float_to_binary(float(PMin), [{decimals, 2}, compact])]),
	io:format("Max process memory            : ~15s(bytes)~n", [float_to_binary(float(PMax), [{decimals, 2}, compact])]),
	io:format("Avg process memory            : ~15s(bytes)~n", [float_to_binary(float(AvgPMem), [{decimals, 2}, compact])]),
	io:format("Min vm memory                 : ~15s(bytes)~n", [float_to_binary(float(VmMin), [{decimals, 2}, compact])]),
	io:format("Max vm memory                 : ~15s(bytes)~n", [float_to_binary(float(VmMax), [{decimals, 2}, compact])]),
	io:format("Avg vm memory                 : ~15s(bytes)~n", [float_to_binary(float(AvgVmMem), [{decimals, 2}, compact])]),
	io:format("==========================================~n").

tm(ProcCnt, LoopTime, M, F, A) ->
	%% 多进程内存测试
	AllAid = loopSpawn(ProcCnt, M, F, A, self(), LoopTime, []),
	%% 强制垃圾回收
	garbage_collect(),
	%% 测量基准内存使用
	{BeforePMem, BeforeVmMem} = getMem(),
	startTest(tmTest, AllAid),
	{TProcCnt, PMin, PMax, PSum, VmMin, VmMax, VmSum} = tmCollector(ProcCnt, 0, none, none, 0, none, none, 0),
	clearDown(),
	CalcProcCnt = case TProcCnt == 0 of true -> 1; _ -> TProcCnt end,
	%% 再次强制垃圾回收
	garbage_collect(),
	{AfterPMem, AfterVmMem} = getMem(),
	%% 计算统计信息
	AvgPMem = PSum / CalcProcCnt,
	AvgVmMem = VmSum / CalcProcCnt,
	io:format("==========================================~n"),
	ArgsStr = formatArgs(A),
	io:format("execute ~p:~p(~s).~n", [M, F, ArgsStr]),
	io:format("execute LoopTime:~p~n", [LoopTime]),
	io:format("execute ProcCnts:~p success:~p~n", [ProcCnt, TProcCnt]),
	io:format("Base  memory(process|vm total): ~15s(bytes) ~15s(bytes)~n", [integer_to_binary(BeforePMem), integer_to_binary(BeforeVmMem)]),
	io:format("Final memory(process|vm total): ~15s(bytes) ~15s(bytes)~n", [integer_to_binary(AfterPMem), integer_to_binary(AfterVmMem)]),
	io:format("Memory delta(process|vm total): ~15s(bytes) ~15s(bytes)~n", [integer_to_binary(AfterPMem - BeforePMem), integer_to_binary(AfterVmMem - BeforeVmMem)]),
	io:format("PMin process memory           : ~15s(bytes)~n", [float_to_binary(float(PMin), [{decimals, 2}, compact])]),
	io:format("PMax process memory           : ~15s(bytes)~n", [float_to_binary(float(PMax), [{decimals, 2}, compact])]),
	io:format("FAvg process memory           : ~15s(bytes)~n", [float_to_binary(float(AvgPMem), [{decimals, 2}, compact])]),
	io:format("PMin vm memory                : ~15s(bytes)~n", [float_to_binary(float(VmMin), [{decimals, 2}, compact])]),
	io:format("PMax vm memory                : ~15s(bytes)~n", [float_to_binary(float(VmMax), [{decimals, 2}, compact])]),
	io:format("FAvg vm memory                : ~15s(bytes)~n", [float_to_binary(float(AvgVmMem), [{decimals, 2}, compact])]),
	io:format("==========================================~n").

%% ===================================================================
%% Comprehensive test: Time and memory measurement together
%% ===================================================================

tmc(LoopTime, M, F, A) ->
	%% 强制垃圾回收，确保测量前内存状态干净
	garbage_collect(),
	%% 测量基准内存使用
	{BeforePMem, BeforeVmMem} = getMem(),
	%% 执行函数并同时测量时间和内存
	{PMin, PMax, PSum, VmMin, VmMax, VmSum, TMin, TMax, TSum} = loopSTmc(LoopTime, M, F, A, LoopTime, none, none, 0, none, none, 0, none, none, 0),
	%% 再次强制垃圾回收，测量最终内存使用
	garbage_collect(),
	{AfterPMem, AfterVmMem} = getMem(),
	
	%% 计算统计信息
	AvgPMem = PSum / LoopTime,
	AvgVmMem = VmSum / LoopTime,
	AvgTime = TSum / LoopTime,
	io:format("==========================================~n"),
	ArgsStr = formatArgs(A),
	io:format("execute ~p:~p(~s).~n", [M, F, ArgsStr]),
	io:format("LoopTime: ~p~n", [LoopTime]),
	io:format("Base  memory(process|vm total): ~15s(bytes) ~15s(bytes)~n", [integer_to_binary(BeforePMem), integer_to_binary(BeforeVmMem)]),
	io:format("Final memory(process|vm total): ~15s(bytes) ~15s(bytes)~n", [integer_to_binary(AfterPMem), integer_to_binary(AfterVmMem)]),
	io:format("Memory delta(process|vm total): ~15s(bytes) ~15s(bytes)~n", [integer_to_binary(AfterPMem - BeforePMem), integer_to_binary(AfterVmMem - BeforeVmMem)]),
	io:format("MinTime                       : ~15s(ns) ~15s(s)~n", [integer_to_binary(TMin), float_to_binary(TMin / 1000000000, [{decimals, 2}, compact])]),
	io:format("MaxTime                       : ~15s(ns) ~15s(s)~n", [integer_to_binary(TMax), float_to_binary(TMax / 1000000000, [{decimals, 2}, compact])]),
	io:format("SumTime                       : ~15s(ns) ~15s(s)~n", [integer_to_binary(TSum), float_to_binary(TSum / 1000000000, [{decimals, 2}, compact])]),
	io:format("AvgTime                       : ~15s(ns) ~15s(s)~n", [float_to_binary(float(AvgTime), [{decimals, 2}, compact]), float_to_binary(AvgTime / 1000000000, [{decimals, 2}, compact])]),
	io:format("Min process memory            : ~15s(bytes)~n", [float_to_binary(float(PMin), [{decimals, 2}, compact])]),
	io:format("Max process memory            : ~15s(bytes)~n", [float_to_binary(float(PMax), [{decimals, 2}, compact])]),
	io:format("Avg process memory            : ~15s(bytes)~n", [float_to_binary(float(AvgPMem), [{decimals, 2}, compact])]),
	io:format("Min vm memory                 : ~15s(bytes)~n", [float_to_binary(float(VmMin), [{decimals, 2}, compact])]),
	io:format("Max vm memory                 : ~15s(bytes)~n", [float_to_binary(float(VmMax), [{decimals, 2}, compact])]),
	io:format("Avg vm memory                 : ~15s(bytes)~n", [float_to_binary(float(AvgVmMem), [{decimals, 2}, compact])]),
	io:format("==========================================~n").

tmc(ProcCnt, LoopTime, M, F, A) ->
	%% 多进程综合测试
	AllAid = loopSpawn(ProcCnt, M, F, A, self(), LoopTime, []),
	%% 强制垃圾回收
	garbage_collect(),
	%% 测量基准内存使用
	{BeforePMem, BeforeVmMem} = getMem(),
	startTest(tmcTest, AllAid),
	{TProcCnt, PMin, PMax, PSum, VmMin, VmMax, VmSum, TMin, TMax, TSum} = tmcCollector(ProcCnt, 0, none, none, 0, none, none, 0, none, none, 0),
	clearDown(),
	CalcProcCnt = case TProcCnt == 0 of true -> 1; _ -> TProcCnt end,
	%% 再次强制垃圾回收
	garbage_collect(),
	{AfterPMem, AfterVmMem} = getMem(),
	
	%% 计算统计信息
	AvgPMem = PSum / CalcProcCnt,
	AvgVmMem = VmSum / CalcProcCnt,
	AvgTime = TSum / CalcProcCnt,
	ArgsStr = formatArgs(A),
	io:format("execute ~p:~p(~s).~n", [M, F, ArgsStr]),
	io:format("execute LoopTime:~p~n", [LoopTime]),
	io:format("execute ProcCnts:~p success:~p~n", [ProcCnt, TProcCnt]),
	io:format("Base  memory(process|vm total): ~15s(bytes) ~15s(bytes)~n", [integer_to_binary(BeforePMem), integer_to_binary(BeforeVmMem)]),
	io:format("Final memory(process|vm total): ~15s(bytes) ~15s(bytes)~n", [integer_to_binary(AfterPMem), integer_to_binary(AfterVmMem)]),
	io:format("Memory delta(process|vm total): ~15s(bytes) ~15s(bytes)~n", [integer_to_binary(AfterPMem - BeforePMem), integer_to_binary(AfterVmMem - BeforeVmMem)]),
	io:format("PMinTime                      : ~15s(ns) ~15s(s)~n", [integer_to_binary(TMin), float_to_binary(TMin / 1000000000, [{decimals, 2}, compact])]),
	io:format("PMaxTime                      : ~15s(ns) ~15s(s)~n", [integer_to_binary(TMax), float_to_binary(TMax / 1000000000, [{decimals, 2}, compact])]),
	io:format("PSumTime                      : ~15s(ns) ~15s(s)~n", [integer_to_binary(TSum), float_to_binary(TSum / 1000000000, [{decimals, 2}, compact])]),
	io:format("PAvgTime                      : ~15s(ns) ~15s(s)~n", [float_to_binary(float(AvgTime), [{decimals, 2}, compact]), float_to_binary(AvgTime / 1000000000, [{decimals, 2}, compact])]),
	io:format("FAvgTime                      : ~15s(ns) ~15s(s)~n", [float_to_binary(AvgTime / LoopTime, [{decimals, 2}, compact]), float_to_binary(AvgTime / LoopTime / 1000000000, [{decimals, 2}, compact])]),
	io:format("PMin process memory           : ~15s(bytes)~n", [float_to_binary(float(PMin), [{decimals, 2}, compact])]),
	io:format("PMax process memory           : ~15s(bytes)~n", [float_to_binary(float(PMax), [{decimals, 2}, compact])]),
	io:format("FAvg process memory           : ~15s(bytes)~n", [float_to_binary(float(AvgPMem), [{decimals, 2}, compact])]),
	io:format("PVmMin vm memory              : ~15s(bytes)~n", [float_to_binary(float(VmMin), [{decimals, 2}, compact])]),
	io:format("PvmMax vm memory              : ~15s(bytes)~n", [float_to_binary(float(VmMax), [{decimals, 2}, compact])]),
	io:format("FVmAvg vm memory              : ~15s(bytes)~n", [float_to_binary(float(AvgVmMem), [{decimals, 2}, compact])]),
	io:format("==========================================~n").

loopSpawn(0, _, _, _, _, _, AllPid) ->
	AllPid;
loopSpawn(ProcCnt, M, F, A, CollectorPid, LoopTime, AllPid) ->
	Pid = spawn_monitor(fun() -> worker(LoopTime, M, F, A, CollectorPid) end),
	loopSpawn(ProcCnt - 1, M, F, A, CollectorPid, LoopTime, [Pid | AllPid]).

startTest(TestType, AllPid) ->
	[OnePid ! TestType || {OnePid, _MRef} <- AllPid],
	ok.

%% 并发测试的进程发送完成信号后 数量为0的时候，可能收集函数就返回 但是最后正常关闭DOWN的消息还没来得及处理
clearDown() ->
	receive
		{'DOWN', _Reference, process, _Pid, _Reason} ->
			clearDown()
	after 1000 ->
		ok
	end.

tcCollector(0, 0, _Min, _Max, _Sum, _List) ->
	{0, 0, 0, 0, []};
tcCollector(0, ProcCnt, Min, Max, Sum, List) ->
	{ProcCnt, Min, Max, Sum, List};
tcCollector(Index, ProcCnt, Min, Max, Sum, List) ->
	receive
		{tcResult, Nanosecond} ->
			NewSum = Sum + Nanosecond,
			{NewMin, NewMax} = compare(Min, Max, Nanosecond),
			tcCollector(Index - 1, ProcCnt + 1, NewMin, NewMax, NewSum, [Nanosecond | List]);
		{'DOWN', _Reference, process, _Pid, Reason} ->
			case Reason of
				normal ->
					tcCollector(Index, ProcCnt, Min, Max, Sum, List);
				_ ->
					io:format("multiprocess tc has error ~0p~n", [Reason]),
					tcCollector(Index - 1, ProcCnt, Min, Max, Sum, List)
			end
	after 1800000 ->
		io:format("tc execute time out~n"),
		tcCollector(0, ProcCnt, Min, Max, Sum, List)
	end.

%% 多进程内存收集器
tmCollector(0, 0, _PMin, _PMax, _PSum, _VmMin, _VmMax, _VmSum) ->
	{0, 0, 0, 0, 0, 0, 0};
tmCollector(0, ProcCnt, PMin, PMax, PSum, VmMin, VmMax, VmSum) ->
	{ProcCnt, PMin, PMax, PSum, VmMin, VmMax, VmSum};
tmCollector(Index, ProcCnt, PMin, PMax, PSum, VmMin, VmMax, VmSum) ->
	receive
		{tmResult, RPSum, RVmSum} ->
			{NewPMin, NewPMax} = compare(PMin, PMax, RPSum),
			{NewVmMin, NewVmMax} = compare(VmMin, VmMax, RVmSum),
			tmCollector(Index - 1, ProcCnt + 1, NewPMin, NewPMax, PSum + RPSum, NewVmMin, NewVmMax, VmSum + RVmSum);
		{'DOWN', _Reference, process, _Pid, Reason} ->
			case Reason of
				normal ->
					tmCollector(Index, ProcCnt, PMin, PMax, PSum, VmMin, VmMax, VmSum);
				_ ->
					io:format("multiprocess tm has error ~0p~n", [Reason]),
					tmCollector(Index - 1, ProcCnt, PMin, PMax, PSum, VmMin, VmMax, VmSum)
			end
	after 1800000 ->
		io:format("tm execute time out~n"),
		tmCollector(0, ProcCnt, PMin, PMax, PSum, VmMin, VmMax, VmSum)
	end.

%% 多进程综合收集器
tmcCollector(0, 0, _PMin, _PMax, _PSum, _VmMin, _VmMax, _VmSum, _TMin, _TMax, _TSum) ->
	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
tmcCollector(0, ProcCnt, PMin, PMax, PSum, VmMin, VmMax, VmSum, TMin, TMax, TSum) ->
	{ProcCnt, PMin, PMax, PSum, VmMin, VmMax, VmSum, TMin, TMax, TSum};
tmcCollector(Index, ProcCnt, PMin, PMax, PSum, VmMin, VmMax, VmSum, TMin, TMax, TSum) ->
	receive
		{tmcResult, RPSum, RVmSum, RTimeSum} ->
			%% 更新时间统计
			{NewPMin, NewPMax} = compare(PMin, PMax, RPSum),
			{NewVmMin, NewVmMax} = compare(VmMin, VmMax, RVmSum),
			{NewTMin, NewTMax} = compare(TMin, TMax, RTimeSum),
			tmcCollector(Index - 1, ProcCnt + 1, NewPMin, NewPMax, PSum + RPSum, NewVmMin, NewVmMax, VmSum + RVmSum, NewTMin, NewTMax, TSum + RTimeSum);
		{'DOWN', _Reference, process, _Pid, Reason} ->
			case Reason of
				normal ->
					tmcCollector(Index, ProcCnt, PMin, PMax, PSum, VmMin, VmMax, VmSum, TMin, TMax, TSum);
				_ ->
					io:format("multiprocess tmc has error ~0p~n", [Reason]),
					tmcCollector(Index - 1, ProcCnt, PMin, PMax, PSum, VmMin, VmMax, VmSum, TMin, TMax, TSum)
			end
	after 1800000 ->
		io:format("tmc execute time out~n"),
		tmcCollector(0, ProcCnt, PMin, PMax, PSum, VmMin, VmMax, VmSum, TMin, TMax, TSum)
	end.

worker(LoopTime, M, F, A, CollectorPid) ->
	receive
		tcTest ->
			SumTime = loopMTc(LoopTime, M, F, A, 0),
			CollectorPid ! {tcResult, SumTime};
		tmTest ->
			%% 执行函数
			{_PMin, _PMax, PSum, _VmMin, _VmMax, VmSum} = loopMTm(LoopTime, M, F, A, 0, 0, 0, 0, 0, 0),
			%% 计算内存增量并发送结果
			CollectorPid ! {tmResult, PSum / LoopTime, VmSum / LoopTime};
		tmcTest ->
			{_PMin, _PMax, PSum, _VmMin, _VmMax, VmSum, TimeSum} = loopMTmc(LoopTime, M, F, A, 0, 0, 0, 0, 0, 0, 0),
			CollectorPid ! {tmcResult, PSum / LoopTime, VmSum / LoopTime, TimeSum}
	end.

%% 多进程内存测量循环
loopMTc(0, _, _, _, SumTime) ->
	SumTime;
loopMTc(LoopTime, M, F, A, SumTime) ->
	Nanosecond = doTc(M, F, A),
	loopMTc(LoopTime - 1, M, F, A, SumTime + Nanosecond).

%% 多进程内存测量循环
loopMTm(0, _, _, _, PMin, PMax, PSum, VmMax, VmMin, VmSum) ->
	{PMin, PMax, PSum, VmMin, VmMax, VmSum};
loopMTm(LoopTime, M, F, A, PMin, PMax, PSum, VmMin, VmMax, VmSum) ->
	Args = getArgs(A),
	%% 每次执行前强制垃圾回收，确保测量准确
	garbage_collect(),
	%% 测量执行前的内存
	{SPMem, SVmMem} = getMem(),
	%% 执行函数
	apply(M, F, Args),
	%% 测量执行后的内存
	{EPMem, EVmMem} = getMem(),
	%% 计算内存增量
	PUsed = EPMem - SPMem,
	VmUsed = EVmMem - SVmMem,
	
	NewPSum = PSum + PUsed,
	NewVmSum = VmSum + VmUsed,
	{NewPMin, NewPMax} = compare(PMin, PMax, PUsed),
	{NewVmMin, NewVmMax} = compare(VmMin, VmMax, VmUsed),
	loopMTm(LoopTime - 1, M, F, A, NewPMin, NewPMax, NewPSum, NewVmMin, NewVmMax, NewVmSum).

%% 多进程内存测量循环
loopMTmc(0, _, _, _, PMin, PMax, PSum, VmMin, VmMax, VmSum, TimeSum) ->
	{PMin, PMax, PSum, VmMin, VmMax, VmSum, TimeSum};
loopMTmc(LoopTime, M, F, A, PMin, PMax, PSum, VmMin, VmMax, VmSum, TimeSum) ->
	Args = getArgs(A),
	%% 每次执行前强制垃圾回收，确保测量准确
	garbage_collect(),
	%% 测量执行前的内存
	{SPMem, SVmMem} = getMem(),
	T1 = erlang:monotonic_time(),
	%% 执行函数
	apply(M, F, Args),
	T2 = erlang:monotonic_time(),
	%% 测量执行后的内存
	{EPMem, EVmMem} = getMem(),
	%% 计算时间
	TimeUsed = erlang:convert_time_unit(T2 - T1, native, nanosecond),
	%% 计算内存增量
	PUsed = EPMem - SPMem,
	VmUsed = EVmMem - SVmMem,
	{NewPMin, NewPMax} = compare(PMin, PMax, PUsed),
	{NewVmMin, NewVmMax} = compare(VmMin, VmMax, VmUsed),
	loopMTmc(LoopTime - 1, M, F, A, NewPMin, NewPMax, PSum + PUsed, NewVmMin, NewVmMax, VmSum + VmUsed, TimeUsed + TimeSum).

%% 单进程内存测量循环
loopSTc(0, _M, _F, _A, _LoopTime, Min, Max, Sum, List) ->
	{Min, Max, Sum, List};
loopSTc(Index, M, F, A, LoopTime, Min, Max, Sum, List) ->
	Nanosecond = doTc(M, F, A),
	NewSum = Sum + Nanosecond,
	{NewMin, NewMax} = compare(Min, Max, Nanosecond),
	loopSTc(Index - 1, M, F, A, LoopTime, NewMin, NewMax, NewSum, [Nanosecond | List]).

%% 单进程内存测量循环
loopSTm(0, _M, _F, _A, _LoopTime, PMin, PMax, PSum, VmMin, VmMax, VmSum) ->
	{PMin, PMax, PSum, VmMin, VmMax, VmSum};
loopSTm(Index, M, F, A, LoopTime, PMin, PMax, PSum, VmMin, VmMax, VmSum) ->
	Args = getArgs(A),
	%% 每次执行前强制垃圾回收，确保测量准确
	garbage_collect(),
	%% 测量执行前的内存
	{SPMem, SVmMem} = getMem(),
	%% 执行函数
	apply(M, F, Args),
	%% 测量执行后的内存
	{EPMem, EVmMem} = getMem(),
	%% 计算内存增量
	PUsed = EPMem - SPMem,
	VmUsed = EVmMem - SVmMem,
	{NewPMin, NewPMax} = compare(PMin, PMax, PUsed),
	{NewVmMin, NewVmMax} = compare(VmMin, VmMax, VmUsed),
	loopSTm(Index - 1, M, F, A, LoopTime, NewPMin, NewPMax, PSum + PUsed, NewVmMin, NewVmMax, VmSum + VmUsed).

%% 单进程综合测量循环
loopSTmc(0, _M, _F, _A, _LoopTime, PMin, PMax, PSum, VmMin, VmMax, VmSum, TMin, TMax, TSum) ->
	{PMin, PMax, PSum, VmMin, VmMax, VmSum, TMin, TMax, TSum};
loopSTmc(Index, M, F, A, LoopTime, PMin, PMax, PSum, VmMin, VmMax, VmSum, TMin, TMax, TSum) ->
	Args = getArgs(A),
	%% 每次执行前强制垃圾回收，确保测量准确
	garbage_collect(),
	%% 测量执行前的内存
	{SPMem, SVmMem} = getMem(),
	%% 测量执行时间
	T1 = erlang:monotonic_time(),
	apply(M, F, Args),
	T2 = erlang:monotonic_time(),
	%% 测量执行后的内存
	{EPMem, EVmMem} = getMem(),
	TimeUsed = erlang:convert_time_unit(T2 - T1, native, nanosecond),
	%% 计算内存增量
	PUsed = EPMem - SPMem,
	VmUsed = EVmMem - SVmMem,
	{NewPMin, NewPMax} = compare(PMin, PMax, PUsed),
	{NewVmMin, NewVmMax} = compare(VmMin, VmMax, VmUsed),
	{NewTMin, NewTMax} = compare(TMin, TMax, TimeUsed),
	loopSTmc(Index - 1, M, F, A, LoopTime, NewPMin, NewPMax, PSum + PUsed, NewVmMin, NewVmMax, VmSum + VmUsed, NewTMin, NewTMax, TSum + TimeUsed).

%% 获取执行时间
doTc(M, F, A) ->
	Args = getArgs(A),
	T1 = erlang:monotonic_time(),
	apply(M, F, Args),
	T2 = erlang:monotonic_time(),
	erlang:convert_time_unit(T2 - T1, native, nanosecond).

getArgs(Fun) when is_function(Fun) -> Fun();
getArgs({Fun, Args}) when is_function(Fun) -> apply(Fun, Args);
getArgs({M, F, Args}) -> apply(M, F, Args);
getArgs(A) when is_list(A) -> A.

formatArgs(Fun) when is_function(Fun) ->
	iolist_to_binary(io_lib:format("~0p", [Fun], [{chars_limit, 80}]));
formatArgs({Fun, Args}) when is_function(Fun) ->
	case Args of [] -> ArgsStr = <<>>; _ -> <<_:16, ArgsStr/binary>> = <<<<", ", (iolist_to_binary(io_lib:format("~0p", [OArg], [{chars_limit, 80}])))/binary>> || OArg <- Args>> end,
	<<(iolist_to_binary(io_lib:format("~0p(", [Fun], [{chars_limit, 80}])))/binary, ArgsStr/binary, ")">>;
formatArgs({M, F, Args}) ->
	case Args of [] -> ArgsStr = <<>>; _ -> <<_:16, ArgsStr/binary>> = <<<<", ", (iolist_to_binary(io_lib:format("~0p", [OArg], [{chars_limit, 80}])))/binary>> || OArg <- Args>> end,
	<<(iolist_to_binary(io_lib:format("~0p:~0p(", [M, F], [{chars_limit, 80}])))/binary, ArgsStr/binary, ")">>;
formatArgs(Args) when is_list(Args) ->
	case Args of [] -> ArgsStr = <<>>; _ -> <<_:16, ArgsStr/binary>> = <<<<", ", (iolist_to_binary(io_lib:format("~0p", [OArg], [{chars_limit, 80}])))/binary>> || OArg <- Args>> end,
	ArgsStr.

%% 获取当前进程的内存使用情况
getMem() ->
	MemoryInfo = erlang:memory(),
	VmMem = proplists:get_value(total, MemoryInfo, 0),
	{memory, ProcessMem} = erlang:process_info(self(), memory),
	{ProcessMem, VmMem}.

compare(none, none, Value) -> {Value, Value};
compare(Min, Max, Value) ->
	{case Min > Value of true -> Value; _ -> Min end, case Max < Value of true -> Value; _ -> Max end}.

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

%% 计算百分位数
percentiles([], _Percentiles) -> [0, 0, 0, 0];
percentiles(List, Percentiles) ->
	Sorted = lists:sort(List),
	Length = length(Sorted),
	[percentile(Sorted, Length, P) || P <- Percentiles].

percentile(Sorted, Length, Percentile) when Percentile >= 0, Percentile =< 100 ->
	Index = max(1, round(Percentile / 100 * Length)),
	lists:nth(Index, Sorted).

testTime(Time) -> timer:sleep(Time).
testOk() -> ok.
testMem(Bytes) ->
	W = erlang:system_info(wordsize),
	Cells = trunc((Bytes - W) / (2 * W)),
	lists:duplicate(Cells, 0).

testCrash() ->
	exit(test).
