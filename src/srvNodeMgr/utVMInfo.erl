-module(utVMInfo).

-include("utComMisc.hrl").

-export([
	vmInfo/0							%% 打印虚拟机简略信息
	, processInfo/1				%% 打印进程简略信息
	, processMqlInfo/0			%% 消息队列阻塞的进程信息
	, processMqlInfo/1			%% 消息队列阻塞的进程信息
	, showEtsCache/0				%% 打印并排序各个表的缓存消耗
	, processHeapSize/0			%% 进程消耗内存的信息
	, processCnt/0					%% 当前进程数量
	, pstack/1						%% 进程栈
	, gcAll/0						%% 对所有process做gc 进程内存过高时，来一发，看看是内存泄露还是gc不过来
	, gcOne/1						%% 对指定process做gc 进程内存过高时，来一发，看看是内存泄露还是gc不过来
	, scheduler_usage/0			%% 统计下1s每个调度器CPU的实际利用率(因为有spin wait、调度工作, 可能usage 比top显示低很多)
	, scheduler_usage/1			%% 统计下1s每个调度器CPU的实际利用率(因为有spin wait、调度工作, 可能usage 比top显示低很多)\
	, scheduler_stat/0			%% 统计下1s内调度进程数量(含义：第一个数字执行进程数量，第二个数字迁移进程数量)
	, scheduler_stat/1			%% 统计下1s内调度进程数量(含义：第一个数字执行进程数量，第二个数字迁移进程数量)

	, memInfoInit/2
	, memInfoPrint/3
	, test/0
]).

%% 打印虚拟机简略信息
vmInfo() ->
	io:format("abormal termination:
        ~n   Scheduler id:                         ~p
        ~n   Num scheduler:                        ~p
        ~n   Process count:                        ~p
        ~n   Process limit:                        ~p
        ~n   Memory used by erlang processes:      ~p
        ~n   Memory allocated by erlang processes: ~p
        ~n   The total amount of memory allocated: ~p
        ~n",
		[erlang:system_info(scheduler_id), erlang:system_info(schedulers), erlang:system_info(process_count), erlang:system_info(process_limit), erlang:memory(processes_used), erlang:memory(processes), erlang:memory(total)]),
	ok.

%% 打印进程简略信息
processInfo(Pid) ->
	io:format("~n~n=====process info===~n~p~n~p~n~p~n~p~n~p~n~p~n~p~n~p~n~p~n~p~n~n~n",
		[
			Pid,
			erlang:process_info(Pid, registered_name),
			erlang:process_info(Pid, current_function),
			erlang:process_info(Pid, message_queue_len),
			erlang:process_info(Pid, status),
			erlang:process_info(Pid, suspending),
			erlang:process_info(Pid, last_calls),
			erlang:process_info(Pid, links),
			erlang:process_info(Pid, dictionary),
			erlang:process_info(Pid, current_stacktrace)
		]).

%% 消息队列阻塞的进程信息
processMqlInfo() ->
	processMqlInfo(100).

processMqlInfo(Num) ->
	[processMqlInfo(Pid, Num) ||  Pid <- erlang:processes()],
	ok.

%% 消息队列阻塞的进程信息
processMqlInfo(Pid, Num) ->
	case erlang:process_info(Pid, message_queue_len) of
		{message_queue_len, Count} when Count > Num ->
			processInfo(Pid);
		_ ->
			ok
	end.

%% 打印并排序各个表的缓存消耗
showEtsCache() ->
	io:format("table name | memory | size~n", []),
	lists:reverse(lists:keysort(2, [{T, ets:info(T, memory), ets:info(T, size)} || T <- ets:all()])).

%% 进程消耗内存排序的信息
processHeapSize() ->
	lists:reverse(lists:keysort(2, [{erlang:process_info(P, registered_name), erlang:process_info(P, heap_size)} || P <- erlang:processes()])).

%% 当前进程数量
processCnt() ->
	erlang:system_info(process_count).

%% 进程栈 类似于jstack，发现大量进程挂起，进程数过高，运行慢，hang住等问题用到
pstack(Reg) when is_atom(Reg) ->
	case is_atom(Reg) andalso whereis(Reg) of
		false -> io:format("~s~n", [element(2, erlang:process_info(Reg, backtrace))]);
		undefined ->
			undefined;
		Pid -> io:format("~s~n", [element(2, erlang:process_info(Pid, backtrace))])
	end.

%% 对所有process做gc 进程内存过高时，来一发，看看是内存泄露还是gc不过来
gcAll() ->
	[erlang:garbage_collect(Pid) || Pid <- processes()],
	ok.

%% 对指定process做gc 进程内存过高时，来一发，看看是内存泄露还是gc不过来
gcOne(Pid) ->
	erlang:garbage_collect(Pid),
	ok.

%% scheduler usage
%% 统计下1s每个调度器CPU的实际利用率(因为有spin wait、调度工作, 可能usage 比top显示低很多)
scheduler_usage() ->
	scheduler_usage(1000).

scheduler_usage(RunMs) ->
	erlang:system_flag(scheduler_wall_time, true),
	Ts0 = lists:sort(erlang:statistics(scheduler_wall_time)),
	timer:sleep(RunMs),
	Ts1 = lists:sort(erlang:statistics(scheduler_wall_time)),
	erlang:system_flag(scheduler_wall_time, false),
	Cores = lists:map(fun({{_I, A0, T0}, {I, A1, T1}}) ->
		{I, (A1 - A0) / (T1 - T0)} end, lists:zip(Ts0, Ts1)),
	{A, T} = lists:foldl(fun({{_, A0, T0}, {_, A1, T1}}, {Ai,Ti}) ->
		{Ai + (A1 - A0), Ti + (T1 - T0)} end, {0, 0}, lists:zip(Ts0, Ts1)),
	Total = A/T,
	io:format("~p~n", [[{total, Total} | Cores]]).

%% 进程调度
%% 统计下1s内调度进程数量(含义：第一个数字执行进程数量，第二个数字迁移进程数量)
scheduler_stat() ->
	scheduler_stat(1000).

scheduler_stat(RunMs) ->
	erlang:system_flag(scheduling_statistics, enable),
	Ts0 = erlang:system_info(total_scheduling_statistics),
	timer:sleep(RunMs),
	Ts1 = erlang:system_info(total_scheduling_statistics),
	erlang:system_flag(scheduling_statistics, disable),
	lists:map(fun({{_Key, In0, Out0}, {Key, In1, Out1}}) ->
		{Key, In1 - In0, Out1 - Out0} end, lists:zip(Ts0, Ts1)).



%% 内存高OOM 排查工具
%% etop 无法应对10w+ 进程节点, 下面代码就没问题了；找到可疑proc后通过pstack、message_queu_len 排查原因
proc_mem_all(SizeLimitKb) ->
	Procs = [{undefined, Pid} || Pid<- erlang:processes()],
	proc_mem(Procs, SizeLimitKb).

proc_mem(SizeLimitKb) ->
	Procs = [{Name, Pid} || {_, Name, Pid, _} <- release_handler_1:get_supervised_procs(),
		is_process_alive(Pid)],
	proc_mem(Procs, SizeLimitKb).

proc_mem(Procs, SizeLimitKb) ->
	SizeLimit = SizeLimitKb * 1024,
	{R, Total} = lists:foldl(fun({Name, Pid}, {Acc, TotalSize}) ->
		case erlang:process_info(Pid, total_heap_size) of
			{_, Size0} ->
				Size = Size0*8,
				case Size > SizeLimit of
					true -> {[{Name, Pid, Size} | Acc], TotalSize+Size};
					false -> {Acc, TotalSize}
				end;
			_ -> {Acc, TotalSize}
		end
							 end, {[], 0}, Procs),
	R1 = lists:keysort(3, R),
	{Total, lists:reverse(R1)}.

show(N) ->
	F = fun(P) ->
		case catch process_info(P, [memory, dictionary]) of
			[{_, Memory}, {_, Dict}] ->
				InitStart = util:prop_get_value('$initial_call', Dict, null),
				{InitStart, Memory};
			_ -> {null, 0}
		end
		 end,
	Infos1 = lists:map(F, processes()),
	Infos2 = [{Name, M} || {Name, M} <- Infos1, Name =/= null],
	SortFun = fun({_, M1}, {_, M2}) -> M1 > M2 end,
	Infos3 = lists:sort(SortFun, Infos2),
	Infos4 = lists:sublist(Infos3, N),
	[io:format("~p : ~p ~n", [Name, M]) || {Name, M} <- Infos4],
	ok.

show(N, SkipNames) ->
	F = fun(P) ->
		case catch process_info(P, [memory, dictionary]) of
			[{_, Memory}, {_, Dict}] ->
				InitStart = util:prop_get_value('$initial_call', Dict, null),
				case catch tuple_to_list(InitStart) of
					[Name | _] ->
						case lists:member(Name, SkipNames) of
							true -> {null, 0};
							false -> {InitStart, Memory}
						end;
					_ -> {null, 0}
				end;
			_ -> {null, 0}
		end
		 end,
	Infos1 = lists:map(F, processes()),
	Infos2 = [{Name, M} || {Name, M} <- Infos1, Name =/= null],
	SortFun = fun({_, M1}, {_, M2}) -> M1 > M2 end,
	Infos3 = lists:sort(SortFun, Infos2),
	Infos4 = lists:sublist(Infos3, N),
	[io:format("~p : ~p ~n", [Name, M]) || {Name, M} <- Infos4],
	ok.

show1(N) ->
	F = fun(P, Acc) ->
		case catch process_info(P, [memory, dictionary]) of
			[{_, Memory}, {_, Dict}] ->
				InitStart = util:prop_get_value('$initial_call', Dict, null),
				case lists:keyfind(InitStart, 1, Acc) of
					false -> [{InitStart, Memory, 1} | Acc];
					{InitStart, Memory1, Num} -> lists:keystore(InitStart, 1, Acc, {InitStart, Memory + Memory1, Num + 1})
				end;
			_ -> Acc
		end
		 end,
	Infos1 = lists:foldl(F, [], processes()),
	Infos2 = [{Name, M, Num} || {Name, M, Num} <- Infos1, Name =/= null],
	SortFun = fun({_, M1, _}, {_, M2, _}) -> M1 > M2 end,
	Infos3 = lists:sort(SortFun, Infos2),
	Infos4 = lists:sublist(Infos3, N),
	[io:format("~p : per_memory=~p process_num=~p ~n", [Name, (M div Num), Num]) || {Name, M, Num} <- Infos4],
	ok.

%% 得到CPU核数
coreCnt() ->
	erlang:system_info(schedulers).

%% 获取当前进程运行的核id
coreIndex() ->
	erlang:system_info(scheduler_id).


%% @doc 节点所有进程信息
process_infos() ->
	filelib:ensure_dir("./logs/"),
	File = "./logs/processes_infos.log",
	{ok, Fd} = file:open(File, [write, raw, binary, append]),
	Fun = fun(Pi) ->
		Info = io_lib:format("=>~p \n\n", [Pi]),
		case filelib:is_file(File) of
			true -> file:write(Fd, Info);
			false ->
				file:close(Fd),
				{ok, NewFd} = file:open(File, [write, raw, binary, append]),
				file:write(NewFd, Info)
		end,
		timer:sleep(20)
			end,
	[Fun(erlang:process_info(P)) || P <- erlang:processes()].

rfc1123_local_date() ->
	rfc1123_local_date(os:timestamp()).
rfc1123_local_date({A, B, C}) ->
	rfc1123_local_date(calendar:now_to_local_time({A, B, C}));
rfc1123_local_date({{YYYY, MM, DD}, {Hour, Min, Sec}}) ->
	DayNumber = calendar:day_of_the_week({YYYY, MM, DD}),
	lists:flatten(
		io_lib:format("~s, ~2.2.0w ~3.s ~4.4.0w ~2.2.0w:~2.2.0w:~2.2.0w GMT",
			[httpd_util:day(DayNumber), DD, httpd_util:month(MM), YYYY, Hour, Min, Sec]));
rfc1123_local_date(Epoch) when erlang:is_integer(Epoch) ->
	rfc1123_local_date(calendar:gregorian_seconds_to_datetime(Epoch + 62167219200)).

%% @doc erlang_dump
crash_dump() ->
	Date = erlang:list_to_binary(rfc1123_local_date()),
	Header = binary:list_to_bin([<<"=erl_crash_dump:0.2\n">>, Date, <<"\nSystem version: ">>]),
	Ets = ets_info(),
	Report = binary:list_to_bin([Header, erlang:list_to_binary(erlang:system_info(system_version)),
		erlang:system_info(info), erlang:system_info(procs), Ets, erlang:system_info(dist),
		<<"=loaded_modules\n">>, binary:replace(erlang:system_info(loaded),
			<<"\n">>, <<"\n=mod:">>, [global])]),
	file:write_file("erl_crash.dump", Report).

ets_info() ->
	binary:list_to_bin([ets_table_info(T) || T <- ets:all()]).

ets_table_info(Table) ->
	Info = ets:info(Table),
	Owner = erlang:list_to_binary(erlang:pid_to_list(proplists:get_value(owner, Info))),
	TableN = erlang:list_to_binary(erlang:atom_to_list(proplists:get_value(name, Info))),
	Name = erlang:list_to_binary(erlang:atom_to_list(proplists:get_value(name, Info))),
	Objects = erlang:list_to_binary(erlang:integer_to_list(proplists:get_value(size, Info))),
	binary:list_to_bin([<<"=ets:">>, Owner, <<"\nTable: ">>, TableN, <<"\nName: ">>, Name,
		<<"\nObjects: ">>, Objects, <<"\n">>]).

%% 检查溢出的内存，强制gc, 并写入日志分析
check_mem(MemLim) ->
	lists:foreach(
		fun(P) ->
			case is_pid(P) andalso erlang:is_process_alive(P) of
				true ->
					{memory, Mem} = erlang:process_info(P, memory),
					case Mem > MemLim of
						true ->
							erlang:garbage_collect(P);
						false ->
							[]
					end;
				false ->
					[]
			end
		end, erlang:processes()).

%% @spec top() -> ok
%% @doc 查看系统当前的综合信息
top() ->
	Release = erlang:system_info(otp_release),
	SchedNum = erlang:system_info(schedulers),
	ProcCount = erlang:system_info(process_count),
	ProcLimit = erlang:system_info(process_limit),
	ProcMemUsed = erlang:memory(processes_used),
	EtsMemAlc = erlang:memory(ets),
	MemTot = erlang:memory(total),
	%PetNum       = all_pets(),
	io:format(
		"++++++++++++++++++++++++++++++++++++++++++~n"
		"   Node:                             ~p~n"
		"   Erlang Ver:                       ~p~n"
		"   Free Threads:                     ~p~n"
		"   Process Used Memory:              ~pMb~n"
		"   Ets Used Memory:                  ~pMb~n"
		"   Erlang VM Used Memory:            ~pMb~n"
		"   Process Limit:                    ~p~n"
		"   Process Used:                     ~p~n"
		"++++++++++++++++++++++++++++++++++++++++++~n"
		, [node(), Release, SchedNum, ProcMemUsed / 1024 / 1024, EtsMemAlc / 1024 / 1024, MemTot / 1024 / 1024, ProcLimit, ProcCount]),
	ok.

%% @doc 运维要用
top_back() ->
	Release = erlang:system_info(otp_release),
	SchedNum = erlang:system_info(schedulers),
	ProcCount = erlang:system_info(process_count),
	ProcLimit = erlang:system_info(process_limit),
	ProcMemUsed = erlang:memory(processes_used),
	EtsMemAlc = erlang:memory(ets),
	MemTot = erlang:memory(total),
	Str = io_lib:format(
		"   Erlang 版本:                          ~p~n"
		"   可使用的调度线程:                     ~p~n"
		"   所有进程使用的内存:                   ~pMb~n"
		"   所有ets使用的内存:                    ~pMb~n"
		"   Erlang系统占用内存:                   ~pMb~n"
		"   可创建进程数量上限:                   ~p~n"
		"   当前进程数:                           ~p~n"
		, [Release, SchedNum, ProcMemUsed / 1024 / 1024, EtsMemAlc / 1024 / 1024, MemTot / 1024 / 1024, ProcLimit, ProcCount]),
	binary_to_list(list_to_binary(Str)).

%% @spec ets_mem() -> term()
%% @doc 查看内存占用最多的30张ets表
ets_mem() ->
	L = ets:all(),
	Mems = lists:map(fun(Tab) ->
		Info = ets:info(Tab),
		case lists:keyfind(memory, 1, Info) of
			{memory, Mem} -> {Tab, Mem};
			_ -> {Tab, 0}
		end
						  end, L),
	L1 = lists:sublist(lists:reverse(lists:keysort(2, Mems)), 30),
	io:format("~n--------------------------------------------------~n"
	"~-30w ~w~n--------------------------------------------------~n"
		, [table, used_memory]),
	lists:foreach(
		fun({Tab, Mem}) ->
			io:format("~-30w ~wKb~n", [Tab, Mem / 1024])
		end, L1).




%% @doc 备份进程信息
dump_process_info(Pid) ->
	{{Year, Month, Day}, {Hour, Minutes, Second}} = util:local_time(),
	{ok, FileHandle} = file:open(util:fbin("~s-~w-~w-~w-~w-~w-~w", [<<"../logs/pid_info.dump">>, Year, Month, Day, Hour, Minutes, Second]), write),
	case erlang:process_info(Pid) of
		Info when is_list(Info) ->
			lists:foreach(fun({messages, Messages}) ->
				case Messages =:= [] of
					true ->
						io:format(FileHandle, "~w~n", [{messages, Messages}]);
					_ ->
						io:format(FileHandle, "{messages,~n", []),
						lists:foreach(fun(M) ->
							io:format(FileHandle, "  ~w~n", [M])
										  end, Messages),
						io:format(FileHandle, "}~n", [])
				end;
				({dictionary, Dics}) ->
					case Dics =:= [] of
						true ->
							io:format(FileHandle, "~w~n", [{dictionary, Dics}]);
						_ ->
							io:format(FileHandle, "{dictionary,~n", []),
							lists:foreach(fun(M) ->
								io:format(FileHandle, "  ~w~n", [M])
											  end, Dics),
							io:format(FileHandle, "}~n", [])
					end;
				(E) ->
					io:format(FileHandle, "~w~n", [E])
							  end, Info);
		_ ->
			io:format("not find pid info")
	end,
	file:close(FileHandle).

get_process_info_and_zero_value(InfoName) ->
	PList = erlang:processes(),
	ZList = lists:filter(
		fun(T) ->
			case erlang:process_info(T, InfoName) of
				{InfoName, 0} -> false;
				_ -> true
			end
		end, PList),
	ZZList = lists:map(
		fun(T) -> {T, erlang:process_info(T, InfoName), erlang:process_info(T, registered_name)}
		end, ZList),
	[length(PList), InfoName, length(ZZList), ZZList].

get_process_info_and_large_than_value(InfoName, Value) ->
	PList = erlang:processes(),
	ZList = lists:filter(
		fun(T) ->
			case erlang:process_info(T, InfoName) of
				{InfoName, VV} ->
					if VV > Value -> true;
						true -> false
					end;
				_ -> true
			end
		end, PList),
	ZZList = lists:map(
		fun(T) -> {T, erlang:process_info(T, InfoName), erlang:process_info(T, registered_name)}
		end, ZList),
	[length(PList), InfoName, Value, length(ZZList), ZZList].

get_msg_queue() ->
	io:fwrite("process count:~p~n~p value is not 0 count:~p~nLists:~p~n",
		get_process_info_and_zero_value(message_queue_len)).

get_memory() ->
	io:fwrite("process count:~p~n~p value is large than ~p count:~p~nLists:~p~n",
		get_process_info_and_large_than_value(memory, 1048576)).

get_memory(Value) ->
	io:fwrite("process count:~p~n~p value is large than ~p count:~p~nLists:~p~n",
		get_process_info_and_large_than_value(memory, Value)).

get_heap() ->
	io:fwrite("process count:~p~n~p value is large than ~p count:~p~nLists:~p~n",
		get_process_info_and_large_than_value(heap_size, 1048576)).

get_heap(Value) ->
	io:fwrite("process count:~p~n~p value is large than ~p count:~p~nLists:~p~n",
		get_process_info_and_large_than_value(heap_size, Value)).

get_processes() ->
	io:fwrite("process count:~p~n~p value is large than ~p count:~p~nLists:~p~n",
		get_process_info_and_large_than_value(memory, 0)).

memInfoInit(CurModule, CurLine) ->
	erlang:put(?pdMemInfo, {CurModule, CurLine, erlang:system_time(nanosecond), recon:info(self(), memory_used), erlang:memory()}).

memInfoPrint(CurModule, CurLine, Threshold) ->
	case erlang:get(?pdMemInfo) of
		undefined ->
			erlang:put(?pdMemInfo, {CurModule, CurLine, erlang:system_time(nanosecond), recon:info(self(), memory_used), erlang:memory()});
		{OldModule, OldLine, OldTime, OldMemInfo, OldSumInfo} ->
			CurMemInfo = recon:info(self(), memory_used),
			CurTime = erlang:system_time(nanosecond),
			CurSumInfo = erlang:memory(),
			erlang:put(?pdMemInfo, {CurModule, CurLine, CurTime, CurMemInfo, CurSumInfo}),
			SubPid = element(2, lists:nth(1, element(2, CurMemInfo))) - element(2, lists:nth(1, element(2, OldMemInfo))),
			SubSum = element(2, lists:keyfind(total, 1, CurSumInfo)) - element(2, lists:keyfind(total, 1, OldSumInfo)),
			case erlang:abs(SubSum) >= Threshold orelse erlang:abs(SubPid) >= Threshold of
				true ->
					io:format(
						"IMY*********Memory use changes are too large:~n"
						"addOrSubSum:~20w~n"
						"addOrSubPid:~20w~n"
						"usedTimeDiff:~19w~n"
						"oldLine:~w~n"
						"CurLine:~w~n"
						"&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&\n"
						"OldSumInfo:~w~n"
						"CurSumInfo:~w~n"
						"&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&\n"
						"OldPidInfo:~p~n"
						"************************************************************************************~n"
						"CurPidInfo:~p~n", [SubSum, SubPid, CurTime - OldTime, {old, OldModule, OldLine, OldTime}, {cur, CurModule, CurLine, CurTime}, OldSumInfo, CurSumInfo, OldMemInfo, CurMemInfo]);
				_ ->
					ignore
			end
	end.


test() ->
	?MII(),
	?MIP(0),
	?MIP(0),
	?MIP(0).





