%% minitop, etop的简化版
%% 使用方法和etop类似，但参数只剩下 lines 和 sort
%% all 	：除了etop的内容以外，还会加上 具体进程的 erlang:process_info 信息
%% top ：输出的功能和etop一样
%% stack: etop的信息和堆栈信息
%% messages: etop的信息和进程消息队列的信息
%% option: etop的信息和指定的erlang:process_info信息

-module(mtop).
-export([all/0, all/1]).
-export([top/0, top/1]).
-export([stack/0, stack/1]).
-export([option/1, option/2]).
-export([messages/0, messages/1]).

-define(SYSFORM,
   " ~-72w~10s~n"
   " Load:  cpu  ~8w               Memory:  total    ~8w    binary   ~8w~n"
   "        procs~8w                        processes~8w    code     ~8w~n"
   "        runq ~8w                        atom     ~8w    ets      ~8w~n").

-define(PROCFORM, "~-15w~-20s~8w~8w~8w~8w ~-20s~n").

-record(mtop_info, {
   now = {0, 0, 0},
   n_procs = 0,
   wall_clock = {0, 0},
   runtime = {0, 0},
   run_queue = 0,
   alloc_areas = [],
   memi = [{total, 0}, {processes, 0}, {ets, 0}, {atom, 0}, {code, 0}, {binary, 0}],
   procinfo = []
}).

-record(mtop_proc_info, {
   pid,
   mem = 0,
   reds = 0,
   name,
   runtime = 0,
   cf,
   mq = 0
}).

-record(opts, {lines = 10, sort = msg_q}).


make_info(ProcInfo) ->
   #mtop_info{now = utTime:now(),
      n_procs = length(ProcInfo),
      run_queue = erlang:statistics(run_queue),
      wall_clock = erlang:statistics(wall_clock),
      runtime = erlang:statistics(runtime),
      memi = mtop_memi(),
      procinfo = ProcInfo
   }.

all() ->
   all([{lines, 10}, {sort, msg_q}]).
all(Opts) ->
   Config = handle_args(init:get_arguments() ++ Opts, #opts{}),
   ProcInfo = get_infos(Config),
   Info = make_info(ProcInfo),
   print_infos_all(standard_io, Info).


top() ->
   top([{lines, 10}, {sort, msg_q}]).

top(Opts) ->
   Config = handle_args(init:get_arguments() ++ Opts, #opts{}),
   ProcInfo = get_infos(Config),
   Info = make_info(ProcInfo),
   print_infos_top(standard_io, Info).

stack() ->
   stack([{lines, 10}, {sort, msg_q}]).

stack(Opts) ->
   Config = handle_args(init:get_arguments() ++ Opts, #opts{}),
   ProcInfo = get_infos(Config),
   Info = make_info(ProcInfo),
   print_infos_stack(standard_io, Info).

option(OptionList) ->
   option([{lines, 10}, {sort, msg_q}], OptionList).

option(Opts, OptionList) ->
   Config = handle_args(init:get_arguments() ++ Opts, #opts{}),
   ProcInfo = get_infos(Config),
   Info = make_info(ProcInfo),
   print_infos_option(standard_io, Info, OptionList).

messages() ->
   messages([{lines, 10}, {sort, msg_q}]).

messages(Opts) ->
   option(Opts, [messages]).


mtop_memi() ->
   try
      [{total, c:memory(total)},
         {processes, c:memory(processes)},
         {ets, c:memory(ets)},
         {atom, c:memory(atom)},
         {code, c:memory(code)},
         {binary, c:memory(binary)}]
   catch
      error:notsup ->
         undefined
   end.

get_infos(Opts) ->
   ProcInfo = mtop_collect(),
   ProcInfo1 = lists:map(fun(PI) -> PI#mtop_proc_info{runtime = '-'} end, ProcInfo),
   sort(Opts, ProcInfo1).

mtop_collect() ->
   mtop_collect(processes(), []).

mtop_collect([P | Ps], Acc) when P =:= self() ->
   mtop_collect(Ps, Acc);
mtop_collect([P | Ps], Acc) ->
   Fs = [registered_name, initial_call, memory, reductions, current_function, message_queue_len],
   case process_info(P, Fs) of
      undefined ->
         mtop_collect(Ps, Acc);
      [{registered_name, Reg}, {initial_call, Initial}, {memory, Mem},
         {reductions, Reds}, {current_function, Current}, {message_queue_len, Qlen}] ->
         Name = case Reg of
                   [] -> Initial;
                   _ -> Reg
                end,
         Info = #mtop_proc_info{pid = P, mem = Mem, reds = Reds, name = Name,
            cf = Current, mq = Qlen},
         mtop_collect(Ps, [Info | Acc])
   end;
mtop_collect([], Acc) -> Acc.

sort(Opts, PI) ->
   Tag = get_tag(Opts#opts.sort),
   PI1 = lists:reverse(lists:keysort(Tag, PI)),
   lists:sublist(PI1, Opts#opts.lines).

get_tag(runtime) -> #mtop_proc_info.runtime;
get_tag(memory) -> #mtop_proc_info.mem;
get_tag(reductions) -> #mtop_proc_info.reds;
get_tag(msg_q) -> #mtop_proc_info.mq.


print_infos_top(Fd, Info) ->
   {Cpu, NProcs, RQ, Clock} = loadinfo(Info),
   io:nl(Fd),
   writedoubleline(Fd),
   case Info#mtop_info.memi of
      undefined ->
         io:fwrite(Fd, " ~-72w~10s~n"
         " Load:  cpu  ~8w~n"
         "        procs~8w~n"
         "        runq ~8w~n",
            [node(), Clock,
               Cpu, NProcs, RQ]);
      Memi ->
         [Tot, Procs, Atom, Bin, Code, Ets] =
            meminfo(Memi, [total, processes, atom, binary, code, ets]),
         io:fwrite(Fd, ?SYSFORM,
            [node(), Clock,
               Cpu, Tot, Bin,
               NProcs, Procs, Code,
               RQ, Atom, Ets])
   end,
   io:nl(Fd),
   writepinfo_header(Fd),
   writesingleline(Fd),
   writepinfo(Fd, Info#mtop_info.procinfo),
   writedoubleline(Fd),
   io:nl(Fd).

writepinfo_header(Fd) ->
   io:fwrite(Fd, "Pid            Name or Initial Func    Time    Reds  Memory    MsgQ Current Function~n", []).

writesingleline(Fd) ->
   io:fwrite(Fd, "----------------------------------------------------------------------------------------~n", []).
writedoubleline(Fd) ->
   io:fwrite(Fd, "========================================================================================~n", []).

formatmfa({M, F, A}) ->
   io_lib:format("~w:~w/~w", [M, F, A]).

to_list(Name) when is_atom(Name) -> atom_to_list(Name);
to_list({_M, _F, _A} = MFA) -> formatmfa(MFA).

writepinfo(Fd, [#mtop_proc_info{pid = Pid,
   mem = Mem,
   reds = Reds,
   name = Name,
   runtime = Time,
   cf = MFA,
   mq = MQ}
   | T]) ->
   io:fwrite(Fd, ?PROCFORM, [Pid, to_list(Name), Time, Reds, Mem, MQ, formatmfa(MFA)]),
   writepinfo(Fd, T);
writepinfo(_Fd, []) ->
   ok.


loadinfo(SysI) ->
   #mtop_info{n_procs = Procs,
      run_queue = RQ,
      now = Now,
      wall_clock = {_, WC},
      runtime = {_, RT}} = SysI,
   Cpu = round(100 * RT / WC),
   Clock = io_lib:format("~2.2.0w:~2.2.0w:~2.2.0w",
      tuple_to_list(element(2, calendar:now_to_datetime(Now)))),
   {Cpu, Procs, RQ, Clock}.

meminfo(MemI, [Tag | Tags]) ->
   [round(get_mem(Tag, MemI) / 1024) | meminfo(MemI, Tags)];
meminfo(_MemI, []) -> [].

get_mem(Tag, MemI) ->
   case lists:keysearch(Tag, 1, MemI) of
      {value, {Tag, I}} -> I;                %these are in bytes
      _ -> 0
   end.

handle_args([{lines, Lines} | R], Config) when is_integer(Lines) ->
   NewC = Config#opts{lines = Lines},
   handle_args(R, NewC);
handle_args([{lines, [Lines]} | R], Config) when is_list(Lines) ->
   NewC = Config#opts{lines = list_to_integer(Lines)},
   handle_args(R, NewC);
handle_args([{sort, Sort} | R], Config) when is_atom(Sort) ->
   NewC = Config#opts{sort = Sort},
   handle_args(R, NewC);
handle_args([{sort, [Sort]} | R], Config) when is_list(Sort) ->
   NewC = Config#opts{sort = list_to_atom(Sort)},
   handle_args(R, NewC);

handle_args([_ | R], C) ->
   handle_args(R, C);
handle_args([], C) ->
   C.

print_infos_all(Fd, Info) ->
   print_infos_top(Fd, Info),
   writeprocessinfo(Fd, Info#mtop_info.procinfo).

writebt(Fd, Pid) ->
   {_, P} = erlang:process_info(Pid, backtrace),
   io:fwrite(Fd, "~s~n", [erlang:binary_to_list(P)]).

writeprocessinfo(Fd, [#mtop_proc_info{pid = Pid}
   | T]) ->
   writedoubleline(Fd),
   io:fwrite(Fd, "~p~n", [erlang:process_info(Pid)]),
   writesingleline(Fd),
   writebt(Fd, Pid),
   writeprocessinfo(Fd, T);

writeprocessinfo(_Fd, []) ->
   ok.


print_infos_stack(Fd, Info) ->
   print_infos_top(Fd, Info),
   writebtinfo(Fd, Info#mtop_info.procinfo).

writebtinfo(Fd, [#mtop_proc_info{pid = Pid}
   | T]) ->
   writedoubleline(Fd),
   writebt(Fd, Pid),
   writebtinfo(Fd, T);

writebtinfo(_Fd, []) ->
   ok.


print_infos_option(Fd, Info, OptionList) ->
   print_infos_top(Fd, Info),
   writeprocessinfo_option(Fd, OptionList, Info#mtop_info.procinfo).

writeprocessinfo_option(Fd, OptionList, [#mtop_proc_info{pid = Pid}
   | T]) ->
   writedoubleline(Fd),
   io:fwrite(Fd, "~p~n", [erlang:process_info(Pid, OptionList)]),
   writesingleline(Fd),
   writebt(Fd, Pid),
   writeprocessinfo_option(Fd, OptionList, T);

writeprocessinfo_option(_Fd, _OptionList, []) ->
   ok.
