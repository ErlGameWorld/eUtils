-module(utProf).

-compile([nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).

% 对整个节点内所有进程执行eprof, eprof 对线上业务有一定影响,慎用!
% 建议TimeoutSec<10s，且进程数< 1000，否则可能导致节点crash
% 结果:
% 输出每个方法实际执行时间（不会累计方法内其他mod调用执行时间）
% 只能得到mod - Fun 执行次数 执行耗时
eprof_all(TimeoutSec) ->
   eprof(processes() -- [whereis(eprof)], TimeoutSec).

eprof(Pids, TimeoutSec) ->
   eprof:start(),
   eprof:start_profiling(Pids),
   timer:sleep(TimeoutSec),
   eprof:stop_profiling(),
   eprof:analyze(total),
   eprof:stop().

% @doc 对MFA 执行分析，会严重减缓运行，建议只对小量业务执行
% 结果:
% fprof 结果比较详细，能够输出热点调用路径
fprof(M, F, A) ->
   fprof:start(),
   fprof:apply(M, F, A),
   fprof:profile(),
   fprof:analyse([{dest, "fprof.analysis"}, {sort, own}]),
   fprof:stop().

%% ====================================================================
%% trace 日志
%% 会把mod 每次调用详细MFA log 下来，args 太大就不好看了
%% trace Mod 所有方法的调用
%% --------------------------------------------------------------------
trace(Mod) ->
   dbg:tracer(),
   dbg:tpl(Mod, '_', []),
   dbg:p(all, c).

%% trace Node上指定 Mod 所有方法的调用, 结果将输出到本地shell
trace(Node, Mod) ->
   dbg:tracer(),
   dbg:n(Node),
   dbg:tpl(Mod, '_', []),
   dbg:p(all, c).

%% 停止trace
trace_stop() ->
   dbg:stop_clear().
%% ====================================================================


