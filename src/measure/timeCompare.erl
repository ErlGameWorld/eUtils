-module(timeCompare).

-export([
   time_compare/2,
   time_compare/3
]).


%% Apis ----------------------------------------
% @doc 运行时间比较
% @spec time_compare(Num, TList) -> ok | skip.
% Num       :: integer()
% TList     :: [Test]
% Test      :: {InfoString :: string(), Fun :: fun()}
% format the test result as following:
%
time_compare(Num, TList) ->
   time_compare(Num, TList, []).

% @doc 运行时间比较
% @spec time_compare(Num, TList, Option) -> ok | skip.
% same as time_compare/2
% Option    :: nyi (Not Yet Implemented)
time_compare(0, _TList, _Opt) ->
   skip;
time_compare(Num, TList, _Opt) ->
   ResultList = [do_time_compare(Num, Tester) || Tester <- TList],
   io:format("=============================================================================================~n"),
   do_print_out(ResultList),
   io:format("=============================================================================================~n").

%% Privates ------------------------------------
do_time_compare(Num, {Label, Func}) ->
   statistics(wall_clock),
   do_time_compare_f1(1, Num, Func),
   {_, Time} = statistics(wall_clock),
   US = Time * 1000 / Num,
   io:format("~-30s [wall_clock: total:~10w  ms  avg:~7.3f  us]~n", [Label, Time, US]),
   {Label, Time};
do_time_compare(Num, {Label, Func, ProcessNum}) ->
   Pids = do_time_compare_init(Num, 0, ProcessNum, Func, []),
   statistics(wall_clock),
   [erlang:send(Pid, go) || Pid <- Pids],
   do_time_compare_water(length(Pids)),
   {_, Time} = statistics(wall_clock),
   US = Time * 1000 / Num,
   io:format("~-30s [wall_clock: total: ~10w ms  avg: ~7.3f)us]~n", [Label, Time, US]),
   {Label, Time}.

do_time_compare_init(0, _Start, _ProcessNum, _Func, Pids) ->
   Pids;
do_time_compare_init(Num, Start, 1, Func, Pids) ->
   Self = erlang:self(),
   Pid = spawn_link(fun() -> do_time_compare_worker(Self, Func, Start + 1, Start + Num) end),
   [Pid | Pids];
do_time_compare_init(Num, Start, ProcessNum, Func, Pids) ->
   Self = erlang:self(),
   DoRound = Num div ProcessNum,
   Pid = spawn_link(fun() -> do_time_compare_worker(Self, Func, Start + 1, Start + DoRound) end),
   do_time_compare_init(Num - DoRound, Start + DoRound, ProcessNum - 1, Func, [Pid | Pids]).

do_time_compare_water(0) ->
   ok;
do_time_compare_water(Num) ->
   receive
      ok ->
         do_time_compare_water(Num - 1);
      Msg ->
         erlang:error(io_lib:format("bad message: ~w", [Msg]))
   end.

do_time_compare_worker(Pid, Func, From, To) ->
   receive
      go ->
         do_time_compare_f1(From, To, Func),
         Pid ! ok;
      _ ->
         ok
   end.

do_time_compare_f1(Max, Max, Func) ->
   Func(Max);
do_time_compare_f1(I, Max, Func) ->
   Func(I),
   do_time_compare_f1(I + 1, Max, Func).

do_print_out([{_LabelBase, TimeBase} | _] = ResultList) ->
   do_print_out(ResultList, TimeBase).

% @doc 第二个参数为参照
% @spec do_print_out(ResultList, {RunTimeBase, WallClockBase}) -> ok.
% ResultList                    :: [Result]
% Result                        :: [Label, RunTime, WallClock]
% Label                         :: string()
% RunTimeBase = WallClockBase   :: integer()
do_print_out(ResultList, TimeBase) ->
   [
      io:format(
         "~-30s [   wall_clock: ~10w ms  percentage: ~7.2f%]~n",
         [Label, Time, Time / (TimeBase + 0.000000001) * 100]
      )
      || {Label, Time} <- ResultList
   ].
