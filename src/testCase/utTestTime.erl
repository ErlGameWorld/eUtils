-module(utTestTime).

-compile([export_all, nowarn_unused_function, nowarn_export_all]).

-define(SECS_1970, 62167219200).        %%utc 1970年经历过的秒数

t1(N, Fun) ->
   LocalTime = erlang:localtime(),
   tt1(N, LocalTime, Fun).

tt1(0, LocalTime, Fun) ->
   ?MODULE:Fun(LocalTime);
tt1(N, LocalTime, Fun) ->
   ?MODULE:Fun(LocalTime),
   tt1(N - 1, LocalTime, Fun).

t2(N, Fun) ->
   Sec = erlang:system_time(second),
   tt2(N, Sec, Fun).

tt2(0, Sec, Fun) ->
   ?MODULE:Fun(Sec);
tt2(N, Sec, Fun) ->
   ?MODULE:Fun(Sec),
   tt2(N - 1, Sec, Fun).

%% 经过测试 ls1 和 sl1 效率最高
%%%%%%%%%%%%%%%%% datetime 和 timestamp 互相转换 测试对比函数 start %%%%%%%%%%%%%%%%%%%
ls1(LocalDate) ->
   erlang:universaltime_to_posixtime(erlang:localtime_to_universaltime(LocalDate)).


%% 此方法很准
ls2(DateTime) ->
   UDateTime = erlang:localtime_to_universaltime(DateTime),
   calendar:datetime_to_gregorian_seconds(UDateTime) - ?SECS_1970.

ls3(DateTime) ->
   case calendar:local_time_to_universal_time_dst(DateTime) of
      [] -> false;
      [_, Udate] ->
         calendar:datetime_to_gregorian_seconds(Udate) - ?SECS_1970;
      [Udate] ->
         calendar:datetime_to_gregorian_seconds(Udate) - ?SECS_1970
   end.

ls4(DateTime) ->
   calendar:datetime_to_gregorian_seconds(DateTime) - 62167248000.


us1(UniversalTime) ->
   erlang:universaltime_to_posixtime(UniversalTime).

%% 将秒单位的时间戳 转为世界 datetime()
su1(Ses) ->
   erlang:posixtime_to_universaltime(Ses).

%% 将秒单位的时间戳 转为本地 datetime()
sl1(Ses) ->
   erlang:universaltime_to_localtime(erlang:posixtime_to_universaltime(Ses)).

sl2(Ses) ->
   DateTime = calendar:gregorian_seconds_to_datetime(Ses + ?SECS_1970),
   calendar:universal_time_to_local_time(DateTime).

sl3(Ses) ->
   T1 = Ses div 1000000,
   T2 = Ses - T1 * 1000000,
   T = calendar:now_to_datetime({T1, T2, 0}),
   calendar:universal_time_to_local_time(T).

%%%%%%%%%%%%%%%%% datetime 和 timestamp 测试对比函数 end  %%%%%%%%%%%%%%%%%%%%%%%%%%

t3(N, Fun) ->
   Sec1 = erlang:system_time(second),
   Sec2 = Sec1 - rand:uniform(1000000),
   tt3(N, Sec1, Sec2, Fun).

tt3(0, Sec1, Sec2, Fun) ->
   ?MODULE:Fun(Sec1, Sec2);
tt3(N, Sec1, Sec2, Fun) ->
   ?MODULE:Fun(Sec1, Sec2),
   tt3(N - 1, Sec1, Sec2, Fun).

%%%%%%%%%%%%%%%%% IsSame*** 测试对比函数 start %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
isd1(Sec1, Sec2) ->
   NDay = Sec1 div 86400,
   ODay = Sec2 div 86400,
   NDay =:= ODay.

isd11(Sec1, Sec2) ->
   Sec1 div 86400 =:= Sec2 div 86400.


isd2(Sec1, Sec2) ->
   {Date1, _Time1} = sl1(Sec1),
   {Date2, _Time2} = sl1(Sec2),
   Date1 =:= Date2.

isw11(Sec1, Sec2) ->
   (Sec1 + 345600) div 604800 =:= (Sec2 + 345600) div 604800.

isw1(Sec1, Sec2) ->
   {{Year1, Month1, Day1}, Time1} = sl1(Sec1),
   % 星期几
   Week1 = calendar:day_of_the_week(Year1, Month1, Day1),
   % 从午夜到现在的秒数
   Diff1 = calendar:time_to_seconds(Time1),
   Monday = Sec1 - Diff1 - (Week1 - 1) * 86400,
   Sunday = Sec1 + (86400 - Diff1) + (7 - Week1) * 86400,
   if ((Sec2 >= Monday) and (Sec2 < Sunday)) -> true;
      true -> false
   end.

day_of_the_week(UnixTime) ->
   {{Y, M, D}, _} = sl1(UnixTime),
   calendar:day_of_the_week(Y, M, D).

%% @doc 返回相应类型的unix时间戳
%% <ul>
%% <li>ms: 取得当前的unix时间戳，精确到毫秒</li>
%% <li>today: 获取当天0时0分0秒的时间戳</li>
%% <li>{today, Ts}: 根据给出的时间戳，获取与该时间戳同一天的零时。当时间为0时，返回值有可能是负值，因为这里有时区偏移值(例如北京时间就可能是-28800)</li>
%% <li>{tomorrow, Ts}: 根据给出的时间戳，获取该时间戳第二天的零时</li>
%% <li>{next_time, Ts}::根据给出的时间间隔（距离0点的时间, 如3600），取出下一个该时间戳</li>
%% </ul>
-spec unixtime(X) -> pos_integer() when
   X :: ms | today | {today, pos_integer()} | {next_day, pos_integer()}.
unixtime(ms) ->
   %% {S1, S2, S3} = erlang:now(),
   {S1, S2, S3} = os:timestamp(),
   trunc(S1 * 1000000000 + S2 * 1000 + S3 / 1000);
unixtime(today) ->
   {M, S, MS} = os:timestamp(),
   {_, Time} = calendar:now_to_local_time({M, S, MS}),
   M * 1000000 + S - calendar:time_to_seconds(Time);
unixtime(tomorrow) ->
   unixtime(today) + 86400;
unixtime({today, Ts}) ->
   Base = unixtime(today),
   case Ts > Base of
      false -> Base - util:ceil((Base - Ts) / 86400) * 86400;
      true -> (Ts - Base) div 86400 * 86400 + Base
   end;
unixtime({tomorrow, Ts}) ->
   unixtime({today, Ts}) + 86400;
unixtime({next_time, DayTs}) ->
   Now = erlang:system_time(second),
   NextT = next_diff(DayTs),
   Now + NextT.

%% @doc 当前距离N天后某时刻的相差秒数
-spec now_diff(N, {H, I, S}) -> integer() when
   N :: today | tomorrow | non_neg_integer(),
   H :: 0..23,
   I :: 0..59,
   S :: 0..59.
now_diff(today, {H, I, S}) ->
   now_diff(0, {H, I, S});
now_diff(tomorrow, {H, I, S}) ->
   now_diff(1, {H, I, S});
now_diff(N, {H, I, S}) when is_integer(N), N >= 0 ->
   {_, Time} = calendar:local_time(),
   N * 86400 + calendar:time_to_seconds({H, I, S}) - calendar:time_to_seconds(Time).

-spec next_diff(0..86400 | [0..86400]) -> Seconds :: pos_integer().
next_diff(L = [_ | _]) ->
   lists:min([next_diff(Sec) || Sec <- L]);
next_diff(Sec) ->
   Now = erlang:system_time(second),
   Zero = unixtime({today, Now}),
   Base = Zero + Sec, %% 取当天距离X的时间为指定时间
   case Base > Now of
      true -> Base - Now; %% 当前时间比指定时间小 直接返回差距
      false -> Base + 86400 - Now %% 当前时间比指定时间大 加上一天时间后求差
   end.

isw2(0, Ts2) when Ts2 > 0 -> false;
isw2(Ts1, 0) when Ts1 > 0 -> false;
isw2(Ts, Ts) -> true;
isw2(Ts1, Ts2) ->
   Dw = day_of_the_week(Ts1),
   Zero = unixtime({today, Ts1}),
   Beg = Zero - 86400 * (Dw - 1),
   End = Zero + 86400 * (8 - Dw),
   Ts2 >= Beg andalso Ts2 < End.

ism1(Sec1, Sec2) ->
   {{Year1, Month1, _Day1}, _Time1} = sl1(Sec1),
   {{Year2, Month2, _Day2}, _Time2} = sl1(Sec2),
   Month1 =:= Month2 andalso Year1 =:= Year2.

ism2(Sec1, Sec2) ->
   {{Year1, Month1, _Day1}, _Time1} = sl1(Sec1),
   Begin = ls1({{Year1, Month1, 1}, {0, 0, 0}}),
   EndDay = calendar:last_day_of_the_month(Year1, Month1),
   End = Begin + EndDay * 86400,
   Sec2 >= Begin andalso Sec2 =< End.

%%%%%%%%%%%%%%%%% IsSame*** 测试对比函数 end  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%% **Begin **End 测试对比函数 start %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
timeZoneDiff() ->
   UTC2001 = {{2001, 1, 1}, {0, 0, 0}},
   %% 将1970年时间由世界时间转为本地时间
   Local2001 = erlang:universaltime_to_localtime(UTC2001),
   %% 计算2001世界时间的秒时间戳
   %  UTC2001Sec = calendar:datetime_to_gregorian_seconds(UTC2001),
   %% UTC2001Sec = 63145526400,
   %% 计算2001年本地时间的时间戳
   %% Local2001Sec = calendar:datetime_to_gregorian_seconds(Local2001),
   %% Local2001Sec - UTC2001Sec.
   erlang:universaltime_to_posixtime(Local2001) - 978307200.

hs1(Sec) ->
   Sec - Sec rem 3600.

ds1(Sec) ->
   Sec - (Sec + timeZoneDiff()) rem 86400.


ws1(Sec) ->
   Sec - (Sec - 345600 + timeZoneDiff()) rem 604800.

ms1(Sec) ->
   {Days, _Time} = calendar:seconds_to_daystime(Sec + ?SECS_1970),
   {Y, M, _D} = calendar:gregorian_days_to_date(Days),
   MonthStartDateTime = {{Y, M, 1}, {0, 0, 0}},
   ls1(MonthStartDateTime).

ms2(Sec) ->
   {{Year, Month, _Day}, _Time} = su1(Sec),
   MonthStartDateTime = {{Year, Month, 1}, {0, 0, 0}},
   ls1(MonthStartDateTime).

%%%%%%%%%%%%%%%%% **Begin **End 测试对比函数 end   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
t4(N, Fun) ->
   persistent_term:put(zonediff, 28800),
   tt4(N, Fun).

tt4(0, Fun) ->
   ?MODULE:Fun();
tt4(N, Fun) ->
   ?MODULE:Fun(),
   tt4(N - 1, Fun).

zone1() ->
   persistent_term:get(zonediff, 28800).


zone2() ->
   Local2001 = erlang:universaltime_to_localtime({{2001, 1, 1}, {0, 0, 0}}),
   io:format("IMY**************** ~p~n", [Local2001]),
   erlang:universaltime_to_posixtime(Local2001) - 978307200.

zone3() ->
   NowTime = erlang:universaltime(),
   LoTime = erlang:universaltime_to_localtime(NowTime),
   io:format("IMY**************** ~p~n", [NowTime]),
   io:format("IMY**************** ~p~n", [LoTime]),
   erlang:universaltime_to_posixtime(LoTime) - erlang:universaltime_to_posixtime(NowTime).

tk1() ->
   erlang:system_time(microsecond).

tk2() ->
   os:system_time(microsecond).

tk3() ->
   erlang:system_time(nanosecond).

tk4() ->
   make_ref().

tk5() ->
   erlang:unique_integer([positive]) rem 16.

tk6() ->
   erlang:monotonic_time(second).
