-module(utTime).
-include("utTime.hrl").

-import(calendar, [day_of_the_week/1, day_of_the_week/3, iso_week_number/1, date_to_gregorian_days/1, gregorian_days_to_date/1]).

-export([
   now/0                            %% 当前的时间戳 秒
   , nowMs/0                        %% 当前的时间戳 毫秒
   , curDateTime/0                  %% 当前的日期
   , curDate/0                      %% 当前的年月日
   , curTime/0                      %% 当前的时分秒
   , weekDay/0                      %% 当前星期几
   , weekDay/1                      %% 计算Data是星期几
   , weekDay/3                      %% 计算 年 月 日 是星期几
   , weekCycle/0                    %% 计算当前的星期周期
   , weekCycle/1                    %% 计算 Date 的星期周期
   , secToLDateTime/1               %% 将秒单位的时间戳 转为本地 datetime()
   , secToUDateTime/1               %% 将秒单位的时间戳 转为世界 datetime()
   , lDateTimeToSec/1               %% 将本地的 datetime() 转为秒单位的时间戳
   , uDateTimeToSec/1               %% 将世界的 datetime() 转为秒单位的时间戳
   , timeZoneDiff/0                 %% 获取本地时区与UTC时区 时间差 单位秒
   , countLDay/1                    %% 计算1970年到Sec 本地经过了多少天
   , countUDay/1                    %% 计算1970年到Sec 世界经过了多少天
   , isSameLDay/2                   %% 判断两个时间戳是否为本地的同一天
   , isSameUDay/2                   %% 判断两个时间戳是否为本地的同一天
   , countLWeek/1                   %% 计算1970年到Sec 本地经过了多少周
   , countUWeek/1                   %% 计算1970年到Sec 世界经过了多少周
   , isSameLWeek/2                  %% 判断两个时间戳是否为本地的同一周
   , isSameUWeek/2                  %% 判断两个时间戳是否为本地的同一周
   , isSameLMonth/2                 %% 判断两个时间戳是否为本地的同一月
   , isSameUMonth/2                 %% 判断两个时间戳是否为世界的同一月
   , countLDay/2                    %% 计算1970年到Sec 本地经过了多少天 ZeroOffset
   , countUDay/2                    %% 计算1970年到Sec 世界经过了多少天 ZeroOffset
   , isSameLDay/3                   %% 判断两个时间戳是否为同一天 ZeroOffset
   , isSameUDay/3                   %% 判断两个时间戳是否为同一天 ZeroOffset
   , countLWeek/2                   %% 计算1970年到Sec 本地经过了多少周 ZeroOffset
   , countUWeek/2                   %% 计算1970年到Sec 世界经过了多少周 ZeroOffset
   , isSameLWeek/3                  %% 判断两个时间戳是否为同一周 ZeroOffset
   , isSameUWeek/3                  %% 判断两个时间戳是否为同一周 ZeroOffset
   , isSameLMonth/3                 %% 判断两个时间戳是否为本地的同一月 ZeroOffset
   , isSameUMonth/3                 %% 判断两个时间戳是否为世界的同一月 ZeroOffset
   , hourBegin/0                    %% 当前小时开始时间戳
   , hourBegin/1                    %% Sec所在小时开始时间戳
   , hourEnd/0                      %% 当前小时结束时间戳
   , hourEnd/1                      %% Sec所在小时结束时间戳
   , dayLBegin/0                    %% 本地当前天开始时间戳
   , dayLBegin/1                    %% 本地Sec所在天开始时间戳
   , dayUBegin/0                    %% 世界Sec所在天开始时间戳
   , dayUBegin/1                    %% 世界Sec所在天开始时间戳
   , dayLEnd/0                      %% 本地当前天结束时间戳
   , dayLEnd/1                      %% 本地Sec所在天结束时间戳
   , dayUEnd/0                      %% 世界当前天结束时间戳
   , dayUEnd/1                      %% 世界Sec所在天结束时间戳
   , dayLBeginEnd/0                 %% 本地当前天开始结束时间戳
   , dayLBeginEnd/1                 %% 本地Sec所在天开始结束时间戳
   , dayUBeginEnd/0                 %% 世界当前天开始结束时间戳
   , dayUBeginEnd/1                 %% 世界Sec所在天开始结束时间戳
   , weekLBegin/0                   %% 本地当前周开始时间戳
   , weekLBegin/1                   %% 本地Sec所在周开始时间戳
   , weekUBegin/0                   %% 世界当前周的开始时间戳
   , weekUBegin/1                   %% 世界Sec所在周的开始时间戳
   , weekLEnd/0                     %% 本地当前周的结束时间戳
   , weekLEnd/1                     %% 本地Sec所在周的结束时间戳
   , weekUEnd/0                     %% 世界当前周的开始时间戳
   , weekUEnd/1                     %% 世界Sec所在周的结束时间戳
   , weekLBeginEnd/0                %% 本地当前周的开始结束时间戳
   , weekLBeginEnd/1                %% 本地Sec所在周的开始结束时间戳
   , weekUBeginEnd/0                %% 世界当前周的开始结束时间戳
   , weekUBeginEnd/1                %% 世界Sec所在周的开始结束时间戳
   , monthLBegin/0                  %% 本地当前月的开始时间戳
   , monthLBegin/1                  %% 本地Sec所在月的开始时间戳
   , monthUBegin/0                  %% 世界当前月的开始时间戳
   , monthUBegin/1                  %% 世界Sec所在月的开始时间戳
   , monthLEnd/0                    %% 本地当前月的结束时间戳
   , monthLEnd/1                    %% 本地Sec所在月的结束时间戳
   , monthUEnd/0                    %% 世界当前周的结束时间戳
   , monthUEnd/1                    %% 世界Sec所在月的结束时间戳
   , isLeapYear/1                   %% 闰年判断函数
   , monthDay/2                     %% 根据 年 月 返回对应月的天数
   , monthSecs/2                    %% 根据年月 返回月的总秒数
   , monthLBeginEnd/0               %% 本地当前月的开始结束时间戳
   , monthLBeginEnd/1               %% 本地Sec所在月的开始结束时间戳
   , monthUBeginEnd/0               %% 世界当前月的开始结束时间戳
   , monthUBeginEnd/1               %% 世界Sec所在月的开始结束时间戳
   , sWeekName/1                    %% 星期名字缩写
   , lWeekName/1                    %% 星期名字全称
   , sMonthName/1                   %% 月份名称缩写
   , lMonthName/1                   %% 月份名称全称
   , dateNumber/0                   %% 年月日的数字version
   , dateNumber/1                   %% 年月日的数字version
   , dateNumber/3                   %% 年月日的数字version
   , numberDate/1                   %% dateNumber 反转
   , secToDayTime/1                 %% 秒转为天数 和 time()
   , diffLDateTimeSec/2             %% 计算两个本地datetime() 时间差 返回单位 秒
   , diffUDateTimeSec/2             %% 计算两个世界datetime() 时间差 返回单位 秒
   , diffLDateTimeDayTime/2         %% 计算两个本地datetime() 时间差 返回单位 daytime()
   , diffUDateTimeDayTime/2         %% 计算两个世界datetime() 时间差 返回单位 daytime()
   , diffSecs/2                     %% 计算两个秒单位的 时间戳的时间差 返回单位 daytime()
   , timeToSec/1                    %% 转换 time() 为 秒数
   , daysInYear/1                   %% 计算 Date为该年的哪一天
   , dateToStr/1                    %% Data to Str
   , dateToStr/3                    %% Data to Str
   , dateToStr/2                    %% Data to Str
   , dateToStr/4                    %% Data to Str
   , timeToStr/1                    %% time to Str
   , timeToStr/3                    %% time to Str
   , timeToStr/2                    %% time to Str
   , timeToStr/4                    %% time to Str
   , dateTimeStr/1                  %% datetime to Str
]).

%% 当前的时间戳 秒
-spec now() -> timestamp().
now() ->
   erlang:system_time(second).

%% 当前的时间戳 毫秒
-spec nowMs() -> timestamp().
nowMs() ->
   erlang:system_time(millisecond).

%% 当前的日期
-spec curDateTime() -> datetime().
curDateTime() ->
   secToLDateTime(erlang:system_time(second)).

%% 当前的年月日
-spec curDate() -> date().
curDate() ->
   {Date, _} = curDateTime(),
   Date.

%% 当前的时分秒
-spec curTime() -> time().
curTime() ->
   {_, Time} = curDateTime(),
   Time.

%% 当前星期几
-spec weekDay() -> week().
weekDay() ->
   day_of_the_week(curDate()).

%% 计算Data是星期几
-spec weekDay(Date :: date()) -> week().
weekDay(Date) ->
   day_of_the_week(Date).

%% 计算 年 月 日 是星期几
-spec weekDay(Year :: year(), Month :: month(), Day :: day()) -> week().
weekDay(Year, Month, Day) ->
   day_of_the_week(Year, Month, Day).

%% 计算当前的星期周期
-spec weekCycle() -> yearWeekCycle().
weekCycle() ->
   iso_week_number(curDate()).

%% 计算 Date 的星期周期
-spec weekCycle(Date :: date()) -> yearWeekCycle().
weekCycle(Date) ->
   iso_week_number(Date).

%% 将秒单位的时间戳 转为本地 datetime()
-spec secToLDateTime(Sec :: timestamp()) -> datetime().
secToLDateTime(Sec) ->
   eNifFtz:universal_time_to_local_time(erlang:posixtime_to_universaltime(Sec)).

%% 将秒单位的时间戳 转为世界 datetime()
-spec secToUDateTime(Sec :: timestamp()) -> datetime().
secToUDateTime(Sec) ->
   erlang:posixtime_to_universaltime(Sec).

%% 将本地的 datetime() 转为秒单位的时间戳
-spec lDateTimeToSec(LocalDate :: datetime()) -> timestamp().
lDateTimeToSec(LocalDate) ->
   erlang:universaltime_to_posixtime(eNifFtz:local_time_to_universal_time(LocalDate)).

%% 将世界的 datetime() 转为秒单位的时间戳
-spec uDateTimeToSec(UniversalTime :: datetime()) -> timestamp().
uDateTimeToSec(UniversalTime) ->
   erlang:universaltime_to_posixtime(UniversalTime).

%% 获取本地时区与UTC时区 时间差 单位秒
-spec timeZoneDiff() -> timestamp().
timeZoneDiff() ->
   eNifFtz:zone_offset().

%% 计算1970年到Sec 本地经过了多少天
-spec countLDay(Sec :: timestamp()) -> integer().
countLDay(Sec) ->
   {{Year, Month, Day}, _} = secToLDateTime(Sec),
   date_to_gregorian_days({Year, Month, Day}) - date_to_gregorian_days({1970, 1, 1}).

%% 计算1970年到Sec 世界经过了多少天
-spec countUDay(Sec :: timestamp()) -> integer().
countUDay(Sec) ->
   Sec div ?SECS_DAY.

%% 判断两个时间戳是否为本地的同一天
-spec isSameLDay(Sec1 :: timestamp(), Sec2 :: timestamp()) -> boolean().
isSameLDay(Sec1, Sec2) ->
   {D1, _} = secToLDateTime(Sec1),
   {D2, _} = secToLDateTime(Sec2),
   D1 =:= D2.

%% 判断两个时间戳是否为本地的同一天
-spec isSameUDay(Sec1 :: timestamp(), Sec2 :: timestamp()) -> boolean().
isSameUDay(Sec1, Sec2) ->
   Sec1 div ?SECS_DAY =:= Sec2 div ?SECS_DAY.

%% 计算1970年到Sec 本地经过了多少周
-spec countLWeek(Sec :: timestamp()) -> integer().
countLWeek(Sec) ->
   Days = countLDay(Sec),
   (Days + 3) div 7.

%% 计算1970年到Sec 世界经过了多少周
-spec countUWeek(Sec :: timestamp()) -> integer().
countUWeek(Sec) ->
   %% 1970-01-01 是周四。为了让周一成为 Week 0 的开始，我们需要偏移 +3 天 (259200秒)
   %% 这样周四(Day 3) 就变成了 (3+3)=6，还不够，我们需要对齐到周一。
   %% 实际上：(Sec + 3天) div 7天。
   (Sec + 259200) div ?SECS_WEEK.

%% 判断两个时间戳是否为本地的同一周
-spec isSameLWeek(Sec1 :: timestamp(), Sec2 :: timestamp()) -> boolean().
isSameLWeek(Sec1, Sec2) ->
   countLWeek(Sec1) =:= countLWeek(Sec2).

%% 判断两个时间戳是否为本地的同一周
-spec isSameUWeek(Sec1 :: timestamp(), Sec2 :: timestamp()) -> boolean().
isSameUWeek(Sec1, Sec2) ->
   countUWeek(Sec1) =:= countUWeek(Sec2).

%% 判断两个时间戳是否为本地的同一月
-spec isSameLMonth(Sec1 :: timestamp(), Sec2 :: timestamp()) -> boolean().
isSameLMonth(Sec1, Sec2) ->
   {{Year1, Month1, _Day1}, _Time1} = secToLDateTime(Sec1),
   {{Year2, Month2, _Day2}, _Time2} = secToLDateTime(Sec2),
   Month1 =:= Month2 andalso Year1 =:= Year2.

%% 判断两个时间戳是否为世界的同一月
-spec isSameUMonth(Sec1 :: timestamp(), Sec2 :: timestamp()) -> boolean().
isSameUMonth(Sec1, Sec2) ->
   {{Year1, Month1, _Day1}, _Time1} = secToUDateTime(Sec1),
   {{Year2, Month2, _Day2}, _Time2} = secToUDateTime(Sec2),
   Month1 =:= Month2 andalso Year1 =:= Year2.

%% 计算1970年到Sec 本地经过了多少天 ZeroOffset为零点偏移时间
-spec countLDay(Sec :: timestamp(), ZeroOffset :: timestamp()) -> integer().
countLDay(Sec, ZeroOffset) ->
   countLDay(Sec - ZeroOffset).

%% 计算1970年到Sec 世界经过了多少天 ZeroOffset为零点偏移时间
-spec countUDay(Sec :: timestamp(), ZeroOffset :: timestamp()) -> integer().
countUDay(Sec, ZeroOffset) ->
   (Sec - ZeroOffset) div ?SECS_DAY.

%% 判断两个时间戳是否为同一天 ZeroOffset为零点偏移时间
-spec isSameLDay(Sec1 :: timestamp(), Sec2 :: timestamp(), ZeroOffset :: timestamp()) -> boolean().
isSameLDay(Sec1, Sec2, ZeroOffset) ->
   isSameLDay(Sec1 - ZeroOffset, Sec2 - ZeroOffset).

%% 判断两个时间戳是否为同一天 ZeroOffset为零点偏移时间
-spec isSameUDay(Sec1 :: timestamp(), Sec2 :: timestamp(), ZeroOffset :: timestamp()) -> boolean().
isSameUDay(Sec1, Sec2, ZeroOffset) ->
   (Sec1 - ZeroOffset) div ?SECS_DAY =:= (Sec2 - ZeroOffset) div ?SECS_DAY.

%% 计算1970年到Sec 本地经过了多少周 ZeroOffset为零点偏移时间
-spec countLWeek(Sec :: timestamp(), ZeroOffset :: timestamp()) -> integer().
countLWeek(Sec, ZeroOffset) ->
   countLWeek(Sec - ZeroOffset).

%% 计算1970年到Sec 世界经过了多少周 ZeroOffset为零点偏移时间
-spec countUWeek(Sec :: timestamp(), ZeroOffset :: timestamp()) -> integer().
countUWeek(Sec, ZeroOffset) ->
   (Sec - ZeroOffset + 259200) div ?SECS_WEEK.

%% 判断两个时间戳是否为同一周 ZeroOffset为零点偏移时间
-spec isSameLWeek(Sec1 :: timestamp(), Sec2 :: timestamp(), ZeroOffset :: timestamp()) -> boolean().
isSameLWeek(Sec1, Sec2, ZeroOffset) ->
   isSameLWeek(Sec1 - ZeroOffset, Sec2 - ZeroOffset).

%% 判断两个时间戳是否为同一周 ZeroOffset为零点偏移时间
-spec isSameUWeek(Sec1 :: timestamp(), Sec2 :: timestamp(), ZeroOffset :: timestamp()) -> boolean().
isSameUWeek(Sec1, Sec2, ZeroOffset) ->
   (Sec1 - ZeroOffset + 259200) div ?SECS_WEEK =:= (Sec2 - ZeroOffset + 259200) div ?SECS_WEEK.

%% 判断两个时间戳是否为本地的同一月 ZeroOffset为零点偏移时间
-spec isSameLMonth(Sec1 :: timestamp(), Sec2 :: timestamp(), ZeroOffset :: timestamp()) -> boolean().
isSameLMonth(Sec1, Sec2, ZeroOffset) ->
   {{Year1, Month1, _Day1}, _Time1} = secToLDateTime(Sec1 - ZeroOffset),
   {{Year2, Month2, _Day2}, _Time2} = secToLDateTime(Sec2 - ZeroOffset),
   Month1 =:= Month2 andalso Year1 =:= Year2.

%% 判断两个时间戳是否为世界的同一月 ZeroOffset为零点偏移时间
-spec isSameUMonth(Sec1 :: timestamp(), Sec2 :: timestamp(), ZeroOffset :: timestamp()) -> boolean().
isSameUMonth(Sec1, Sec2, ZeroOffset) ->
   {{Year1, Month1, _Day1}, _Time1} = secToUDateTime(Sec1 - ZeroOffset),
   {{Year2, Month2, _Day2}, _Time2} = secToUDateTime(Sec2 - ZeroOffset),
   Month1 =:= Month2 andalso Year1 =:= Year2.

%% 当前小时开始时间戳
-spec hourBegin() -> timestamp().
hourBegin() ->
   hourBegin(erlang:system_time(second)).

%% Sec所在小时开始时间戳
%% [修正] 兼容半小时时区 (如 India +5:30)，必须基于本地时间计算
-spec hourBegin(Sec :: timestamp()) -> timestamp().
hourBegin(Sec) ->
   {{Y, M, D}, {H, _, _}} = secToLDateTime(Sec),
   lDateTimeToSec({{Y, M, D}, {H, 0, 0}}).

%% 当前小时结束时间戳
-spec hourEnd() -> timestamp().
hourEnd() ->
   hourEnd(erlang:system_time(second)).

%% Sec所在小时结束时间戳 (返回下一小时的起始点)
-spec hourEnd(Sec :: timestamp()) -> timestamp().
hourEnd(Sec) ->
   {{Y, M, D}, {H, _, _}} = secToLDateTime(Sec),
   %% 计算下一个小时
   {NewDate, NewTime} =
      case H of
         23 ->
            Days = date_to_gregorian_days({Y, M, D}),
            {gregorian_days_to_date(Days + 1), {0, 0, 0}};
         _ ->
            {{Y, M, D}, {H + 1, 0, 0}}
      end,
   lDateTimeToSec({NewDate, NewTime}).

%% 本地当前天开始时间戳
-spec dayLBegin() -> timestamp().
dayLBegin() ->
   dayLBegin(erlang:system_time(second)).

%% 本地Sec所在天开始时间戳
-spec dayLBegin(Sec :: timestamp()) -> timestamp().
dayLBegin(Sec) ->
   {{Y, M, D}, _} = secToLDateTime(Sec),
   lDateTimeToSec({{Y, M, D}, {0, 0, 0}}).

%% 世界Sec所在天开始时间戳
-spec dayUBegin() -> timestamp().
dayUBegin() ->
   dayUBegin(erlang:system_time(second)).

%% 世界Sec所在天开始时间戳
-spec dayUBegin(Sec :: timestamp()) -> timestamp().
dayUBegin(Sec) ->
   Sec - Sec rem ?SECS_DAY.

%% 本地当前天结束时间戳
-spec dayLEnd() -> timestamp().
dayLEnd() ->
   dayLEnd(erlang:system_time(second)).

%% 本地Sec所在天结束时间戳
-spec dayLEnd(Sec :: timestamp()) -> timestamp().
dayLEnd(Sec) ->
   {Date, _} = secToLDateTime(Sec),
   Days = date_to_gregorian_days(Date),
   NextDay = gregorian_days_to_date(Days + 1),
   lDateTimeToSec({NextDay, {0, 0, 0}}).

%% 世界当前天结束时间戳
-spec dayUEnd() -> timestamp().
dayUEnd() ->
   dayUEnd(erlang:system_time(second)).

%% 世界Sec所在天结束时间戳
-spec dayUEnd(Sec :: timestamp()) -> timestamp().
dayUEnd(Sec) ->
   Sec - Sec rem ?SECS_DAY + ?SECS_DAY.

%% 本地当前天开始结束时间戳
-spec dayLBeginEnd() -> {timestamp(), timestamp()}. %% [修正] Spec类型
dayLBeginEnd() ->
   dayLBeginEnd(erlang:system_time(second)).

%% 本地Sec所在天开始结束时间戳
-spec dayLBeginEnd(Sec :: timestamp()) -> {timestamp(), timestamp()}.
dayLBeginEnd(Sec) ->
   {dayLBegin(Sec), dayLEnd(Sec)}.

%% 世界当前天开始结束时间戳
-spec dayUBeginEnd() -> {timestamp(), timestamp()}.
dayUBeginEnd() ->
   dayUBeginEnd(erlang:system_time(second)).

%% 世界Sec所在天开始结束时间戳
-spec dayUBeginEnd(Sec :: timestamp()) -> {timestamp(), timestamp()}.
dayUBeginEnd(Sec) ->
   Begin = Sec - Sec rem ?SECS_DAY,
   {Begin, Begin + ?SECS_DAY}.

%% 本地当前周开始时间戳
-spec weekLBegin() -> timestamp().
weekLBegin() ->
   weekLBegin(erlang:system_time(second)).

%% 本地Sec所在周开始时间戳
-spec weekLBegin(Sec :: timestamp()) -> timestamp().
weekLBegin(Sec) ->
   {{Y, M, D}, _} = secToLDateTime(Sec),
   W = day_of_the_week(Y, M, D),
   StartG = date_to_gregorian_days({Y, M, D}) - (W - 1),
   StartDate = gregorian_days_to_date(StartG),
   lDateTimeToSec({StartDate, {0, 0, 0}}).

%% 世界当前周的开始时间戳
-spec weekUBegin() -> timestamp().
weekUBegin() ->
   weekUBegin(erlang:system_time(second)).

%% 世界Sec所在周的开始时间戳
%% [修正] 确保与 countUWeek 的 +259200 逻辑绝对一致。
%% Start = Count * 7days - 259200 (Offset)
-spec weekUBegin(Sec :: timestamp()) -> timestamp().
weekUBegin(Sec) ->
   countUWeek(Sec) * ?SECS_WEEK - 259200.

%% 本地当前周的结束时间戳
-spec weekLEnd() -> timestamp().
weekLEnd() ->
   weekLEnd(erlang:system_time(second)).

%% 本地Sec所在周的结束时间戳
-spec weekLEnd(Sec :: timestamp()) -> timestamp().
weekLEnd(Sec) ->
   {{Y, M, D}, _} = secToLDateTime(Sec),
   W = day_of_the_week(Y, M, D),
   Days = date_to_gregorian_days({Y, M, D}) - (W - 1) + 7,
   NextMonday = gregorian_days_to_date(Days),
   lDateTimeToSec({NextMonday, {0, 0, 0}}).

%% 世界当前周的开始时间戳
-spec weekUEnd() -> timestamp().
weekUEnd() ->
   weekUEnd(erlang:system_time(second)).

%% 世界Sec所在周的结束时间戳
-spec weekUEnd(Sec :: timestamp()) -> timestamp().
weekUEnd(Sec) ->
   weekUBegin(Sec) + ?SECS_WEEK.

%% 本地当前周的开始结束时间戳
-spec weekLBeginEnd() -> {timestamp(), timestamp()}.
weekLBeginEnd() ->
   weekLBeginEnd(erlang:system_time(second)).

%% 本地Sec所在周的开始结束时间戳
-spec weekLBeginEnd(Sec :: timestamp()) -> {timestamp(), timestamp()}.
weekLBeginEnd(Sec) ->
   {weekLBegin(Sec), weekLEnd(Sec)}.

%% 世界当前周的开始结束时间戳
-spec weekUBeginEnd() -> {timestamp(), timestamp()}.
weekUBeginEnd() ->
   weekUBeginEnd(erlang:system_time(second)).

%% 世界Sec所在周的开始结束时间戳
-spec weekUBeginEnd(Sec :: timestamp()) -> {timestamp(), timestamp()}.
weekUBeginEnd(Sec) ->
   Begin = weekUBegin(Sec),
   {Begin, Begin + ?SECS_WEEK}.

%% 本地当前月的开始时间戳
-spec monthLBegin() -> timestamp().
monthLBegin() ->
   monthLBegin(erlang:system_time(second)).

%% 本地Sec所在月的开始时间戳
-spec monthLBegin(Sec :: timestamp()) -> timestamp().
monthLBegin(Sec) ->
   {{Year, Month, _Day}, _Time} = secToLDateTime(Sec),
   MonthStartDateTime = {{Year, Month, 1}, {0, 0, 0}},
   lDateTimeToSec(MonthStartDateTime).

%% 世界当前月的开始时间戳
-spec monthUBegin() -> timestamp().
monthUBegin() ->
   monthUBegin(erlang:system_time(second)).

%% 世界Sec所在月的开始时间戳
-spec monthUBegin(Sec :: timestamp()) -> timestamp().
monthUBegin(Sec) ->
   {{Year, Month, _Day}, _Time} = erlang:posixtime_to_universaltime(Sec),
   MonthStartDateTime = {{Year, Month, 1}, {0, 0, 0}},
   erlang:universaltime_to_posixtime(MonthStartDateTime).

%% 本地当前月的结束时间戳
-spec monthLEnd() -> timestamp().
monthLEnd() ->
   monthLEnd(erlang:system_time(second)).

%% 本地Sec所在月的结束时间戳
-spec monthLEnd(Sec :: timestamp()) -> timestamp().
monthLEnd(Sec) ->
   {{Year, Month, _Day}, _Time} = secToLDateTime(Sec),
   {NextYear, NextMonth} = case Month of
      12 -> {Year + 1, 1};
      _  -> {Year, Month + 1}
   end,
   lDateTimeToSec({{NextYear, NextMonth, 1}, {0, 0, 0}}).

%% 世界当前周的结束时间戳
-spec monthUEnd() -> timestamp().
monthUEnd() ->
   monthUEnd(erlang:system_time(second)).

%% 世界Sec所在月的结束时间戳
-spec monthUEnd(Sec :: timestamp()) -> timestamp().
monthUEnd(Sec) ->
   {{Year, Month, _Day}, _Time} = erlang:posixtime_to_universaltime(Sec),
   {NextYear, NextMonth} = case Month of
      12 -> {Year + 1, 1};
      _  -> {Year, Month + 1}
   end,
   erlang:universaltime_to_posixtime({{NextYear, NextMonth, 1}, {0, 0, 0}}).

%% 闰年判断函数
-spec isLeapYear(Yesr :: year()) -> boolean().
isLeapYear(Year) ->
   (Year rem 4 =:= 0 andalso Year rem 100 =/= 0) orelse (Year rem 400 =:= 0).

%% 根据 年 月 返回对应月的天数
-spec monthDay(year(), month()) -> timestamp().
monthDay(_, 4) -> 30;
monthDay(_, 6) -> 30;
monthDay(_, 9) -> 30;
monthDay(_, 11) -> 30;
monthDay(Y, 2) ->
   case isLeapYear(Y) of
      true -> 29;
      _ -> 28
   end;
monthDay(_, _) ->
   31.

%% 根据年月 返回月的总秒数
-spec monthSecs(year(), month()) -> timestamp().
monthSecs(_, 4) -> 2592000;
monthSecs(_, 6) -> 2592000;
monthSecs(_, 9) -> 2592000;
monthSecs(_, 11) -> 2592000;
monthSecs(Y, 2) ->
   case isLeapYear(Y) of
      true -> 2505600;
      _ -> 2419200
   end;
monthSecs(_, _) ->
   2678400.

%% 本地当前月的开始结束时间戳
-spec monthLBeginEnd() -> {timestamp(), timestamp()}.
monthLBeginEnd() ->
   monthLBeginEnd(erlang:system_time(second)).

%% 本地Sec所在月的开始结束时间戳
-spec monthLBeginEnd(Sec :: timestamp()) -> {timestamp(), timestamp()}.
monthLBeginEnd(Sec) ->
   {monthLBegin(Sec), monthLEnd(Sec)}.

%% 世界当前月的开始结束时间戳
-spec monthUBeginEnd() -> {timestamp(), timestamp()}.
monthUBeginEnd() ->
   monthUBeginEnd(erlang:system_time(second)).


%% 世界Sec所在月的开始结束时间戳
-spec monthUBeginEnd(Sec :: timestamp()) -> {timestamp(), timestamp()}.
monthUBeginEnd(Sec) ->
   {monthUBegin(Sec), monthUEnd(Sec)}.

%% 星期名字缩写
-spec sWeekName(week()) -> binary().
sWeekName(1) -> <<"Mon">>;
sWeekName(2) -> <<"Tue">>;
sWeekName(3) -> <<"Wed">>;
sWeekName(4) -> <<"Thu">>;
sWeekName(5) -> <<"Fri">>;
sWeekName(6) -> <<"Sat">>;
sWeekName(7) -> <<"Sun">>.

%% 星期名字全称
-spec lWeekName(week()) -> binary().
lWeekName(1) -> <<"Monday">>;
lWeekName(2) -> <<"Tuesday">>;
lWeekName(3) -> <<"Wednesday">>;
lWeekName(4) -> <<"Thursday">>;
lWeekName(5) -> <<"Friday">>;
lWeekName(6) -> <<"Saturday">>;
lWeekName(7) -> <<"Sunday">>.

%% 月份名称缩写
-spec sMonthName(month()) -> binary().
sMonthName(1) -> <<"Jan">>;
sMonthName(2) -> <<"Feb">>;
sMonthName(3) -> <<"Mar">>;
sMonthName(4) -> <<"Apr">>;
sMonthName(5) -> <<"May">>;
sMonthName(6) -> <<"Jun">>;
sMonthName(7) -> <<"Jul">>;
sMonthName(8) -> <<"Aug">>;
sMonthName(9) -> <<"Sep">>;
sMonthName(10) -> <<"Oct">>;
sMonthName(11) -> <<"Nov">>;
sMonthName(12) -> <<"Dec">>.

%% 月份名称全称
-spec lMonthName(month()) -> binary().
lMonthName(1) -> <<"January">>;
lMonthName(2) -> <<"February">>;
lMonthName(3) -> <<"March">>;
lMonthName(4) -> <<"April">>;
lMonthName(5) -> <<"May">>;
lMonthName(6) -> <<"June">>;
lMonthName(7) -> <<"July">>;
lMonthName(8) -> <<"August">>;
lMonthName(9) -> <<"September">>;
lMonthName(10) -> <<"October">>;
lMonthName(11) -> <<"November">>;
lMonthName(12) -> <<"December">>.

%% 年月日的数字version
-spec dateNumber() -> integer().
dateNumber() ->
   {Year, Month, Day} = curDate(),
   Year * 10000 + Month * 100 + Day.

%% 年月日的数字version
-spec dateNumber(date()) -> integer().
dateNumber({Year, Month, Day}) ->
   Year * 10000 + Month * 100 + Day.

%% 年月日的数字version
-spec dateNumber(Year :: year(), Month :: month(), Day :: day()) -> integer().
dateNumber(Year, Month, Day) ->
   Year * 10000 + Month * 100 + Day.

%% dateNumber 反转
-spec numberDate(DateNumber :: integer()) -> date().
numberDate(DateNumber) ->
   Y = DateNumber div 10000,
   M = DateNumber rem 10000 div 100,
   D = DateNumber rem 100,
   {Y, M, D}.

%% 秒转为天数 和 time()
-spec secToDayTime(Secs :: timestamp()) -> {Day :: integer(), Time :: time()}.
secToDayTime(Secs) ->
   Days0 = Secs div ?SECS_DAY,
   Secs0 = Secs rem ?SECS_DAY,
   Hour = Secs0 div ?SECS_HOUR,
   Secs1 = Secs0 rem ?SECS_HOUR,
   Minute = Secs1 div ?SECS_MIN,
   Second = Secs1 rem ?SECS_MIN,
   {Days0, {Hour, Minute, Second}}.

%% 计算两个本地datetime() 时间差 单位 秒
-spec diffLDateTimeSec(datetime(), datetime()) -> timestamp().
diffLDateTimeSec(DateTime1, DateTime2) ->
   Secs = lDateTimeToSec(DateTime1) - lDateTimeToSec(DateTime2),
   erlang:abs(Secs).

%% 计算两个世界datetime() 时间差 单位 秒
-spec diffUDateTimeSec(datetime(), datetime()) -> timestamp().
diffUDateTimeSec(DateTime1, DateTime2) ->
   Secs = uDateTimeToSec(DateTime1) - uDateTimeToSec(DateTime2),
   erlang:abs(Secs).

%% 计算两个本地datetime() 时间差 单位 daytime()
-spec diffLDateTimeDayTime(datetime(), datetime()) -> {integer(), time()}. %% [修正] Spec类型
diffLDateTimeDayTime(DateTime1, DateTime2) ->
   Secs = lDateTimeToSec(DateTime1) - lDateTimeToSec(DateTime2),
   secToDayTime(erlang:abs(Secs)).

%% 计算两个世界datetime() 时间差 单位 daytime()
-spec diffUDateTimeDayTime(datetime(), datetime()) -> {integer(), time()}. %% [修正] Spec类型
diffUDateTimeDayTime(DateTime1, DateTime2) ->
   Secs = uDateTimeToSec(DateTime1) - uDateTimeToSec(DateTime2),
   secToDayTime(erlang:abs(Secs)).

%% 计算两个秒单位的 时间戳 的时间差 单位 daytime()
-spec diffSecs(timestamp(), timestamp()) -> {integer(), time()}. %% [修正] Spec类型
diffSecs(Sec1, Sec2) ->
   secToDayTime(erlang:abs(Sec1 - Sec2)).

%% 转换 time() 为 Sec
-spec timeToSec(time()) -> timestamp().
timeToSec({H, M, S}) ->
   H * ?SECS_HOUR + M * ?SECS_MIN + S.

%% 计算 Date为该年的哪一天
-spec daysInYear(date()) -> integer().
daysInYear({Y, _, _} = Date) ->
   date_to_gregorian_days(Date) - date_to_gregorian_days({Y, 1, 1}).

%%  Data to Str
-spec dateToStr(date()) -> binary().
dateToStr({Year, Month, Day}) ->
   <<(integer_to_binary(Year))/binary, "-", (i2b(Month))/binary, "-", (i2b(Day))/binary>>.

%%  Data to Str
-spec dateToStr(year(), month(), day()) -> binary().
dateToStr(Year, Month, Day) ->
   <<(integer_to_binary(Year))/binary, "-", (i2b(Month))/binary, "-", (i2b(Day))/binary>>.

%%  Data to Str
-spec dateToStr(date(), binary()) -> binary().
dateToStr({Year, Month, Day}, Separator) ->
   <<(integer_to_binary(Year))/binary, Separator/binary, (i2b(Month))/binary, Separator/binary, (i2b(Day))/binary>>.

%%  Data to Str
-spec dateToStr(year(), month(), day(), binary()) -> binary().
dateToStr(Year, Month, Day, Separator) ->
   <<(integer_to_binary(Year))/binary, Separator/binary, (i2b(Month))/binary, Separator/binary, (i2b(Day))/binary>>.

%%  time to Str
-spec timeToStr(time()) -> binary().
timeToStr({Hour, Minute, Second}) ->
   <<(i2b(Hour))/binary, ":", (i2b(Minute))/binary, ":", (i2b(Second))/binary>>.

%%  time to Str
-spec timeToStr(hour(), minute(), second()) -> binary().
timeToStr(Hour, Minute, Second) ->
   <<(i2b(Hour))/binary, ":", (i2b(Minute))/binary, ":", (i2b(Second))/binary>>.

%%  time to Str
-spec timeToStr(time(), binary()) -> binary().
timeToStr({Hour, Minute, Second}, Separator) ->
   <<(i2b(Hour))/binary, Separator/binary, (i2b(Minute))/binary, Separator/binary, (i2b(Second))/binary>>.

%%  time to Str
-spec timeToStr(hour(), minute(), second(), binary()) -> binary().
timeToStr(Hour, Minute, Second, Separator) ->
   <<(i2b(Hour))/binary, Separator/binary, (i2b(Minute))/binary, Separator/binary, (i2b(Second))/binary>>.

%%  datetime to Str
-spec dateTimeStr(datetime()) -> binary().
dateTimeStr({{Year, Month, Day}, {Hour, Minute, Second}}) ->
   <<(integer_to_binary(Year))/binary, "-", (i2b(Month))/binary, "-", (i2b(Day))/binary, " ", (i2b(Hour))/binary, ":", (i2b(Minute))/binary, ":", (i2b(Second))/binary>>.

i2b(Num) ->
   if
      Num < 10 ->
         <<"0", (integer_to_binary(Num))/binary>>;
      true ->
         integer_to_binary(Num)
   end.

i3b(Num) ->
   if
      Num < 10 ->
         <<"00", (integer_to_binary(Num))/binary>>;
      Num < 100 ->
         <<"0", (integer_to_binary(Num))/binary>>;
      true ->
         integer_to_binary(Num)
   end.