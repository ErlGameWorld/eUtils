-module(utTime).
-include("utTime.hrl").

-import(calendar, [day_of_the_week/1, day_of_the_week/3, iso_week_number/1, date_to_gregorian_days/1]).

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
   , timeZoneDiff/0                 %% 获取本地时区与UTC时区 时间差 单位秒 由于某些时区存在夏令时 下面的计算在不同时候 可能会返回不同的值 所以并不能只计算一次 需要在每次用到的时候 重新计算
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
   , countLDay/2                    %% 计算1970年到Sec 本地经过了多少天 ZeroOffset为零点偏移时间
   , countUDay/2                    %% 计算1970年到Sec 世界经过了多少天 ZeroOffset为零点偏移时间
   , isSameLDay/3                   %% 判断两个时间戳是否为同一天 ZeroOffset为零点偏移时间
   , isSameUDay/3                   %% 判断两个时间戳是否为同一天 ZeroOffset为零点偏移时间
   , countLWeek/2                   %% 计算1970年到Sec 本地经过了多少周 ZeroOffset为零点偏移时间
   , countUWeek/2                   %% 计算1970年到Sec 世界经过了多少周 ZeroOffset为零点偏移时间
   , isSameLWeek/3                  %% 判断两个时间戳是否为同一周 ZeroOffset为零点偏移时间
   , isSameUWeek/3                  %% 判断两个时间戳是否为同一周 ZeroOffset为零点偏移时间
   , isSameLMonth/3                 %% 判断两个时间戳是否为本地的同一月 ZeroOffset为零点偏移时间
   , isSameUMonth/3                 %% 判断两个时间戳是否为世界的同一月 ZeroOffset为零点偏移时间
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
   , timeToSecs/1                   %% 转换 time() 为 Sec
   , daysInYear/1                   %% 计算 Date为该年的哪一天
   , dateToStr/1                    %%  Data to Str
   , dateToStr/3                    %%  Data to Str
   , dateToStr/2                    %%  Data to Str
   , dateToStr/4                    %%  Data to Str
   , timeToStr/1                    %%  time to Str
   , timeToStr/3                    %%  time to Str
   , timeToStr/2                    %%  time to Str
   , timeToStr/4                    %%  time to Str
   , dateTimeStr/1                  %%  datetime to Str
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
   erlang:localtime().

%% 当前的年月日
-spec curDate() -> date().
curDate() ->
   erlang:date().

%% 当前的时分秒
-spec curTime() -> time().
curTime() ->
   erlang:time().

%% 当前星期几
-spec weekDay() -> week().
weekDay() ->
   day_of_the_week(erlang:date()).

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
   iso_week_number(erlang:date()).

%% 计算 Date 的星期周期
-spec weekCycle(Date :: date()) -> yearWeekCycle().
weekCycle(Date) ->
   iso_week_number(Date).

%% 将秒单位的时间戳 转为本地 datetime()
-spec secToLDateTime(Sec :: timestamp()) -> datetime().
secToLDateTime(Ses) ->
   erlang:universaltime_to_localtime(erlang:posixtime_to_universaltime(Ses)).

%% 将秒单位的时间戳 转为世界 datetime()
-spec secToUDateTime(Sec :: timestamp()) -> datetime().
secToUDateTime(Ses) ->
   erlang:posixtime_to_universaltime(Ses).

%% 将本地的 datetime() 转为秒单位的时间戳
-spec lDateTimeToSec(LocalDate :: datetime()) -> timestamp().
lDateTimeToSec(LocalDate) ->
   erlang:universaltime_to_posixtime(erlang:localtime_to_universaltime(LocalDate)).

%% 将世界的 datetime() 转为秒单位的时间戳
-spec uDateTimeToSec(UniversalTime :: datetime()) -> timestamp().
uDateTimeToSec(UniversalTime) ->
   erlang:universaltime_to_posixtime(UniversalTime).

%% 获取本地时区与UTC时区 时间差 单位秒 由于某些时区存在夏令时 下面的计算在不同时候 可能会返回不同的值 所以并不能只计算一次 需要在每次用到的时候 重新计算
-spec timeZoneDiff() -> timestamp().
timeZoneDiff() ->
   Local2001 = erlang:universaltime_to_localtime({{2001, 1, 1}, {0, 0, 0}}),
   erlang:universaltime_to_posixtime(Local2001) - 978307200.

%% 计算1970年到Sec 本地经过了多少天
-spec countLDay(Sec :: timestamp()) -> integer().
countLDay(Sec) ->
   (Sec + timeZoneDiff()) div ?SECS_DAY.

%% 计算1970年到Sec 世界经过了多少天
-spec countUDay(Sec :: timestamp()) -> integer().
countUDay(Sec) ->
   Sec div ?SECS_DAY.

%% 判断两个时间戳是否为本地的同一天
-spec isSameLDay(Sec1 :: timestamp(), Sec2 :: timestamp()) -> boolean().
isSameLDay(Sec1, Sec2) ->
   TimeZoneDiff = timeZoneDiff(),
   (Sec1 + TimeZoneDiff) div ?SECS_DAY =:= (Sec2 + TimeZoneDiff) div ?SECS_DAY.

%% 判断两个时间戳是否为本地的同一天
-spec isSameUDay(Sec1 :: timestamp(), Sec2 :: timestamp()) -> boolean().
isSameUDay(Sec1, Sec2) ->
   Sec1 div ?SECS_DAY =:= Sec2 div ?SECS_DAY.

%% 计算1970年到Sec 本地经过了多少周
-spec countLWeek(Sec :: timestamp()) -> integer().
countLWeek(Sec) ->
   (Sec + 259200 + timeZoneDiff()) div ?SECS_WEEK.

%% 计算1970年到Sec 世界经过了多少周
-spec countUWeek(Sec :: timestamp()) -> integer().
countUWeek(Sec) ->
   (Sec + 259200) div ?SECS_WEEK.

%% 判断两个时间戳是否为本地的同一周
-spec isSameLWeek(Sec1 :: timestamp(), Sec2 :: timestamp()) -> boolean().
isSameLWeek(Sec1, Sec2) ->
   TimeZoneDiff = timeZoneDiff(),
   (Sec1 + 259200 + TimeZoneDiff) div ?SECS_WEEK =:= (Sec2 + 259200 + TimeZoneDiff) div ?SECS_WEEK.

%% 判断两个时间戳是否为本地的同一周
-spec isSameUWeek(Sec1 :: timestamp(), Sec2 :: timestamp()) -> boolean().
isSameUWeek(Sec1, Sec2) ->
   (Sec1 + 259200) div ?SECS_WEEK =:= (Sec2 + 259200) div ?SECS_WEEK.

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
   (Sec - ZeroOffset + timeZoneDiff()) div ?SECS_DAY.

%% 计算1970年到Sec 世界经过了多少天 ZeroOffset为零点偏移时间
-spec countUDay(Sec :: timestamp(), ZeroOffset :: timestamp()) -> integer().
countUDay(Sec, ZeroOffset) ->
   (Sec - ZeroOffset) div ?SECS_DAY.

%% 判断两个时间戳是否为同一天 ZeroOffset为零点偏移时间
-spec isSameLDay(Sec1 :: timestamp(), Sec2 :: timestamp(), ZeroOffset :: timestamp()) -> boolean().
isSameLDay(Sec1, Sec2, ZeroOffset) ->
   TimeZoneDiff = timeZoneDiff(),
   (Sec1 - ZeroOffset + TimeZoneDiff) div ?SECS_DAY =:= (Sec2 - ZeroOffset + TimeZoneDiff) div ?SECS_DAY.

%% 判断两个时间戳是否为同一天 ZeroOffset为零点偏移时间
-spec isSameUDay(Sec1 :: timestamp(), Sec2 :: timestamp(), ZeroOffset :: timestamp()) -> boolean().
isSameUDay(Sec1, Sec2, ZeroOffset) ->
   (Sec1 - ZeroOffset) div ?SECS_DAY =:= (Sec2 - ZeroOffset) div ?SECS_DAY.

%% 计算1970年到Sec 本地经过了多少周 ZeroOffset为零点偏移时间
-spec countLWeek(Sec :: timestamp(), ZeroOffset :: timestamp()) -> integer().
countLWeek(Sec, ZeroOffset) ->
   (Sec - ZeroOffset + 259200 + timeZoneDiff()) div ?SECS_WEEK.

%% 计算1970年到Sec 世界经过了多少周 ZeroOffset为零点偏移时间
-spec countUWeek(Sec :: timestamp(), ZeroOffset :: timestamp()) -> integer().
countUWeek(Sec, ZeroOffset) ->
   (Sec - ZeroOffset + 259200) div ?SECS_WEEK.

%% 判断两个时间戳是否为同一周 ZeroOffset为零点偏移时间
-spec isSameLWeek(Sec1 :: timestamp(), Sec2 :: timestamp(), ZeroOffset :: timestamp()) -> boolean().
isSameLWeek(Sec1, Sec2, ZeroOffset) ->
   TimeZoneDiff = timeZoneDiff(),
   (Sec1 - ZeroOffset + 259200 + TimeZoneDiff) div ?SECS_WEEK =:= (Sec2 - ZeroOffset + 259200 + TimeZoneDiff) div ?SECS_WEEK.

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
   Sec = erlang:system_time(second),
   Sec - Sec rem ?SECS_HOUR.

%% Sec所在小时开始时间戳
-spec hourBegin(Sec :: timestamp()) -> timestamp().
hourBegin(Sec) ->
   Sec - Sec rem ?SECS_HOUR.

%% 当前小时结束时间戳
-spec hourEnd() -> timestamp().
hourEnd() ->
   Sec = erlang:system_time(second),
   Sec - Sec rem ?SECS_HOUR + ?SECS_HOUR.

%% Sec所在小时结束时间戳
-spec hourEnd(Sec :: timestamp()) -> timestamp().
hourEnd(Sec) ->
   Sec - Sec rem ?SECS_HOUR + ?SECS_HOUR.

%% 本地当前天开始时间戳
-spec dayLBegin() -> timestamp().
dayLBegin() ->
   Sec = erlang:system_time(second),
   Sec - (Sec + timeZoneDiff()) rem ?SECS_DAY.

%% 本地Sec所在天开始时间戳
-spec dayLBegin(Sec :: timestamp()) -> timestamp().
dayLBegin(Sec) ->
   Sec - (Sec + timeZoneDiff()) rem ?SECS_DAY.

%% 世界Sec所在天开始时间戳
-spec dayUBegin() -> timestamp().
dayUBegin() ->
   Sec = erlang:system_time(second),
   Sec - Sec rem ?SECS_DAY.

%% 世界Sec所在天开始时间戳
-spec dayUBegin(Sec :: timestamp()) -> timestamp().
dayUBegin(Sec) ->
   Sec - Sec rem ?SECS_DAY.

%% 本地当前天结束时间戳
-spec dayLEnd() -> timestamp().
dayLEnd() ->
   Sec = erlang:system_time(second),
   Sec - (Sec + timeZoneDiff()) rem ?SECS_DAY + ?SECS_DAY.

%% 本地Sec所在天结束时间戳
-spec dayLEnd(Sec :: timestamp()) -> timestamp().
dayLEnd(Sec) ->
   Sec - (Sec + timeZoneDiff()) rem ?SECS_DAY + ?SECS_DAY.

%% 世界当前天结束时间戳
-spec dayUEnd() -> timestamp().
dayUEnd() ->
   Sec = erlang:system_time(second),
   Sec - Sec rem ?SECS_DAY + ?SECS_DAY.

%% 世界Sec所在天结束时间戳
-spec dayUEnd(Sec :: timestamp()) -> timestamp().
dayUEnd(Sec) ->
   Sec - Sec rem ?SECS_DAY + ?SECS_DAY.

%% 本地当前天开始结束时间戳
-spec dayLBeginEnd() -> timestamp().
dayLBeginEnd() ->
   Sec = erlang:system_time(second),
   Begin = Sec - (Sec + timeZoneDiff()) rem ?SECS_DAY,
   {Begin, Begin + ?SECS_DAY}.

%% 本地Sec所在天开始结束时间戳
-spec dayLBeginEnd(Sec :: timestamp()) -> timestamp().
dayLBeginEnd(Sec) ->
   Begin = Sec - (Sec + timeZoneDiff()) rem ?SECS_DAY,
   {Begin, Begin + ?SECS_DAY}.

%% 世界当前天开始结束时间戳
-spec dayUBeginEnd() -> timestamp().
dayUBeginEnd() ->
   Sec = erlang:system_time(second),
   Begin = Sec - Sec rem ?SECS_DAY,
   {Begin, Begin + ?SECS_DAY}.

%% 世界Sec所在天开始结束时间戳
-spec dayUBeginEnd(Sec :: timestamp()) -> timestamp().
dayUBeginEnd(Sec) ->
   Begin = Sec - Sec rem ?SECS_DAY,
   {Begin, Begin + ?SECS_DAY}.

%% 本地当前周开始时间戳
-spec weekLBegin() -> timestamp().
weekLBegin() ->
   Sec = erlang:system_time(second),
   Sec - (Sec - 345600 + timeZoneDiff()) rem ?SECS_WEEK.

%% 本地Sec所在周开始时间戳
-spec weekLBegin(Sec :: timestamp()) -> timestamp().
weekLBegin(Sec) ->
   Sec - (Sec - 345600 + timeZoneDiff()) rem ?SECS_WEEK.

%% 世界当前周的开始时间戳
-spec weekUBegin() -> timestamp().
weekUBegin() ->
   Sec = erlang:system_time(second),
   Sec - (Sec - 345600) rem ?SECS_WEEK.

%% 世界Sec所在周的开始时间戳
-spec weekUBegin(Sec :: timestamp()) -> timestamp().
weekUBegin(Sec) ->
   Sec - (Sec - 345600) rem ?SECS_WEEK.

%% 本地当前周的结束时间戳
-spec weekLEnd() -> timestamp().
weekLEnd() ->
   Sec = erlang:system_time(second),
   Sec - (Sec - 345600 + timeZoneDiff()) rem ?SECS_WEEK + ?SECS_WEEK.

%% 本地Sec所在周的结束时间戳
-spec weekLEnd(Sec :: timestamp()) -> timestamp().
weekLEnd(Sec) ->
   Sec - (Sec - 345600 + timeZoneDiff()) rem ?SECS_WEEK + ?SECS_WEEK.

%% 世界当前周的开始时间戳
-spec weekUEnd() -> timestamp().
weekUEnd() ->
   Sec = erlang:system_time(second),
   Sec - (Sec - 345600) rem ?SECS_WEEK + ?SECS_WEEK.

%% 世界Sec所在周的结束时间戳
-spec weekUEnd(Sec :: timestamp()) -> timestamp().
weekUEnd(Sec) ->
   Sec - (Sec - 345600) rem ?SECS_WEEK + ?SECS_WEEK.

%% 本地当前周的开始结束时间戳
-spec weekLBeginEnd() -> timestamp().
weekLBeginEnd() ->
   Sec = erlang:system_time(second),
   Begin = Sec - (Sec - 345600 + timeZoneDiff()) rem ?SECS_WEEK,
   {Begin, Begin + ?SECS_WEEK}.

%% 本地Sec所在周的开始结束时间戳
-spec weekLBeginEnd(Sec :: timestamp()) -> timestamp().
weekLBeginEnd(Sec) ->
   Begin = Sec - (Sec - 345600 + timeZoneDiff()) rem ?SECS_WEEK,
   {Begin, Begin + ?SECS_WEEK}.

%% 世界当前周的开始结束时间戳
-spec weekUBeginEnd() -> timestamp().
weekUBeginEnd() ->
   Sec = erlang:system_time(second),
   Begin = Sec - (Sec - 345600) rem ?SECS_WEEK,
   {Begin, Begin + ?SECS_WEEK}.

%% 世界Sec所在周的开始结束时间戳
-spec weekUBeginEnd(Sec :: timestamp()) -> timestamp().
weekUBeginEnd(Sec) ->
   Begin = Sec - (Sec - 345600) rem ?SECS_WEEK,
   {Begin, Begin + ?SECS_WEEK}.

%% 本地当前月的开始时间戳
-spec monthLBegin() -> timestamp().
monthLBegin() ->
   Sec = erlang:system_time(second),
   {{Year, Month, _Day}, _Time} = secToLDateTime(Sec),
   MonthStartDateTime = {{Year, Month, 1}, {0, 0, 0}},
   lDateTimeToSec(MonthStartDateTime).

%% 本地Sec所在月的开始时间戳
-spec monthLBegin(Sec :: timestamp()) -> timestamp().
monthLBegin(Sec) ->
   {{Year, Month, _Day}, _Time} = secToLDateTime(Sec),
   MonthStartDateTime = {{Year, Month, 1}, {0, 0, 0}},
   lDateTimeToSec(MonthStartDateTime).

%% 世界当前月的开始时间戳
-spec monthUBegin() -> timestamp().
monthUBegin() ->
   Sec = erlang:system_time(second),
   {{Year, Month, _Day}, _Time} = erlang:posixtime_to_universaltime(Sec),
   MonthStartDateTime = {{Year, Month, 1}, {0, 0, 0}},
   erlang:universaltime_to_posixtime(MonthStartDateTime).

%% 世界Sec所在月的开始时间戳
-spec monthUBegin(Sec :: timestamp()) -> timestamp().
monthUBegin(Sec) ->
   {{Year, Month, _Day}, _Time} = erlang:posixtime_to_universaltime(Sec),
   MonthStartDateTime = {{Year, Month, 1}, {0, 0, 0}},
   erlang:universaltime_to_posixtime(MonthStartDateTime).

%% 本地当前月的结束时间戳
-spec monthLEnd() -> timestamp().
monthLEnd() ->
   Sec = erlang:system_time(second),
   {{Year, Month, _Day}, _Time} = secToLDateTime(Sec),
   MonthDay = monthDay(Year, Month),
   MonthEndDateTime = {{Year, Month, MonthDay}, {23, 59, 59}},
   lDateTimeToSec(MonthEndDateTime).

%% 本地Sec所在月的结束时间戳
-spec monthLEnd(Sec :: timestamp()) -> timestamp().
monthLEnd(Sec) ->
   {{Year, Month, _Day}, _Time} = secToLDateTime(Sec),
   MonthDay = monthDay(Year, Month),
   MonthEndDateTime = {{Year, Month, MonthDay}, {23, 59, 59}},
   lDateTimeToSec(MonthEndDateTime).

%% 世界当前周的结束时间戳
-spec monthUEnd() -> timestamp().
monthUEnd() ->
   Sec = erlang:system_time(second),
   {{Year, Month, _Day}, _Time} = erlang:posixtime_to_universaltime(Sec),
   MonthDay = monthDay(Year, Month),
   MonthEndDateTime = {{Year, Month, MonthDay}, {23, 59, 59}},
   erlang:universaltime_to_posixtime(MonthEndDateTime).

%% 世界Sec所在月的结束时间戳
-spec monthUEnd(Sec :: timestamp()) -> timestamp().
monthUEnd(Sec) ->
   {{Year, Month, _Day}, _Time} = erlang:posixtime_to_universaltime(Sec),
   MonthDay = monthDay(Year, Month),
   MonthEndDateTime = {{Year, Month, MonthDay}, {23, 59, 59}},
   erlang:universaltime_to_posixtime(MonthEndDateTime).

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
-spec monthLBeginEnd() -> timestamp().
monthLBeginEnd() ->
   Sec = erlang:system_time(second),
   {{Year, Month, _Day}, _Time} = secToLDateTime(Sec),
   MonthStartDateTime = {{Year, Month, 1}, {0, 0, 0}},
   Begin = lDateTimeToSec(MonthStartDateTime),
   {Begin, Begin + monthSecs(Year, Month)}.

%% 本地Sec所在月的开始结束时间戳
-spec monthLBeginEnd(Sec :: timestamp()) -> timestamp().
monthLBeginEnd(Sec) ->
   {{Year, Month, _Day}, _Time} = secToLDateTime(Sec),
   MonthStartDateTime = {{Year, Month, 1}, {0, 0, 0}},
   Begin = lDateTimeToSec(MonthStartDateTime),
   {Begin, Begin + monthSecs(Year, Month)}.

%% 世界当前月的开始结束时间戳
-spec monthUBeginEnd() -> timestamp().
monthUBeginEnd() ->
   Sec = erlang:system_time(second),
   {{Year, Month, _Day}, _Time} = erlang:posixtime_to_universaltime(Sec),
   MonthStartDateTime = {{Year, Month, 1}, {0, 0, 0}},
   Begin = erlang:universaltime_to_posixtime(MonthStartDateTime),
   {Begin, Begin + monthSecs(Year, Month)}.

%% 世界Sec所在月的开始结束时间戳
-spec monthUBeginEnd(Sec :: timestamp()) -> timestamp().
monthUBeginEnd(Sec) ->
   {{Year, Month, _Day}, _Time} = erlang:posixtime_to_universaltime(Sec),
   MonthStartDateTime = {{Year, Month, 1}, {0, 0, 0}},
   Begin = erlang:universaltime_to_posixtime(MonthStartDateTime),
   {Begin, Begin + monthSecs(Year, Month)}.

%% 星期名字缩写
-spec sWeekName(week()) -> string().
sWeekName(1) -> <<"Mon">>;
sWeekName(2) -> <<"Tue">>;
sWeekName(3) -> <<"Wed">>;
sWeekName(4) -> <<"Thu">>;
sWeekName(5) -> <<"Fri">>;
sWeekName(6) -> <<"Sat">>;
sWeekName(7) -> <<"Sun">>.

%% 星期名字全称
-spec lWeekName(week()) -> string().
lWeekName(1) -> <<"Monday">>;
lWeekName(2) -> <<"Tuesday">>;
lWeekName(3) -> <<"Wednesday">>;
lWeekName(4) -> <<"Thursday">>;
lWeekName(5) -> <<"Friday">>;
lWeekName(6) -> <<"Saturday">>;
lWeekName(7) -> <<"Sunday">>.

%% 月份名称缩写
-spec sMonthName(month()) -> string().
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
-spec lMonthName(month()) -> string().
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
   {Year, Month, Day} = erlang:date(),
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
-spec diffLDateTimeDayTime(datetime(), datetime()) -> timestamp().
diffLDateTimeDayTime(DateTime1, DateTime2) ->
   Secs = lDateTimeToSec(DateTime1) - lDateTimeToSec(DateTime2),
   secToDayTime(erlang:abs(Secs)).

%% 计算两个世界datetime() 时间差 单位 daytime()
-spec diffUDateTimeDayTime(datetime(), datetime()) -> timestamp().
diffUDateTimeDayTime(DateTime1, DateTime2) ->
   Secs = uDateTimeToSec(DateTime1) - uDateTimeToSec(DateTime2),
   secToDayTime(erlang:abs(Secs)).

%% 计算两个秒单位的 时间戳 的时间差 单位 daytime()
-spec diffSecs(timestamp(), timestamp()) -> timestamp().
diffSecs(Sec1, Sec2) ->
   secToDayTime(erlang:abs(Sec1 - Sec2)).

%% 转换 time() 为 Sec
-spec timeToSecs(time()) -> timestamp().
timeToSecs({H, M, S}) ->
   H * ?SECS_HOUR + M * ?SECS_MIN + S.

%% 计算 Date为该年的哪一天
-spec daysInYear(date()) -> integer().
daysInYear({Y, _, _} = Date) ->
   date_to_gregorian_days(Date) - date_to_gregorian_days({Y, 1, 1}).

%%  Data to Str
-spec dateToStr(date()) -> string().
dateToStr({Year, Month, Day}) ->
   S = io_lib:format("~B_~2.10.0B_~2.10.0B", [Year, Month, Day]),
   lists:flatten(S).

%%  Data to Str
-spec dateToStr(year(), month(), day()) -> string().
dateToStr(Year, Month, Day) ->
   S = io_lib:format("~B_~2.10.0B_~2.10.0B", [Year, Month, Day]),
   lists:flatten(S).

%%  Data to Str
-spec dateToStr(date(), string()) -> string().
dateToStr({Year, Month, Day}, Separator) ->
   S = io_lib:format("~B~w~2.10.0B~w~2.10.0B", [Year, Separator, Month, Separator, Day]),
   lists:flatten(S).

%%  Data to Str
-spec dateToStr(year(), month(), day(), string()) -> string().
dateToStr(Year, Month, Day, Separator) ->
   S = io_lib:format("~B~w~2.10.0B~w~2.10.0B", [Year, Separator, Month, Separator, Day]),
   lists:flatten(S).

%%  time to Str
-spec timeToStr(time()) -> string().
timeToStr({Hour, Minute, Second}) ->
   S = io_lib:format("~B:~2.10.0B:~2.10.0B", [Hour, Minute, Second]),
   lists:flatten(S).

%%  time to Str
-spec timeToStr(hour(), minute(), second()) -> string().
timeToStr(Hour, Minute, Second) ->
   S = io_lib:format("~B:~2.10.0B:~2.10.0B", [Hour, Minute, Second]),
   lists:flatten(S).

%%  time to Str
-spec timeToStr(time(), string()) -> string().
timeToStr({Hour, Minute, Second}, Separator) ->
   S = io_lib:format("~B~w~2.10.0B~w~2.10.0B", [Hour, Separator, Minute, Separator, Second]),
   lists:flatten(S).

%%  time to Str
-spec timeToStr(hour(), minute(), second(), string()) -> string().
timeToStr(Hour, Minute, Second, Separator) ->
   S = io_lib:format("~B~w~2.10.0B~w~2.10.0B", [Hour, Separator, Minute, Separator, Second]),
   lists:flatten(S).

%%  datetime to Str
-spec dateTimeStr(datetime()) -> string().
dateTimeStr({{Year, Month, Day}, {Hour, Minute, Second}}) ->
   S = io_lib:format("~B_~2.10.0B_~2.10.0B ~B:~2.10.0B:~2.10.0B", [Year, Month, Day, Hour, Minute, Second]),
   lists:flatten(S).