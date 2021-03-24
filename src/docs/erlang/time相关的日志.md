erlang 模块的时间函数---------》》》》

localtime_to_universaltime/1
    如果基础操作系统支持，则将本地日期和时间转换为世界标准时间（UTC）. 否则，不进行任何转换，并 返回Localtime

localtime_to_universaltime/2
    将本地日期和时间转换为erlang：localtime_to_universaltime / 1，以协调世界时（UTC），但调用者确定夏令时是否处于活动状态。
    如果IsDst == true，则本地 时间位于夏令时，如果IsDst == false ，则不是。如果IsDst == undefined，则底层操作系统可以猜测，
    这与调用 erlang：localtime_to_universaltime（Localtime）相同。

universaltime_to_localtime/1
    如果基础操作系统支持，则以{{Year，Month，Day}，{Hour，Minute，Second}}的形式将世界标准时间（UTC）日期和时间转换为本地日期和时间 。
    否则，不进行任何转换，并 返回Universaltime。例：

time/0
    以{Hour，Minute，Second}的形式返回当前时间。时区和夏令时校正取决于基础操作系统。
date/0
    返回当前日期为{Year，Month，Day}。时区和夏令时校正取决于基础操作系统。

localtime/0
    返回当前的本地日期和时间 {{Year，Month，Day}，{Hour，Minute，Second}} 时区和夏令时校正取决于基础操作系统。

universaltime/0
    如果基础操作系统支持，则根据世界标准时间（UTC）以{{Year，Month，Day}，{Hour，Minute，Second}}的形式返回当前日期和时间 。
    否则，erlang：universaltime（）等效于 erlang：localtime（）。例：

posixtime_to_universaltime/1
    posixtime 转为 universaltime
universaltime_to_posixtime/1
    universaltime换为posixtime时间戳

system_time/0,
    以本地时间单位返回当前的 Erlang系统时间。

system_time/1
    返回当前的 Erlang系统时间， 该时间已转换为作为参数传递的Unit。

convert_time_unit/3
    转换时间的时间单位的值 FromUnit到相应 ConvertedTime时间单元的值 ToUnit。使用下限功能对结果进行四舍五入。
    警告:在时间单位之间进行转换时，可能会失去准确性和精度。为了最大程度地减少此类损失，请以本地时间单位收集所有数据，然后对最终结果进行转换。
time_offset/0
    以 本地时间单位返回Erlang单调时间和 Erlang系统时间之间的当前时间偏移 。添加到Erlang单调时间的当前时间偏移会给出相应的Erlang系统时间。
    时间偏移可能会或可能不会在操作期间更改，具体取决于所使用的时间扭曲模式。
    注意:
    通过不同的过程，可以在稍有不同的时间点观察到时间偏移量的变化。
    如果运行时系统处于 多时间扭曲模式，则当运行时系统检测到OS系统时间已更改时，时间偏移也会 更改。但是，运行时系统不会立即检测到它。
    检查时间偏移的任务计划至少每分钟执行一次；因此，在正常操作下，一分钟内即可检测到，但是在重负载下可能需要更长的时间。

time_offset/1
    返回Erlang单调时间和 Erlang系统时间之间的当前时间偏移， 该时间已转换为作为参数传递的Unit。

timestamp/0
    以{MegaSecs，Secs，MicroSecs}格式返回当前的 Erlang系统时间。此格式与os：timestamp / 0 和不赞成使用的erlang：now / 0相同 。
    存在erlang：timestamp（）的原因纯粹是为了简化对采用这种时间戳格式的现有代码的使用。可以使用erlang：system_time / 1以您选择的时间单位
    更有效地检索当前Erlang系统时间 。
    The erlang:timestamp() BIF is equivalent to:
    timestamp() ->
        ErlangSystemTime = erlang:system_time(microsecond),
        MegaSecs = ErlangSystemTime div 1000000000000,
        Secs = ErlangSystemTime div 1000000 - MegaSecs*1000000,
        MicroSecs = ErlangSystemTime rem 1000000,
        {MegaSecs, Secs, MicroSecs}.


calendar 时间模块 -------》》》》》
    模块总结
        本地和世界时间，星期几，日期和时间转换。
    描述
        此模块提供本地和通用时间，星期几以及许多时间转换功能的计算。

        根据当前时区和夏令时进行调整时，时间是本地时间。当它反映的是经度为零的时间时，它是通用的，无需为夏时制进行任何调整。
        世界标准时间（UTC）时间也称为格林威治标准时间（GMT）。

        此模块中的时间函数local_time / 0和 Universal_time / 0都返回日期和时间。这是因为日期和时间的单独功能可能导致日期/时间组合错开24小时。
        如果其中一个功能在午夜之前调用，而另一个功能在午夜之后调用，则会发生这种情况。此问题也适用于Erlang BIF date / 0和time / 0，
        如果需要可靠的日期/时间戳，强烈建议不要使用它们。

        所有日期均符合公历。此历法由教皇格雷戈里十三世在1582年引入，从今年开始在所有天主教国家中使用。德国和荷兰的新教部分在1698年采用了它，
        英格兰随后在1752年采用了，俄国在1918年（根据格里高利历法，1917年10月的革命发生在11月）。

        此模块中的公历将回溯到0年。对于给定的日期，公历天数是指指定日期之前（包括该日期）的天数。同样，指定日期和时间的公历秒数是直至并包括指定日期和时间的秒数。

        要计算时间间隔之间的差异，请使用计算公历天或秒的功能。如果将纪元指定为本地时间，则必须将其转换为通用时间，以获取各纪元之间经过时间的正确值
        。不建议使用功能time_difference / 2。

        一年中的一周存在不同的定义。该模块包含符合ISO 8601标准的一年中的一周实施。由于指定日期的星期数可以落在上一个，当前或下一年，
        因此指定年号和星期数很重要。函数iso_week_number / 0和iso_week_number / 1 返回年份和星期数的元组。
calendar:

date_to_gregorian_days/1
date_to_gregorian_days/3
    计算从0年开始到指定日期结束的公历天数。

datetime_to_gregorian_seconds/1
    计算从年份0开始到指定的日期和时间的公历秒数。

gregorian_days_to_date/1
    根据指定的公历天数计算日期。
gregorian_seconds_to_datetime/1
    根据指定的公历秒数计算日期和时间。

day_of_the_week/1
day_of_the_week/3
    从指定的Year，Month和 Day计算星期几 。将星期几返回为 1：星期一，2：星期二，依此类推。

is_leap_year/1
    检查指定的年份是否为闰年。

iso_week_number/0
    返回表示实际日期的ISO周编号的元组{Year，WeekNum}。要确定实际日期，请使用函数 local_time / 0。
iso_week_number/1
    返回表示指定日期的ISO周编号的元组{Year，WeekNum}。

last_day_of_the_month/2
    计算一个月中的天数。

local_time/0
    等效于 erlang:localtime()

local_time_to_universal_time/1              %% 不推荐使用了 额豁
    从本地时间转换为世界标准时间（UTC）。 DateTime1必须引用1970年1月1日之后的本地日期。
    警告：不推荐使用此功能。请改用 local_time_to_universal_time_dst / 1 ，因为它可以提供更正确和完整的结果。
    尤其是对于不存在的时间段，由于在切换到夏时制时会被跳过，因此此功能仍会返回结果。
local_time_to_universal_time/2

local_time_to_universal_time_dst/1
    从本地时间转换为世界标准时间（UTC）。 参数DateTime1必须引用1970年1月1日之后的本地日期。

    返回值是0、1或2个可能的UTC时间的列表：

    []
    对于当地时间{Date1，Time1}，在切换到夏令时时会跳过该时间段，因此没有相应的UTC，因为当地时间是非法的（从未发生过）。

    [DstDateTimeUTC，DateTimeUTC]
    对于从夏令时开始重复的时段中的本地{Date1，Time1}，存在两个对应的UTC；一个用于夏令时仍处于活动状态的时段的第一个实例，另一个用于第二个实例。

    [DateTimeUTC]
    对于所有其他本地时间，仅存在一个对应的UTC。

now_to_datetime/1			% = now_to_universal_time/1
    返回从erlang：timestamp / 0的返回值转换的通用协调时间（UTC） 。

now_to_local_time/1
    返回从erlang：timestamp / 0的返回值转换的本地日期和时间 。

now_to_universal_time/1
     返回从erlang：timestamp / 0的返回值转换的通用协调时间（UTC） 。

rfc3339_to_system_time/1
rfc3339_to_system_time/2
        将RFC 3339时间戳转换为系统时间。RFC 3339描述了RFC 3339时间戳的数据格式 。

seconds_to_daystime/1
    将指定的秒数转换为天，小时，分钟和秒。时间始终是非负的，但是如果参数Seconds是，则 Days是负的 。

seconds_to_time/1
    根据指定的秒数计算时间。 秒数必须小于每天的秒数（86400）。

system_time_to_local_time/2
    将指定的系统时间转换为本地日期和时间。     TODO 优化此函数

system_time_to_universal_time/2         TODO 优化此函数
    将指定的系统时间转换为通用日期和时间。

system_time_to_rfc3339/1
system_time_to_rfc3339/2
    将系统时间转换为RFC 3339时间戳。RFC 3339描述了RFC 3339时间戳的数据格式 。偏移量的数据格式也由RFC 3339描述。

time_difference/2           %% 改函数过时 不用
time_to_seconds/1
    返回自午夜到指定时间的秒数。

universal_time/0
    等效于erlang:universaltime().

universal_time_to_local_time/1
    erlang:universaltime_to_localtime(DateTime).

valid_date/1
valid_date/3
    此功能检查日期是否有效。