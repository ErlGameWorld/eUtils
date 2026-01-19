-module(eNifFtz).
-on_load(init/0).

-export([
   init/0,
   is_available/0,
   zone_offset/0,
   universal_time_to_local_time/1,
   local_time_to_universal_time/1,
   local_time_to_universal_time/3
]).

%% 日期时间类型定义
%% 格式1: {{年, 月, 日}, {时, 分, 秒}}
%% 格式2: {{年, 月, 日}, {时, 分, 秒, 微秒}}
-type datetime() :: {{integer(), integer(), integer()}, {integer(), integer(), integer()}}
| {{integer(), integer(), integer()}, {integer(), integer(), integer(), integer()}}.

%% 歧义时间处理策略 (用于夏令时切换时的重复时间)
%% 0 | undefined: 默认策略（歧义时返回双元素列表）
%% 1 | earliest: 选择夏令时开始前的 UTC 时间（偏移量较大）
%% 2 | latest: 选择夏令时开始后的 UTC 时间（偏移量较小）
%% 3 | strict: 严格模式，歧义或不存在时间返回错误
-type amb_policy() :: undefined | earliest | latest | strict.

%% 夏令时标志 (仅当 amb_policy = 0 或 3 时生效)
%% -1 | undefined: 未指定（根据 strategy 决定）
%% 0 | false: 非夏令时时间 (Standard Time)
%% 1 | true: 夏令时时间 (DST)
-type non_policy() :: undefined | false | true.

%% UTC 转换结果类型
%% 返回 UTC 时间（单元素或双元素列表）
%% - 唯一时间: [{{年, 月, 日}, {时, 分, 秒}}]
%% - 歧义时间: [UTC1, UTC2] (两个可能的 UTC 时间，仅当 strategy=0 时)
%% - 不存在时间: 自动跳转到下一个有效时间 [{{年, 月, 日}, {时, 分, 秒}}]
-type utc_result() :: [datetime()] | {error, timezone_not_loaded | invalid_datetime | ambiguous_time | nonexistent_time | invalid_strategy | invalid_dst_input | internal_error}.

%% 本地时间转换结果类型
%% 返回本地时间 tuple
-type local_result() :: datetime() | {error, timezone_not_loaded | invalid_datetime | internal_error}.

init() ->
   SoName = case code:priv_dir(?MODULE) of
      {error, _} ->
         case code:which(?MODULE) of
            Filename when is_list(Filename) ->
               filename:join([filename:dirname(Filename), "../priv", ?MODULE]);
            _ ->
               filename:join(["priv", ?MODULE])
         end;
      Dir ->
         filename:join(Dir, ?MODULE)
   end,
   erlang:load_nif(SoName, 0).
%% @doc 检查时区库是否可用
-spec is_available() -> boolean().
is_available() ->
   erlang:nif_error(nif_not_loaded).

%% @doc 获取当前时区的 UTC 偏移量（秒）
%% 返回值示例:
%%   - 北京时区: 28800 (UTC+8)
%%   - 东京时区: 32400 (UTC+9)
%%   - 纽约时区: -18000 (UTC-5) 或 -14400 (UTC-4, 夏令时)
%% 注意: 此函数返回当前时刻的偏移量，会根据夏令时自动调整
-spec zone_offset() -> integer() | {error, term()}.
zone_offset() ->
   erlang:nif_error(nif_not_loaded).

%% @doc 将 UTC 时间转换为本地时间
%% 参数: UTC 时间 (datetime 类型)
%% 返回: 本地时间 (datetime 类型)
%% 说明: 
%%   - 返回格式: {{年, 月, 日}, {时, 分, 秒}}
%%   - 不包含 UTC 偏移量和 DST 标志
%% 示例: 
%%   eNifFtz:universal_time_to_local_time({{2024,1,1},{0,0,0}}).
%%   => {{2024,1,1},{8,0,0}}
-spec universal_time_to_local_time(datetime()) -> local_result().
universal_time_to_local_time(_UtcDT) ->
   erlang:nif_error(nif_not_loaded).

%% @doc 将本地时间转换为 UTC 时间（简化版）
%% 参数: 本地时间 (datetime 类型)
%% 返回: UTC 时间 (datetime 类型)
%% 说明: 
%%   - 返回格式: {{年, 月, 日}, {时, 分, 秒}}
%%   - 对于唯一时间，直接返回对应的 UTC 时间
%%   - 对于歧义时间（夏令时切换时的重复时间），使用 earliest 策略
%%   - 对于不存在时间（夏令时切换时的跳过时间），自动跳转到下一个有效时间
%% 注意: 此函数不返回列表，始终返回单个 datetime tuple
-spec local_time_to_universal_time(datetime()) -> datetime() | {error, term()}.
local_time_to_universal_time(_LocalDT) ->
   erlang:nif_error(nif_not_loaded).

%% @doc 将本地时间转换为 UTC 时间（完整版，支持策略控制）
%% 参数1: 本地时间 (datetime 类型)
%% 参数2: 歧义时间处理策略 (amb_policy 类型)
%%   - 0 | undefined: 默认策略（歧义时返回双元素列表 [UTC1, UTC2]）
%%   - 1 | earliest: 选择夏令时开始前的 UTC 时间（偏移量较大）
%%   - 2 | latest: 选择夏令时开始后的 UTC 时间（偏移量较小）
%%   - 3 | strict: 严格模式，歧义或不存在时间返回错误
%% 参数3: 夏令时标志 (non_policy 类型)
%%   - -1 | undefined: 未指定（根据 strategy 决定）
%%   - 0 | false: 非夏令时时间 (Standard Time)
%%   - 1 | true: 夏令时时间 (DST)
%% 
%% 返回: [UTC时间] (单元素或双元素列表)
%%   - 唯一时间: [{{年, 月, 日}, {时, 分, 秒}}]
%%   - 歧义时间且 strategy=0: [UTC1, UTC2] (两个可能的 UTC 时间)
%%   - 其他情况: [{{年, 月, 日}, {时, 分, 秒}}]
%%   - 错误: {error, Reason}
%% 
%% 策略和 DST 参数的交互规则:
%%   - 当 strategy = 1 (earliest) 时，强制设置 dst = true（夏令时）
%%   - 当 strategy = 2 (latest) 时，强制设置 dst = false（非夏令时）
%%   - 当 strategy = 0 (undefined) 时，dst 参数被忽略（歧义时返回双元素列表）
%%   - 当 strategy = 3 (strict) 时，dst 参数生效（歧义时根据 dst 选择，不存在时返回错误）
%% 
%% 使用场景说明:
%% 夏令时切换时会出现两种特殊情况:
%% 
%% 1. 歧义时间 (Ambiguous Time):
%%    时钟从 2:00 跳到 3:00 时，2:30 这个本地时间会出现两次
%%    - 第一次: 2:30 (夏令时前, offset=-5h) => UTC 7:30
%%    - 第二次: 2:30 (夏令时后, offset=-4h) => UTC 6:30
%%    解决方法: 使用策略参数选择其中一个
%% 
%% 2. 不存在时间 (Nonexistent Time):
%%    时钟从 2:00 跳到 3:00 时，2:30 这个本地时间根本不存在
%%    解决方法: 自动跳转到 3:00 (下一个有效时间)，除非 strategy=strict
%% 
%% 示例 (美国东部时区 2024-03-10 02:30:00):
%%   % 使用默认策略（返回双元素列表）
%%   eNifFtz:local_time_to_universal_time({{2024,3,10},{2,30,0}}, undefined, undefined).
%%   => [{{2024,3,10},{7,30,0}}, {{2024,3,10},{6,30,0}}]
%% 
%%   % 使用 earliest 策略（夏令时前，offset=-5h）
%%   eNifFtz:local_time_to_universal_time({{2024,3,10},{2,30,0}}, earliest, undefined).
%%   => [{{2024,3,10},{7,30,0}}]
%% 
%%   % 使用 latest 策略（夏令时后，offset=-4h）
%%   eNifFtz:local_time_to_universal_time({{2024,3,10},{2,30,0}}, latest, undefined).
%%   => [{{2024,3,10},{6,30,0}}]
%% 
%%   % 使用 strict 策略 + DST=false（非夏令时）
%%   eNifFtz:local_time_to_universal_time({{2024,3,10},{2,30,0}}, strict, false).
%%   => [{{2024,3,10},{7,30,0}}]
%% 
%%   % 使用 strict 策略 + DST=true（夏令时）
%%   eNifFtz:local_time_to_universal_time({{2024,3,10},{2,30,0}}, strict, true).
%%   => [{{2024,3,10},{6,30,0}}]
-spec local_time_to_universal_time(datetime(), amb_policy(), non_policy()) -> utc_result().
local_time_to_universal_time(_LocalDT, _AmbPolicy, _NonPolicy) ->
   erlang:nif_error(nif_not_loaded).

