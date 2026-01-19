这是一个为您生成的完整项目文档，包含 Markdown 格式的说明。我特别强化了**“为什么要开发这个 NIF”**的章节，深入剖析了性能痛点和本方案的技术优势。

---

# FastTZ: 基于 C++20 的高性能 Erlang 时区转换 NIF

**FastTZ** 是一个为 Erlang/Elixir 虚拟机 (BEAM) 设计的原生实现函数库 (NIF)。它利用现代 C++20 标准库中的 `<chrono>` 模块和线程本地缓存 (Thread Local Storage)，提供了纳秒级的时区转换能力。

---

## 💡 核心价值：为什么我们需要这个 NIF？

在 Erlang/Elixir 的标准库或常规第三方库（如 `tzdata`, `timex`）中，时区转换虽然功能强大，但在**高并发**或**海量数据处理**场景下存在显著的性能瓶颈。

### 1. 消除系统调用与锁竞争
Erlang 自带的 `calendar:local_time/0` 等函数通常依赖于操作系统的底层 API（如 POSIX `localtime`）。这些 API 往往涉及：
*   **文件 I/O**：读取 `/etc/localtime`。
*   **全局锁**：在某些 OS 实现中，时区转换不是线程安全的，会导致隐式的锁竞争。
*   **性能波动**：依赖外部环境，无法保证稳定的低延迟。

### 2. 突破纯 Erlang 实现的极限
纯 Erlang 实现（如 `tzdata`）通常将时区数据加载到 ETS 表中。虽然比 OS 调用快，但每次转换仍需要：
*   ETS 查询开销。
*   数据解码与计算开销。
*   对于每秒处理数十万条日志或实时数据的系统，这些微小的开销累积起来会显著占用 CPU。

### 3. 本方案的“杀手锏”：TLS 缓存 + C++20
FastTZ 引入了 **Thread Local Storage (TLS) 缓存机制**，解决了上述痛点：
*   **局部性原理**：在处理时间序列数据时，相邻的时间戳通常具有相同的时区偏移（例如，连续处理的 1000 条日志都在 "UTC+8" 范围内）。
*   **零计算路径**：当缓存命中时，FastTZ **不进行任何查表或计算**，仅执行一次整数加减法。这使得转换速度从微秒级提升到**纳秒级**。
*   **C++20 标准库**：利用 GCC/Clang 高度优化的 `std::chrono::time_zone`，通过内存直接映射，无需手动解析时区文件。

**总结：FastTZ 专为对延迟极度敏感、吞吐量要求极高的场景设计（如广告竞价、日志流处理、金融撮合）。**

---

## 🛡️ 技术特性与安全性

*   **生产级安全**：全路径 `try-catch` 包裹，防止 C++ 异常导致 BEAM 虚拟机崩溃。
*   **无锁设计**：利用原子变量 (`std::atomic`) 和线程局部存储 (`thread_local`)，实现真正的无锁并发。
*   **防御性编程**：
    *   严格拒绝闰秒（秒数 60），防止时间隐式进位。
    *   严格校验日期合法性（拦截 "2023-02-30"）。
    *   原子加载状态检查，防止在 NIF 未加载时调用导致的段错误。
*   **无卡顿 (No Stalls)**：所有计算均为纯内存操作，无磁盘 I/O，无网络请求，执行时间远低于 1ms，不会阻塞 Erlang 调度器。

---

## ⚙️ 环境要求

由于使用了 C++20 的 `<chrono>` 特性，编译环境必须支持 **C++20**：

*   **GCC**: 13.1 或更高版本
*   **Clang**: 15.0 或更高版本
*   **MSVC**: Visual Studio 2019 version 16.10+ (需开启 `/std:c++20`)
*   **OS**: Linux, macOS, Windows

---

## 📚 API 文档

所有时间格式均为 Erlang 标准 Tuple：
*   `DateTime` = `{{Year, Month, Day}, {Hour, Minute, Second}}` 或 `{{Y,M,D}, {H,M,S,Microseconds}}`

### 1. `zone/0`
获取当前系统时区的偏移量（秒）。

```erlang
-spec zone() -> integer() | {error, reason()}.
```
**示例**:
```erlang
fast_tz:zone().
%% Returns: 28800  (即 UTC+8)
```

### 2. `universal_time_to_local_time/1`
将 UTC 时间转换为本地时间。

```erlang
-spec universal_time_to_local_time(DateTime) -> 
    {LocalTime, Offset, IsDst} | {error, reason()}.
```
*   `Offset`: 该时间点对应的时区偏移秒数。
*   `IsDst`: `true` 表示夏令时，`false` 表示标准时。

**示例**:
```erlang
UTC = {{2023, 10, 27}, {10, 0, 0}},
fast_tz:universal_time_to_local_time(UTC).
%% Returns: {{{2023,10,27},{18,0,0,0}}, 28800, false}
```

### 3. `local_time_to_universal_time/1`
将本地时间转换为 UTC 时间。

```erlang
-spec local_time_to_universal_time(DateTime) -> 
    [UtcTime] | {error, reason()}.
```
*   **返回值是列表**：因为在夏令时结束的回拨时刻（Ambiguous Time），一个本地时间可能对应两个 UTC 时间。
*   **间隙时间 (Gap)**：如果时间无效（夏令时开始跳过的时段），返回间隙结束后的第一个有效 UTC 时间。

**示例**:
```erlang
Local = {{2023, 10, 27}, {18, 0, 0}},
fast_tz:local_time_to_universal_time(Local).
%% Returns: [{{2023,10,27},{10,0,0,0}}]
```

### 4. `local_time_to_universal_time/3`
提供精细控制策略的本地转 UTC 函数。

```erlang
-spec local_time_to_universal_time(DateTime, Strategy, DstInput) -> 
    [UtcTime] | {error, reason()}.
```

**参数说明**:
*   **Strategy** (策略):
    *   `undefined` / `0`: 默认行为。模糊时间返回列表，间隙时间返回下一个有效值。
    *   `earliest` / `1`: 模糊时间取最早的（通常是夏令时）。
    *   `latest` / `2`: 模糊时间取最晚的（通常是标准时）。
    *   `strict` / `3`: 遇到模糊或间隙时间直接报错。
*   **DstInput** (强制夏令时):
    *   `undefined` / `-1`: 忽略，自动判断。
    *   `true`: 倾向于夏令时结果。
    *   `false`: 倾向于标准时结果。

**示例 (处理夏令时回拨)**:
```erlang
%% 假设 02:30 发生了两次
AmbiguousTime = {{2023, 11, 5}, {1, 30, 0}}, 

%% 获取最早的那个（夏令时）
fast_tz:local_time_to_universal_time(AmbiguousTime, earliest, undefined).

%% 严格模式，如果时间重叠则报错
fast_tz:local_time_to_universal_time(AmbiguousTime, strict, undefined).
%% Returns: {error, ambiguous_time}
```

---

## 🚨 错误处理

NIF 即使在出错时也保证返回 `{error, Reason}` 元组，绝不 Crash。

| 错误码 | 含义 |
| :--- | :--- |
| `zone_not_loaded` | NIF 库未加载成功，请检查 `.so` 文件路径。 |
| `internal_error` | C++ 内部时区指针丢失（罕见）。 |
| `conversion_failed` | C++ 库抛出了未预期的异常。 |
| `invalid_date_time` | 输入格式错误，或日期非法（如 2月30日，或秒数>59）。 |
| `invalid_strategy` | 策略参数无效。 |
| `ambiguous_time` | 严格模式下遇到了重叠时间。 |
| `nonexistent_time` | 严格模式下遇到了间隙时间。 |

---

## 🛠️ 编译说明 (rebar3)

在 `rebar.config` 中配置 NIF 编译钩子：

```erlang
{port_specs, [
    {"priv/fast_tz.so", ["c_src/*.cpp"]}
]}.

{port_env, [
    %% 确保指定 C++20 标准
    {"(linux|solaris|freebsd|netbsd|openbsd|dragonfly|darwin|gnu)",
     "CXXFLAGS", "$CXXFLAGS -std=c++20 -O3 -Wall"},
    {"(linux|solaris|freebsd|netbsd|openbsd|dragonfly|darwin|gnu)",
     "LDFLAGS", "$LDFLAGS -lstdc++"}
]}.
```