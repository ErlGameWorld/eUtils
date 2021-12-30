# 安装
    1)[直接下载](https://s3.amazonaws.com/rebar3/rebar3) 
        安装到本地 ./rebar3 local install
        升级版本   rebar3 local upgrade
    2)源码安装
        $ git clone https://github.com/erlang/rebar3.git
        $ cd rebar3
        $ ./bootstrap
        $ ./rebar3 local install

# 使用rebar3创建新的项目
    使用rebar3组织项目主要有两种方式：
    1) 作为单个应用程序
        单个应用程序项目在目录的根目录中包含一个单独的顶级应用程序，其Erlang源模块直接位于src/目录中。
    2)作为伞行项目
        伞项目的定义特征是它们可以包含多个顶层的Erlang / OTP应用中，通常内的顶层apps/或lib/目录。这些应用程序中的每一个都可能包含其自己的rebar.config文件。此格式仅适用于具有一个或多个顶级应用程序的发行版。

    Rebar3带有用于创建这两种类型的项目的模板，可通过rebar3 new <template> <project-name>命令进行调用。该<template>值可以是以下任意值：
        app：带有监控树的有状态OTP应用程序，作为单个应用程序项目
        lib：一个库OTP应用程序（无监督树），对于将各个模块组合在一起作为单个应用程序项目很有用
        release：准备发布的伞项目
        escript：单个应用程序项目的一种特殊形式，可以将其构建为可运行的脚本
        plugin：rebar3插件的结构。

# 添加依赖
    依赖格式
        {deps,[
        %% Packages
        rebar,
        {rebar,"1.0.0"},
        {rebar, {pkg, rebar_fork}}, % rebar app under a different pkg name
        {rebar, "1.0.0", {pkg, rebar_fork}},
        %% Source Dependencies
        {rebar, {git, "git://github.com/erlang/rebar3.git"}},
        {rebar, {git, "http://github.com/erlang/rebar3.git"}},
        {rebar, {git, "https://github.com/erlang/rebar3.git"}},
        {rebar, {git, "git@github.com:erlang/rebar3.git"}},
        {rebar, {hg, "https://othersite.com/erlang/rebar3"}},
        {rebar, {git, "git://github.com/erlang/rebar3.git", {ref, "aef728"}}},
        {rebar, {git, "git://github.com/erlang/rebar3.git", {branch, "master"}}},
        {rebar, {git, "git://github.com/erlang/rebar3.git", {tag, "3.0.0"}}},
        %% Source dependencies (git only) in subdirectories, from version 3.14 onwards
        {rebar, {git_subdir, "git://github.com/erlang/rebar3.git", {branch, "main"}, "subdir"}},
        {rebar, {git_subdir, "git://github.com/erlang/rebar3.git", {tag, "3.14"}, "sub/dir"},
        {rebar, {git_subdir, "git://github.com/erlang/rebar3.git", {ref, "aeaefd"}, "dir"}
        ]}.

# 命令相关
    rebar3 update               更新缓存软件依赖包到最新, 但是不会修改到项目中
    rebar3 upgrade <depname>    升级软件包到项目中   这将更新锁文件定义，并且在下一个构建中，将提取并编译新副本。如果传递依赖项也已升级，则将检测并处理。
    您应该避免在可能的情况下删除锁定文件，并且如果需要升级多个应用程序，可以致电rebar3 upgrade app1,app2,app3。如果rebar3 upgrade不带任何参数调用，将升级所有应用程序。在小型项目上可以这样做，但是您可能希望在大型项目上逐步进行。 

# 为常见任务创建别名(别名可以手动创建一个简单的命令来运行多个任务，而不是手动调用所有任务)
    {alias, [
    {check, [xref, dialyzer, edoc,
             {proper, "--regressions"},
             {proper, "-c"}, {ct, "-c"}, {cover, "-v --min_coverage=80"}]}
    ]}.

# 各种工具的推荐配置

    一些rebar3配置和默认设置可能太宽松或太严格。但是，由于对向后兼容性的承诺，我们不能总是对其进行更改和改编，因为这可能会破坏依赖于这些特定配置的项目。
    以下是一些配置的集合，这些配置在启动新项目时可用作新的默认值。
    {dialyzer, [
    {warnings, [
       %% Warn about undefined types and unknown functions
       unknown
    ]}
    ]}.

    {xref_checks,[
        %% enable most checks, but avoid 'unused calls' which is often very verbose
        undefined_function_calls, undefined_functions, locals_not_used,
        deprecated_function_calls, deprecated_functions
    ]}.

    {profiles, [
    {test, [
        %% Avoid warnings when test suites use `-compile(export_all)`
        {erl_opts, [nowarn_export_all]}
    ]}
    ]}.

