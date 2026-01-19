-module(utEvalStr).

-export([eval/1, eval/2]).

%%  注意事项（非常重要）
%% 1.  **句号结尾**：
%% Erlang 的语法要求表达式必须以 `.` 结束。如果输入的字符串没有句号，`erl_parse` 可能会失败。你在代码里自动补全：
%%
%% % 简单的自动补全示例
%% StrWithDot = case lists:suffix(".", String) of
%% true -> String;
%% false -> String ++ "."
%% end.
%%
%%
%% 2.  **性能开销**：
%% 这种方式是**解释执行**的，而不是编译执行。它的速度比编译后的 Erlang 代码（BEAM字节码）要慢得多。不要在高性能循环中使用它。
%%
%% 3.  **安全性（代码注入）**：
%% **永远不要在生产环境中直接执行来自用户输入的字符串代码**。这会导致严重的安全漏洞，攻击者可以调用 `os:cmd("rm -rf /")` 等危险指令。
%%
%% 4.  **原子（Atom）溢出**：
%% 如果动态执行的代码中包含大量动态生成的原子（Atom），可能会导致 Erlang 虚拟机的原子表溢出（Atom table limit），导致虚拟机崩溃。

%% Result2 = evalStr("lists:seq(1, 5).").
%%% Result2 = [1,2,3,4,5]
eval(String) ->
	eval(String, #{}).

%%  带变量绑定的执行
%%  如果你想在字符串代码中使用外部传入的变量（例如：字符串是 `"A + B."`，你想传入 A=1, B=2），你需要处理 `Bindings`。
% Code = "X * Y.",
% Map = #{'X' => 10, 'Y' => 5}, % 注意 Key 必须是原子(atom)且对应大写变量名
% Result = evalStr(Code, Map).
% Result = 50
eval(String, BindingsMap) ->
	try
		StrWithDot = String, % case lists:suffix(".", String) of true -> String; false -> String ++ "." end,
		
		% 1. 词法分析
		{ok, Tokens, _} = erl_scan:string(StrWithDot),
		
		% 2. 语法分析
		{ok, AbsForm} = erl_parse:parse_exprs(Tokens),
		
		% 3. 构建绑定环境 直接传map格式的就行
		% 4. 转换本地函数 注意：很多 OTP 版本里第三参就是这种 {value, Fun} 形式
		LocalFuncHandler = {value, fun dispatchLocalFun/2},
		{value, Value, NewBindings} = erl_eval:exprs(AbsForm, BindingsMap, LocalFuncHandler),
		{ok, Value, NewBindings}
	catch C:R:S ->
		{error, {C, R, S}}
	end.

%% ==========================================================
%% 核心调度逻辑：决定函数由谁执行
%% ==========================================================
dispatchLocalFun(Func, Args) ->
	Arity = length(Args),
	
	maybe
		false ?= erlang:function_exported(erlang, Func, Arity) andalso erlang,
		false ?= erlang:function_exported(user_default, Func, Arity) andalso user_default,
		false ?= erlang:function_exported(shell_default, Func, Arity) andalso shell_default,
		false ?= erlang:function_exported(c, Func, Arity) andalso c,
		false ?= erlang:function_exported(i, Func, Arity) andalso i,
		erlang:error({undef, [{Func, Arity}]})
	else
		FModule -> apply(FModule, Func, Args)
	end.