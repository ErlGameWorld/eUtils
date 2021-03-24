-module(utParseStack).

-include("utParseStack.hrl").

-export([
   parseStack/1            %% 堆栈格式美化函数
   , parseStack/3          %% Class Reason 堆栈格式美化函数
   , printStack/1          %% 打印当前堆栈
   , testStack/1           %% 相关测试函数
]).

%%  堆栈回溯（stacktrace）是{Module，Function，Arity，Location}元组的列表。
%%  Arity: 根据异常，Arity字段可以是该函数调用的参数列表，而不是arity整数。
%%  Location: 是一个二元组的列表（可能为空），可以指示该函数在源代码中的位置。
%%  第一个元素是描述第二个元素中信息类型的原子。可能发生以下情况：
%% [{file, 一个字符串, 代表函数源文件的文件名}, {line, 元组的第二个元素是发生异常或调用函数的源文件中的行号, 整数> 0} :

%% term序列化, term转为string
parseStack(Class, Reason, Stacktrace) ->
   eFmt:formatBin(<<"~n  Class:~s~n  Reason:~p~n  Stacktrace:~s">>, [Class, Reason, parseStack(Stacktrace)]).

parseStack(Stacktrace) ->
   <<begin
       case Location of
          [] ->
             <<"     ", (atom_to_binary(Mod, utf8))/binary, ":", (atom_to_binary(Func, utf8))/binary, "(", (eFmt:formatBin("~w", [Arity]))/binary, ")\n">>;
          [{file, File}, {line, Line}] ->
             <<"     ", (atom_to_binary(Mod, utf8))/binary, ":", (atom_to_binary(Func, utf8))/binary, "/", (integer_to_binary(Arity))/binary, "(", (unicode:characters_to_binary(File))/binary, ":", (integer_to_binary(Line))/binary, ")\n">>;
          _ ->
             <<"     ", (atom_to_binary(Mod, utf8))/binary, ":", (atom_to_binary(Func, utf8))/binary, "(", (eFmt:formatBin("~w", [Arity]))/binary, ")", (eFmt:formatBin("~w", [Location]))/binary, "\n">>
       end
    end || {Mod, Func, Arity, Location} <- Stacktrace
   >>.

printStack(Tag) ->
   ?PRINT_STACK(Tag).

%% 下面是测试代码
make_throw() ->
   throw({test, exception}).

bad_arity() ->
   lists:concat([], []).

bad_arg(ErrArgs) ->
   integer_to_list(ErrArgs).

testStack(Index) ->
   application:set_env(lager, reverse_pretty_stacktrace, false),
   try
      case Index of
         1 ->
            make_throw();
         2 ->
            bad_arity();
         3 ->
            bad_arg(Index + 0.0)
      end
   catch
      ?EXCEPTION(Class, Reason, Stacktrace) ->
         Stacktrace = ?GET_STACK(Stacktrace),
         io:format("~p~n", [Stacktrace]),
         io:format(parseStack(Stacktrace)),
         io:format(parseStack(Class, Reason, Stacktrace))
      %% lagere使用示例
      %% lager:error(parse_stack(Stacktrace)),
      %% lager:error(parse_stack(Stacktrace, Class, Reason))
   end.