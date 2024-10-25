-module(utCompileStr).

-compile([nowarn_unused_function]).

-export([
   loadStr/1
   , loadStr/2
   , formsStr/1
   , formsStr/2
]).

-type option() :: atom() | {atom(), term()} | {'d', atom(), term()}.

-type abstract_code() :: [erl_parse:abstract_form()].
-type error_info() :: erl_lint:error_info().
-type errors() :: [{file:filename(), [error_info()]}].
-type warnings() :: [{file:filename(), [error_info()]}].
-type mod_ret() ::
   {'ok', module()}|
   {'ok', module(), cerl:c_module()} | %% with option 'to_core'
   {'ok', module() | [], abstract_code()}  |                         %% with option 'to_pp'   %% module() if 'to_exp'
   {'ok', module(), warnings()}.
-type bin_ret() ::
   {'ok', module(), binary()}|
   {'ok', module(), binary(), warnings()}.
-type err_ret() ::
   'error' |
   {'error', errors(), warnings()}.
-type comp_ret() ::
   mod_ret() |
   bin_ret() |
   err_ret().

-import(lists, [reverse/1, keyreplace/4]).

%% 从字符串编译并加载
-spec loadStr(CodeStr :: string()) -> {module, Module :: module()} | {error, What :: term()}.
loadStr(CodeStr) ->
   loadStr(CodeStr, []).

%% 从字符串编译并加载
-spec loadStr(CodeStr :: string(), CompileFormsOpts :: [option()] | option()) -> {module, Module :: module()} | {error, What :: term()}.
loadStr(CodeStr, CompileFormsOpts) ->
   {Mod, Bin} = formsStr(CodeStr, CompileFormsOpts),
   code:load_binary(Mod, [], Bin).

%% 从字符串编译
-spec formsStr(CodeStr :: string()) -> comp_ret().
formsStr(CodeStr) ->
   formsStr(CodeStr, []).

%% 从字符串编译
-spec formsStr(CodeStr :: string(), CompileFormsOpts :: [option()] | option()) -> comp_ret().
formsStr(CodeStr, CompileFormsOpts) ->
   Filename = "compiled_from_string",

   %% 根据 compile:forms 的文档：当遇到 -include 或 -include_dir 指令时，编译器会在以下目录中搜索头文件：
   %%    1. "."，文件服务器的当前工作目录；
   %%    2. 编译文件的基名；
   %%    3. 使用编译选项 'i' 选项指定的目录。最后指定的目录首先被搜索。在这种情况下，2 没有意义。

   IncludePath = ["." | reverse([Dir || {i, Dir} <- CompileFormsOpts])],
   case scanAndParse(CodeStr, Filename, 1, [], [], #{}, IncludePath) of
      {ok, RevForms, _OutMacroMap} ->
         Forms = reverse(RevForms),

         %% note: 'binary' is forced as an implicit option, whether it is provided or not.
         case compile:forms(Forms, CompileFormsOpts) of
            {ok, _ModName, CompiledCodeBinary} = Ret when is_binary(CompiledCodeBinary) ->
               Ret;
            {ok, _ModName, _CompiledCodeBinary, _Warnings} = Ret ->
               Ret;
            {error, [{_, Errors}], Warnings} ->
               {error, Errors, Warnings};
            Other ->
               {error, {bat_return, Other}}
         end;
      {error, Errors} ->
         {error, Errors, []}
   end.


scanAndParse([], _CurrFilename, _CurrLine, RevForms, Errors, MacroMap, _IncludePath) ->
   case Errors of
      [] ->
         {ok, RevForms, MacroMap};
      _ ->
         {error, lists:flatten(lists:reverse(Errors))}
   end;
scanAndParse(RemainingText, CurrFilename, CurrLine, RevForms, Errors, MacroMap, IncludeSearchPath) ->
   case scanner(RemainingText, CurrLine, MacroMap) of
      {tokens, NLine, NRemainingText, Toks} ->
         case erl_parse:parse_form(Toks) of
            {ok, Form0} ->
               {Form, Forms} = normalizeRecord(Form0),

               scanAndParse(NRemainingText, CurrFilename, NLine, [Form | Forms ++ RevForms], Errors, MacroMap, IncludeSearchPath);
            {error, E} ->
               scanAndParse(NRemainingText, CurrFilename, NLine, RevForms, [E | Errors], MacroMap, IncludeSearchPath)
         end;
      {macro, NLine, NRemainingText, NMacroMap} ->
         scanAndParse(NRemainingText, CurrFilename, NLine, RevForms, Errors, NMacroMap, IncludeSearchPath);
      {Def, NLine, NRemainingText, NMacroMap} when Def =:= def; Def =:= endif; Def =:= 'else';Def =:= undef; Def =:= attribute; Def =:= vsn ->
         scanAndParse(NRemainingText, CurrFilename, NLine, RevForms, Errors, NMacroMap, IncludeSearchPath);
      {vsn, NLine, NRemainingText, NMacroMap} ->
         scanAndParse(NRemainingText, CurrFilename, NLine, RevForms, Errors, NMacroMap, IncludeSearchPath);
      {Include, NLine, NRemainingText, IncludeFilename} when Include =:= include; Include =:= include_lib ->
         {IncludeCurrentFile, IncludeFileRemainingTextents} =
            case Include of
               include ->
                  readIncludeFile(IncludeFilename, CurrFilename);
               _ ->
                  readIncludeLibFile(IncludeFilename, CurrFilename)
            end,

         %%io:format("include file ~p contents: ~n~p~nRemainingText = ~p~n", [IncludeFilename,IncludeFileRemainingTextents, RemainingText]),
         %% Modify the FILE macro to reflect the filename
         IncludeMacroMap = MacroMap,

         %% Process the header file (inc. any nested header files)
         {ok, RevIncludeForms, IncludedMacroMap} = scanAndParse(IncludeFileRemainingTextents, IncludeCurrentFile, 1, [], Errors, IncludeMacroMap, IncludeSearchPath),
         %io:format("include file results = ~p~n", [R]),
         %% Restore the FILE macro in the NEW MacroMap (so we keep any macros defined in the header file)
         NMacroMap = IncludedMacroMap,
         %% Continue with the original file
         scanAndParse(NRemainingText, CurrFilename, NLine, RevIncludeForms ++ RevForms, Errors, NMacroMap, IncludeSearchPath);
      {continue, Continuation} ->
         scanAndParse([], CurrFilename, CurrLine, [Continuation | RevForms], Errors, MacroMap, IncludeSearchPath);
      done ->
         scanAndParse([], CurrFilename, CurrLine, RevForms, Errors, MacroMap, IncludeSearchPath)
   end.

scanner(Text, Line, MacroMap) ->
   case erl_scan:tokens([], Text, Line) of
      {done, {ok, Tokens, NLine}, LeftOverChars} ->
         case preProc(Tokens, MacroMap) of
            {tokens, NToks} ->
               {tokens, NLine, LeftOverChars, NToks};
            {macro, NMacroMap} ->
               {macro, NLine, LeftOverChars, NMacroMap};
            {include, Filename} ->
               {include, NLine, LeftOverChars, Filename};
            {include_lib, Filename} ->
               {include_lib, NLine, LeftOverChars, Filename};
            Def when Def =:= def; Def =:= endif; Def =:= 'else'; Def =:= undef; Def =:= attribute; Def =:= vsn ->
               {Def, NLine, LeftOverChars, MacroMap}
         end;
      {more, {erl_scan_continuation, _, _, Tokens, NLine, _, Any, _} = _Continuation} ->

         %% 这应该表示“术语尚未完成”（即“。”尚未达到 %%）。
         %% 但是，由于某些奇怪的原因，如果在最后的 '.' 之后有注释，我们也会得到这个。在一个文件中。
         %% 所以我们检查 Text 是否只包含评论。
         case isOnlyComments(Text) of
            true ->
               done;
            {false, _} ->
               Header =
                  case string:to_integer(Any) of
                     {Int, []} ->
                        [{dot, NLine}, {integer, NLine, Int}];
                     _ when is_list(Any), Any =/= [] ->
                        [{dot, NLine}, {string, NLine, Any}];
                     _ ->
                        [{dot, NLine}]
                  end,

               case preProc(lists:reverse(lists:concat([Header, Tokens])), MacroMap) of
                  {tokens, NToks} -> {tokens, NLine, [], NToks};
                  {macro, NMacroMap} -> {macro, NLine, [], NMacroMap};
                  {include, Filename} -> {include, NLine, [], Filename};
                  {include_lib, Filename} -> {include_lib, NLine, [], Filename};
                  Def when Def =:= def; Def =:= endif; Def =:= 'else'; Def =:= undef; Def =:= attribute; Def =:= vsn ->
                     {Def, NLine, [], MacroMap}

               end
         end
   end.

is_endif(Text) ->
   is_endif(Text, not_in_endif).

is_endif([], _) -> false;
is_endif([$  | T], not_in_endif) ->
   is_endif(T, not_in_endif);
is_endif([$\t | T], not_in_endif) ->
   is_endif(T, not_in_endif);
is_endif([$\n | T], not_in_endif) ->
   is_endif(T, not_in_endif);
is_endif("-endif.", not_in_endif) ->
   true;
is_endif(_, not_in_endif) -> false.


isOnlyComments(Text) ->
   isOnlyComments(Text, not_in_comment).

isOnlyComments([], _) ->
   true;
isOnlyComments([$  | T], not_in_comment) ->
   isOnlyComments(T, not_in_comment);               % skipping whitspace outside of comment
isOnlyComments([$\t | T], not_in_comment) ->
   isOnlyComments(T, not_in_comment);                % skipping whitspace outside of comment
isOnlyComments([$\n | T], not_in_comment) ->
   isOnlyComments(T, not_in_comment);                 % skipping whitspace outside of comment
isOnlyComments([$\r | T], not_in_comment) ->
   isOnlyComments(T, not_in_comment);
isOnlyComments([$% | T], not_in_comment) ->
   isOnlyComments(T, in_comment);     % found start of a comment
isOnlyComments(Text, not_in_comment) ->
   {false, Text};
% found any significant char NOT in a comment
isOnlyComments([$\n | T], in_comment) ->
   isOnlyComments(T, not_in_comment); % found end of a comment
isOnlyComments([_ | T], in_comment) ->
   isOnlyComments(T, in_comment).     % skipping over in-comment chars

%% have to implement a subset of the pre-processor, since epp insists on running on a file.
%% only handles 2 cases;
%%    -define(MACRO, something).
%%    -define(MACRO(VAR1,VARN),{stuff,VAR1,more,stuff,VARN,extra,stuff}).
preProc([{'-', _}, {atom, _, define}, {'(', _}, {_, _, Name} | DefTokens], MacroMap) ->
   case DefTokens of
      [{',', _} | Macro] ->
         {macro, maps:put(Name, {[], macroBodyDef(Macro, [], [])}, MacroMap)};
      [{'(', _} | Macro] ->
         {macro, maps:put(Name, macroParamsBodyDef(Macro, []), MacroMap)}
   end;
preProc([{'-', _}, {atom, _, vsn}, {'(', _}, _, {')', _}, {dot, _}], _MacroMap) ->
   vsn;
preProc([{'-', _}, {atom, _, include}, {'(', _}, {string, _, Filename}, {')', _}, {dot, _}], _MacroMap) ->
   {include, Filename};
preProc([{'-', _}, {atom, _, include_lib}, {'(', _}, {string, _, Filename}, {')', _}, {dot, _}], _MacroMap) ->
   {include_lib, Filename};
preProc([{'-', _}, {atom, _, ifndef}, {'(', _}, _, {')', _}, {dot, _}], _MacroMap) ->
   def;
preProc([{'-', _}, {atom, _, ifdef}, {'(', _}, _, {')', _}, {dot, _}], _MacroMap) ->
   def;
preProc([{'-', _}, {atom, _, endif}, {dot, _}], _MacroMap) ->
   endif;
preProc([{'-', _}, {atom, _, 'else'}, {dot, _}], _MacroMap) ->
   'else';
preProc([{'-', _}, {atom, _, undef}, {'(', _}, _, {')', _}, {dot, _}], _MacroMap) ->
   undef;
preProc([{'-', _}, {atom, _, author}, {'(', _}, {'{', _}, {_, _, _}, {',', _}, {_, _, _}, {'}', _}, {')', _}, {dot, _}], _MacroMap) ->
   attribute;
preProc([{'-', _}, {atom, _, author}, {'(', _}, _, {')', _}, {dot, _}], _MacroMap) ->
   attribute;
preProc([{'-', _}, {atom, _, copyright}, {'(', _}, {'{', _}, _, {',', _}, _, {'}', _}, {')', _}, {dot, _}], _MacroMap) ->
   attribute;
preProc([{'-', _}, {atom, _, copyright}, {'(', _}, _, {')', _}, {dot, _}], _MacroMap) ->
   attribute;
preProc([{'-', _}, {atom, _, description}, {'(', _}, _, {')', _}, {dot, _}], _MacroMap) ->
   attribute;
preProc(Tokens, MacroMap) ->
   {tokens, substMacros(Tokens, MacroMap)}.

macroParamsBodyDef([{')', _}, {',', _} | Tokens], RevParams) ->
   {reverse(RevParams), macroBodyDef(Tokens, [], [])};
macroParamsBodyDef([{var, _, Param} | Tokens], RevParams) ->
   macroParamsBodyDef(Tokens, [Param | RevParams]);
macroParamsBodyDef([{',', _}, {var, _, Param} | Tokens], RevParams) ->
   macroParamsBodyDef(Tokens, [Param | RevParams]).

macroBodyDef([{')', _}, {dot, _}], _, RevMacroBodyTokens) ->
   reverse(RevMacroBodyTokens);
macroBodyDef([{')', Line} | Tokens], LeftList, RevMacroBodyTokens) ->
   macroBodyDef(Tokens, lists:delete('(', LeftList), [{')', Line} | RevMacroBodyTokens]);
macroBodyDef([{'(', Line} | Tokens], [], RevMacroBodyTokens) ->
   macroBodyDef(Tokens, ['('], [{'(', Line} | RevMacroBodyTokens]);
macroBodyDef([{'(', Line} | Tokens], LeftList, RevMacroBodyTokens) ->
   macroBodyDef(Tokens, ['(' | LeftList], [{'(', Line} | RevMacroBodyTokens]);
macroBodyDef([Tok | Tokens], LeftList, RevMacroBodyTokens) ->
   macroBodyDef(Tokens, LeftList, [Tok | RevMacroBodyTokens]).

substMacros(Tokens, MacroMap) ->
   reverse(substMacrosRev(Tokens, MacroMap, [])).

%% returns a reversed list of tokes
substMacrosRev([{'?', _}, {_, LineNum, 'MODULE'} | Tokens], MacroMap, RevOutTokens) ->
   {[], MacroValue} = maps:get('MODULE', MacroMap),
   substMacrosRev(Tokens, MacroMap, [{atom, LineNum, MacroValue}] ++ RevOutTokens);
substMacrosRev([{'?', _}, {_, LineNum, 'FILE'} | Tokens], MacroMap, RevOutTokens) ->
   {[], MacroValue} = maps:get('FILE', MacroMap),
   substMacrosRev(Tokens, MacroMap, [{string, LineNum, MacroValue}] ++ RevOutTokens);
substMacrosRev([{'?', _}, {_, LineNum, 'LINE'} | Tokens], MacroMap, RevOutTokens) ->
   %% special-case for ?LINE, to avoid creating a new MacroMap for every line in the source file
   substMacrosRev(Tokens, MacroMap, [{integer, LineNum, LineNum}] ++ RevOutTokens);
substMacrosRev([{'?', _}, {_, _, Name}, {'(', _} = Paren | Tokens], MacroMap, RevOutTokens) ->
   case maps:is_key(Name, MacroMap) of
      true ->
         case maps:get(Name, MacroMap) of
            {[], MacroValue} ->
               %% 这个宏没有任何变量，所以忽略调用后面跟着 "(...stuff" %% 在这个宏的值内递归展开任何宏调用
               %% TODO: 避免由于循环引用（甚至是间接引用）导致的无限扩展
               RevExpandedOtherMacrosTokens = substMacrosRev(MacroValue, MacroMap, []),
               substMacrosRev([Paren | Tokens], MacroMap, RevExpandedOtherMacrosTokens ++ RevOutTokens);
            ParamsAndBody ->
               %% 这个宏确实有变量。在有序列表中收集所有传递参数
               {NTokens, Arguments, Line} = substMacrosArgsExpress(Tokens, []),
               %% Expand the varibles
               %%io:format("Toks: ~p ~n ParamsAndBody: ~p ~n Arguments: ~p ~n",[Toks, ParamsAndBody, Arguments]),

               ExpandedParamsTokens = substMacrosSubstArgsForVars(ParamsAndBody, [], Arguments, Line),
               %% 递归扩展此宏值内的任何宏调用
               %% TODO: 避免由于循环引用（甚至是间接引用）导致的无限扩展
               RevExpandedOtherMacrosTokens = substMacrosRev(ExpandedParamsTokens, MacroMap, []),
               substMacrosRev(NTokens, MacroMap, RevExpandedOtherMacrosTokens ++ RevOutTokens)
         end;
      _ ->
         substMacrosRev(Tokens, MacroMap, RevOutTokens)
   end;

substMacrosRev([{'?', _}, {_, _, Name} | Toks], MacroMap, RevOutToks) ->
   %% This macro invocation does not have arguments.
   %% Therefore the definition should not have parameters
   case maps:is_key(Name, MacroMap) of
      true ->
         {[], MacroValue} = maps:get(Name, MacroMap),

         %% Recursively expand any macro calls inside this macro's value
         %% TODO: avoid infinite expansion due to circular references (even indirect ones)
         RevExpandedOtherMacrosToks = substMacrosRev(MacroValue, MacroMap, []),
         substMacrosRev(Toks, MacroMap, RevExpandedOtherMacrosToks ++ RevOutToks);
      _ ->
         substMacrosRev(Toks, MacroMap, RevOutToks)
   end;

substMacrosRev([Tok | Toks], MacroMap, RevOutToks) ->
   substMacrosRev(Toks, MacroMap, [Tok | RevOutToks]);
substMacrosRev([], _MacroMap, RevOutToks) -> RevOutToks.

substMacrosArgsExpress([{var, _, ArgsName}, {')', Line} | Toks], RevArgs) ->
   {Toks, reverse([ArgsName | RevArgs]), Line};
substMacrosArgsExpress([{var, _, ArgsName}, {',', _} | Toks], RevArgs) ->
   substMacrosArgsExpress(Toks, [ArgsName | RevArgs]);
substMacrosArgsExpress(Toks, RevArgs) ->
   substMacrosGetExpress(Toks, [], [], RevArgs).

substMacrosGetExpress([{')', Line} | Toks], [], RevExpress, RevArgs) ->
   {Toks, reverse([reverse(RevExpress) | RevArgs]), Line};
substMacrosGetExpress([{')', Line} | Toks], LeftList, RevExpress, RevArgs) ->
   substMacrosGetExpress(Toks, lists:delete('(', LeftList), [{')', Line} | RevExpress], RevArgs);
substMacrosGetExpress([{'}', Line} | Toks], LeftList, RevExpress, RevArgs) ->
   substMacrosGetExpress(Toks, lists:delete('{', LeftList), [{'}', Line} | RevExpress], RevArgs);
substMacrosGetExpress([{'end', Line} | Toks], LeftList, RevExpress, RevArgs) ->
   substMacrosGetExpress(Toks, lists:delete('begin', LeftList), [{'end', Line} | RevExpress], RevArgs);
substMacrosGetExpress([{']', Line} | Toks], LeftList, RevExpress, RevArgs) ->
   substMacrosGetExpress(Toks, lists:delete('[', LeftList), [{']', Line} | RevExpress], RevArgs);
substMacrosGetExpress([{',', _} | Toks], [], RevExpress, RevArgs) ->
   substMacrosArgsExpress(Toks, [reverse(RevExpress) | RevArgs]);
substMacrosGetExpress([{',', Line} | Toks], LeftList, RevExpress, RevArgs) ->
   substMacrosGetExpress(Toks, LeftList, [{',', Line} | RevExpress], RevArgs);
substMacrosGetExpress([{'(', Line} | Toks], LeftList, RevExpress, RevArgs) ->
   substMacrosGetExpress(Toks, ['(' | LeftList], [{'(', Line} | RevExpress], RevArgs);
substMacrosGetExpress([{'begin', Line} | Toks], LeftList, RevExpress, RevArgs) ->
   substMacrosGetExpress(Toks, ['begin' | LeftList], [{'begin', Line} | RevExpress], RevArgs);
substMacrosGetExpress([{'[', Line} | Toks], LeftList, RevExpress, RevArgs) ->
   substMacrosGetExpress(Toks, ['[' | LeftList], [{'[', Line} | RevExpress], RevArgs);
substMacrosGetExpress([{'{', Line} | Toks], LeftList, RevExpress, RevArgs) ->
   substMacrosGetExpress(Toks, ['{' | LeftList], [{'{', Line} | RevExpress], RevArgs);
substMacrosGetExpress([Tok | Toks], LeftList, RevExpress, RevArgs) ->
   substMacrosGetExpress(Toks, LeftList, [Tok | RevExpress], RevArgs).

subst_macros_get_args([{')', _} | Toks], RevArgs) ->
   {Toks, reverse(RevArgs)};
subst_macros_get_args([{',', _}, {var, _, ArgName} | Toks], RevArgs) ->
   subst_macros_get_args(Toks, [ArgName | RevArgs]);
subst_macros_get_args([{var, _, ArgName} | Toks], RevArgs) ->
   subst_macros_get_args(Toks, [ArgName | RevArgs]).

substMacrosSubstArgsForVars({[], BodyToks}, NBodyToks, [], Line) ->
   L = lists:map(
      fun(A) when is_tuple(A) ->
         setelement(2, A, Line);
         (A) ->
            A
      end,
      BodyToks),
   lists:concat(lists:reverse([L | NBodyToks]));
substMacrosSubstArgsForVars({[Param | Params], BodyToks}, NBodyToks, [Arg | Args], Line) ->
   case lists:splitwith(fun({_, _, Var}) when Var =:= Param -> false;(_) -> true end, BodyToks) of
      {L1, []} ->
         L = lists:map(fun(A) when is_tuple(A) -> setelement(2, A, Line);(A) -> A end, L1),
         lists:concat(lists:reverse([L | NBodyToks]));
      {L1, [_ | L2]} ->
         Arg2 =
            case Arg of
               [_ | _] ->
                  lists:map(fun(A) when is_tuple(A) -> setelement(2, A, Line);(A) -> A end, Arg);
               _ ->
                  [{var, 1, Arg}]
            end,
         L3 = lists:map(fun(A) when is_tuple(A) -> setelement(2, A, Line);(A) -> A end, L1),
         substMacrosSubstArgsForVars({Params, L2}, [Arg2, L3 | NBodyToks], Args, Line)
   end.

readIncludeFile(Filename, CurrFilename) ->
   Dir1 = filename:dirname(CurrFilename),
   Dir2 = filename:dirname(Filename),
   Dir = lists:concat([Dir1, "/", Dir2]),
   BaseName = filename:basename(Filename),
   case file:path_open([Dir], BaseName, [read, raw, binary]) of
      {ok, IoDevice, FullName} ->
         {ok, Data} = file:read(IoDevice, filelib:file_size(FullName)),
         file:close(IoDevice),
         {FullName, binary_to_list(Data)};
      {error, Reason} ->
         throw({failed_to_read_include_file, Reason, Filename, CurrFilename})
   end.

getIncludeLibPath(FileName) ->
   case filename:split(FileName) of
      [LibName | _] when LibName =/= "/" ->
         OTPLIB = code:lib_dir(),
         case z_lib:list_file(OTPLIB, 1) of
            {ok, [_ | _] = FileList} ->
               [FilePath] = [lists:concat([OTPLIB, "/", File, "/", "include"]) || {File, _, directory, _, _} <- FileList, string:str(File, LibName) > 0],
               {ok, FilePath};
            _ ->
               {error, no_path}
         end;
      _ ->
         {error, invalid_path}
   end.

readIncludeLibFile(Filename, CurrFilename) ->
   case getIncludeLibPath(Filename) of
      {ok, FilePath} ->
         BaseName = filename:basename(Filename),
         case file:path_open([FilePath], BaseName, [read, raw, binary]) of
            {ok, IoDevice, FullName} ->
               {ok, Data} = file:read(IoDevice, filelib:file_size(FullName)),
               file:close(IoDevice),
               {FullName, binary_to_list(Data)};
            {error, Reason} ->
               throw({readIncludeLibFile, Reason, Filename, CurrFilename})
         end;
      {error, Error} ->
         throw({getIncludeLibPath, Error, Filename, CurrFilename})
   end.

normalizeRecord({attribute, La, record, {Record, Fields}} = Form) ->
   case epp:normalize_typed_record_fields(Fields) of
      {typed, NewFields} ->
         {{attribute, La, record, {Record, NewFields}}, [{attribute, La, type, {{record, Record}, Fields, []}}]};
      not_typed ->
         {Form, []}
   end;
normalizeRecord(Form) ->
   {Form, []}.

