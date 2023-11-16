-module(utBeamToSrc).

-export([
   genSrc/2
   , genSrcs/2
   , notCan/2
   , reToDir/2
   , delFile/2
   , isHasUnicode/1
]).

%% 通过beam生成erl文件，生成的beam编译选项必要带debug_info才能反编译生成代码
genSrc(Module, SrcDir) ->
   case beam_lib:chunks(code:which(Module), [abstract_code]) of
      {ok, {_, [{abstract_code, {_, AC}}]}} ->
         Code = erl_prettypr:format(erl_syntax:form_list(AC)),
         %% 如果代码中有unicode码 需要下面的函数转换一下 第二个函数效率更高
         %% SrcStr = io_lib:fwrite("~ts~n", [erl_prettypr:format(erl_syntax:form_list(AC))]),
         SrcBin = unicode:characters_to_binary(Code),
         file:write_file(lists:concat([SrcDir, Module, ".erl"]), SrcBin),
         io:format("build beam:~p to erl:~p success.~n", [Module, Module]);
      {error, beam_lib, Reason} ->
         io:format("code_gen_erl_file error, reason:~p~n", [Reason]);
      _Err ->
         io:format("code_gen_erl_file error, reason:~p~n", [_Err])
   end.

%% 通过beam生成erl文件，生成的beam编译选项必要带debug_info才能反编译生成代码
genSrcs(BeamDir, SrcDir) ->
   FunDeal =
      fun(File, ProAcc) ->
         ModName = filename:basename(File, ".beam"),
         Module = list_to_atom(ModName),
         case beam_lib:chunks(code:which(Module), [abstract_code]) of
            {ok, {_, [{abstract_code, {_, AC}}]}} ->
               Code = erl_prettypr:format(erl_syntax:form_list(AC)),
               %% 如果代码中有unicode码 需要下面的函数转换一下 第二个函数效率更高
               %% SrcStr = io_lib:fwrite("~ts~n", [erl_prettypr:format(erl_syntax:form_list(AC))]),
               SrcBin = unicode:characters_to_binary(Code),
               file:write_file(lists:concat([SrcDir, Module, ".erl"]), SrcBin),
               io:format("build beam:~p to erl:~p success.~n", [Module, Module]);
            {error, beam_lib, Reason} ->
               io:format("code_gen_erl_file error, reason:~p~n", [Reason]);
            _Err ->
               io:format("code_gen_erl_file error, reason:~p~n", [_Err])
         end,
         ProAcc
      end,
   filelib:fold_files(BeamDir, "\\.beam$", true, FunDeal, []).

%% 将反编译出来的源文件 -file 属性删除掉
delFile(SrcDir, NewDir) ->
   FunDeal =
      fun(File, ProAcc) ->
         ModName = filename:basename(File, ".erl"),
         Module = list_to_atom(ModName),
         {ok, CodeBin} = file:read_file(File),
         SrcBin = doDelFile(CodeBin, <<>>),
         file:write_file(lists:concat([NewDir, Module, ".erl"]), SrcBin),
         io:format("build beam:~p to erl:~p success.~n", [Module, Module]),
         ProAcc
      end,
   filelib:fold_files(SrcDir, "\\.erl$", true, FunDeal, []).

doDelFile(CodeBin, SrcBin) ->
   case binary:split(CodeBin, <<"-file(">>) of
      [Part1] ->
         <<SrcBin/binary, Part1/binary>>;
      [Part1, Part2] ->
         [_, LeftPart] = binary:split(Part2, <<").">>),
         doDelFile(LeftPart, <<SrcBin/binary, Part1/binary>>)
   end.

%% 检查源码中是否存在unicode字符 主要是为了检查是否存在中文
isHasUnicode(SrcDir) ->
   FunDeal =
      fun(File, ProAcc) ->
         ModName = filename:basename(File, ".erl"),
         Module = list_to_atom(ModName),
         {ok, CodeBin} = file:read_file(File),
         IsHas = checkUnicode(CodeBin),
         
         case IsHas of
            true ->
               [Module | ProAcc];
            _ ->
               ProAcc
         end
      end,
   AllMods = filelib:fold_files(SrcDir, "\\.erl$", true, FunDeal, []),
   ModStr = <<<<(erlang:atom_to_binary(OneMod))/binary, "\n">> || OneMod <- AllMods>>,
   file:write_file("hasUnicodeMod.txt", ModStr).

checkUnicode(<<>>) ->
   false;
checkUnicode(<<Word/utf8, Left/binary>>) ->
   case Word > 256 of
      true ->
         true;
      _ ->
         checkUnicode(Left)
   end.

%% 将不能反编译的beam文件复制到指定的目录
notCan(BeamDir, SrcDir) ->
   FunDeal =
      fun(File, ProAcc) ->
         ModName = filename:basename(File, ".beam"),
         Module = list_to_atom(ModName),
         case beam_lib:chunks(code:which(Module), [abstract_code]) of
            {ok, {_, [{abstract_code, {_, _AC}}]}} ->
               ProAcc;
            {error, beam_lib, Reason} ->
               io:format("code_gen_erl_file error, reason:~p~n", [Reason]),
               file:copy(File, lists:concat([SrcDir, ModName, ".beam"]));
            _Err ->
               io:format("code_gen_erl_file error, reason:~p~n", [_Err]),
               file:copy(File, lists:concat([SrcDir, ModName, ".beam"]))
         end,
         ProAcc
      end,
   filelib:fold_files(BeamDir, "\\.beam$", true, FunDeal, []).

%% 将反编译的文件根据最前面的-file信息  重新复制到正确的目录
reToDir(SSrcDir, DSrcDir) ->
   FunDeal =
      fun(File, ProAcc) ->
         case file:read_file(File) of
            {ok, <<"-file(", _/binary>> = BinStr} ->
               case binary:split(BinStr, <<"-module">>) of
                  [AllFileInfo, _] ->
                     FileInfo = binary:replace(AllFileInfo, [<<" ">>, <<"\n">>, <<"\"">>], <<"">>, [global]),
                     [_, LeftFileInfo] = binary:split(FileInfo, <<"(">>),
                     [DirInfo, _] = binary:split(LeftFileInfo, <<",">>),
                     FileDir = filename:join(DSrcDir, DirInfo),
                     filelib:ensure_dir(FileDir),
                     {ok, _} = file:copy(File, FileDir),
                     ok;
                  _ ->
                     ProAcc
               end;
            _ ->
               ProAcc
         end
      end,
   filelib:fold_files(SSrcDir, "\\.erl$", true, FunDeal, []).

%% 还可以根据反编译的内容恢复头文件 暂时没这个需求