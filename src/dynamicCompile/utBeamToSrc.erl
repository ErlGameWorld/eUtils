-module(utBeamToSrc).

-export([
   genSrc/2
   , genSrcs/2
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
   FunRead =
      fun(File, ProAcc) ->
         case filename:extension(File) == ".beam" of
            true ->
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
               ProAcc;
            _ ->
               ProAcc
         end
      end,
   filelib:fold_files(BeamDir, "\\.beam$", true, FunRead, []).