-module(utDecompile).

-export([decCom/2]).

decCom(BeamDir, SrcDir) ->
   FunRead =
      fun(File, ProAcc) ->
         case filename:extension(File) == ".beam" of
            true ->
               io:format("Convert proto msg file: ~s ~n", [File]),
               ModName = filename:basename(File, ".beam"),
               Module = list_to_atom(ModName),
               case beam_lib:chunks(code:which(Module), [abstract_code]) of
                  {ok, {_, [{abstract_code, {_, AC}}]}} ->
                     %% Code = erl_prettypr:format(erl_syntax:form_list(AC)),
                     SrcStr = io_lib:fwrite("~ts~n", [erl_prettypr:format(erl_syntax:form_list(AC))]),
                     file:write_file(lists:concat([SrcDir, Module, ".erl"]), SrcStr),
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
