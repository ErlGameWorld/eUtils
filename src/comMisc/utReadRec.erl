-module(utReadRec).
-compile([export_all, nowarn_export_all]).

read_records(FileOrModule, Opts0) ->
   Opts = lists:delete(report_warnings, Opts0),
   case find_file(FileOrModule) of
      {beam, Beam, File} ->
         read_records_from_beam(Beam, File);
      {files, [File]} ->
         read_file_records(File, Opts);
      {files, Files} ->
         lists:flatmap(fun(File) ->
            case read_file_records(File, Opts) of
               RAs when is_list(RAs) -> RAs;
               _ -> []
            end
                       end, Files);
      Error ->
         Error
   end.

-include_lib("kernel/include/file.hrl").
find_file(Mod) when is_atom(Mod) ->
   case code:which(Mod) of
      File when is_list(File) ->
         %% Special cases:
         %% - Modules not in the code path (loaded with code:load_abs/1):
         %%   code:get_object_code/1 only searches in the code path
         %%   but code:which/1 finds all loaded modules
         %% - File can also be a file in an archive,
         %%   beam_lib:chunks/2 cannot handle such paths but
         %%   erl_prim_loader:get_file/1 can
         case erl_prim_loader:get_file(File) of
            {ok, Beam, _} ->
               {beam, Beam, File};
            error ->
               {error, nofile}
         end;
      preloaded ->
         {_M, Beam, File} = code:get_object_code(Mod),
         {beam, Beam, File};
      _Else -> % non_existing, interpreted, cover_compiled
         {error, nofile}
   end;
find_file(File) ->
   case catch filelib:wildcard(File) of
      {'EXIT', _} ->
         {error, invalid_filename};
      Files ->
         {files, Files}
   end.

read_file_records(File, Opts) ->
   case filename:extension(File) of
      ".beam" ->
         read_records_from_beam(File, File);
      _ ->
         parse_file(File, Opts)
   end.

read_records_from_beam(Beam, File) ->
   case beam_lib:chunks(Beam, [abstract_code, "CInf"]) of
      {ok, {_Mod, [{abstract_code, {Version, Forms}}, {"CInf", CB}]}} ->
         case record_attrs(Forms) of
            [] when Version =:= raw_abstract_v1 ->
               [];
            [] ->
               %% If the version is raw_X, then this test
               %% is unnecessary.
               try_source(File, CB);
            Records ->
               Records
         end;
      {ok, {_Mod, [{abstract_code, no_abstract_code}, {"CInf", CB}]}} ->
         try_source(File, CB);
      Error ->
         %% Could be that the "Abst" chunk is missing (pre R6).
         Error
   end.

%% This is how the debugger searches for source files. See int.erl.
try_source(Beam, RawCB) ->
   EbinDir = filename:dirname(Beam),
   CB = binary_to_term(RawCB),
   Os = proplists:get_value(options, CB, []),
   Src0 = filename:rootname(Beam) ++ ".erl",
   Src1 = filename:join([filename:dirname(EbinDir), "src",
      filename:basename(Src0)]),
   Src2 = proplists:get_value(source, CB, []),
   try_sources([Src0, Src1, Src2], Os).

try_sources([], _) ->
   {error, nofile};
try_sources([Src | Rest], Os) ->
   case is_file(Src) of
      true -> parse_file(Src, Os);
      false -> try_sources(Rest, Os)
   end.

is_file(Name) ->
   case filelib:is_file(Name) of
      true ->
         not filelib:is_dir(Name);
      false ->
         false
   end.

parse_file(File, Opts) ->
   Cwd = ".",
   Dir = filename:dirname(File),
   IncludePath = [Cwd, Dir | inc_paths(Opts)],
   case epp:parse_file(File, IncludePath, pre_defs(Opts)) of
      {ok, Forms} ->
         record_attrs(Forms);
      Error ->
         Error
   end.

pre_defs([{d, M, V} | Opts]) ->
   [{M, V} | pre_defs(Opts)];
pre_defs([{d, M} | Opts]) ->
   [M | pre_defs(Opts)];
pre_defs([_ | Opts]) ->
   pre_defs(Opts);
pre_defs([]) -> [].

inc_paths(Opts) ->
   [P || {i, P} <- Opts, is_list(P)].

record_attrs(Forms) ->
   [{RecName, [record_field(OneAttr) || OneAttr <- FieldsAttr]} || {attribute, _, record, {RecName, FieldsAttr}} <- Forms].

record_field({record_field, _Anno, {_, _, Filed}}) ->
   Filed;
record_field({record_field, _Anno, {_, _, Filed}, _Val}) ->
   Filed;
record_field({typed_record_field, {record_field, _Anno, {_, _, Filed}}, _Type}) ->
   Filed;
record_field({typed_record_field, {record_field, _Anno, {_, _, Filed}, _Val}, _Type}) ->
   Filed;
record_field({typed_record_field, Field, _Type}) ->
   record_field(Field).
