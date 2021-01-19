-module(utKvsToBeam).

-export([
   load/2
   , beamToSrc/1
]).

%% 注意 map类型的数据不能当做key
-type key() :: atom() | binary() | bitstring() | float() | integer() | list() | tuple().
-type value() :: atom() | binary() | bitstring() | float() | integer() | list() | tuple() | map().

-spec load(term(), [{key(), value()}]) -> ok.
load(Module, KVs) ->
   Forms = forms(Module, KVs),
   {ok, Module, Bin} = compile:forms(Forms),
   code:soft_purge(Module),
   {module, Module} = code:load_binary(Module, atom_to_list(Module), Bin),
   ok.

forms(Module, KVs) ->
   %% -module(Module).
   Mod = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(Module)]),
   %% -export([get/0]).
   ExportList = [erl_syntax:arity_qualifier(erl_syntax:atom(get), erl_syntax:integer(1))],
   Export = erl_syntax:attribute(erl_syntax:atom(export), [erl_syntax:list(ExportList)]),
   %% get(K) -> V
   Function = erl_syntax:function(erl_syntax:atom(get), lookup_clauses(KVs, [])),
   [erl_syntax:revert(X) || X <- [Mod, Export, Function]].

lookup_clause(Key, Value) ->
   Var = erl_syntax:abstract(Key),
   Body = erl_syntax:abstract(Value),
   erl_syntax:clause([Var], [], [Body]).

lookup_clause_anon() ->
   Var = erl_syntax:variable("_"),
   Body = erl_syntax:atom(undefined),
   erl_syntax:clause([Var], [], [Body]).

lookup_clauses([], Acc) ->
   lists:reverse(lists:flatten([lookup_clause_anon() | Acc]));
lookup_clauses([{Key, Value} | T], Acc) ->
   lookup_clauses(T, [lookup_clause(Key, Value) | Acc]).

%% 通过beam生成erl文件，生成的beam编译选项必要带debug_info才行
beamToSrc(Module) ->
   case beam_lib:chunks(code:which(Module), [abstract_code]) of
      {ok, {_, [{abstract_code, {_, AC}}]}} ->
         Code = erl_prettypr:format(erl_syntax:form_list(AC)),
         file:write_file(lists:concat([Module, ".erl"]), list_to_binary(Code)),
         io:format("build beam:~p to erl:~p success.~n", [Module, Module]);
      {error, beam_lib, Reason} ->
         io:format("code_gen_erl_file error, reason:~p~n", [Reason]);
      _Err ->
         io:format("code_gen_erl_file error, reason:~p~n", [_Err])
   end.
