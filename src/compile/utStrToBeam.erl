-module(utStrToBeam).

-export([load/3]).

-spec load(Module :: atom(), Export :: [{Fun :: atom(), Arity :: pos_integer()}], Str :: string()) -> {module, Module :: atom()} | {error, _}.
load(Module, Export, Str) ->
   {ok, Tokens, _EndLine} = erl_scan:string(Str),
   {ok, Forms} = erl_parse:parse_form(Tokens),
   NewForms = [{attribute, 1, module, Module}, {attribute, 2, export, Export}, Forms],
   {ok, _, Binary} = compile:forms(NewForms),
   code:load_binary(Module, [], Binary).
