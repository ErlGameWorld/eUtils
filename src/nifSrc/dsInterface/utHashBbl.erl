-module(utHashBbl).
-import(erlang, [make_tuple/2, make_tuple/3]).
-compile([export_all, nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).

-define(Solt_1, 128).
-define(Solt_2, 128).

new() ->
   erlang:make_tuple(?Solt_1, erlang:make_tuple(?Solt_2, undefined)).

put(Key, Value, HashBbl1) ->
   BblNum1 = erlang:phash2(Key, ?Solt_1) + 1,
   BblNum2 = erlang:phash(Key, ?Solt_2),
   HashBbl2 = erlang:element(BblNum1, HashBbl1),
   ValueList = erlang:element(BblNum2, HashBbl2),
   case ValueList of
      undefined ->
         erlang:setelement(BblNum1, HashBbl1, erlang:setelement(BblNum2, HashBbl2, {Key, Value}));
      {Key, Value} ->
         HashBbl1;
      {Key, _OldValue} ->
         erlang:setelement(BblNum1, HashBbl1, erlang:setelement(BblNum2, HashBbl2, {Key, Value}));
      {_OldKey, _OldValue} = OldInfo ->
         erlang:setelement(BblNum1, HashBbl1, erlang:setelement(BblNum2, HashBbl2, [{Key, Value}, OldInfo]));
      List ->
         case lists:keyfind(Key, 1, List) of
            false ->
               erlang:setelement(BblNum1, HashBbl1, erlang:setelement(BblNum2, HashBbl2, [{Key, Value} | List]));
            {Key, Value} ->
               HashBbl1;
            {Key, _OldValue} ->
               erlang:setelement(BblNum1, HashBbl1, erlang:setelement(BblNum2, HashBbl2, lists:keyreplace(Key, 1, List, {Key, Value})))
         end
   end.

get(Key, HashBbl1) ->
   BblNum1 = erlang:phash2(Key, ?Solt_1) + 1,
   BblNum2 = erlang:phash(Key, ?Solt_2),
   HashBbl2 = erlang:element(BblNum1, HashBbl1),
   ValueList = erlang:element(BblNum2, HashBbl2),
   case ValueList of
      undefined ->
         undefined;
      {Key, Value} ->
         Value;
      List ->
         case lists:keyfind(Key, 1, List) of
            false ->
               undefined;
            {Key, Value} ->
               Value
         end
   end.
