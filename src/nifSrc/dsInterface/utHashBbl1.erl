-module(utHashBbl1).
-import(erlang, [make_tuple/2, make_tuple/3]).
-compile([export_all, nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).

-define(Solt_1, 128).
-define(Solt_2, 128).

new() ->
   erlang:make_tuple(?Solt_1, []).

put(Key, Value, HashBbl1) ->
   BblNum1 = erlang:phash2(Key, ?Solt_1) + 1,
   BblNum2 = erlang:phash(Key, ?Solt_2),
   HashBbl2 = erlang:element(BblNum1, HashBbl1),
   case lists:keyfind(BblNum2, 1, HashBbl2) of
      false ->
         erlang:setelement(BblNum1, HashBbl1, [{BblNum2, {Key, Value}} | HashBbl2]);
      {BblNum2, ValueList} ->
         case ValueList of
            {Key, Value} ->
               HashBbl1;
            {Key, _OldValue} ->
               erlang:setelement(BblNum1, HashBbl1, lists:keyreplace(BblNum2, 1, HashBbl2, {BblNum2, {Key, Value}}));
            {_OldKey, _OldValue} = OldInfo ->
               erlang:setelement(BblNum1, HashBbl1, lists:keyreplace(BblNum2, 1, HashBbl2, {BblNum2, [{Key, Value}, OldInfo]}));
            List ->
               case lists:keyfind(Key, 1, List) of
                  false ->
                     erlang:setelement(BblNum1, HashBbl1, lists:keyreplace(BblNum2, 1, HashBbl2, {BblNum2, [{Key, Value} | List]}));
                  {Key, Value} ->
                     HashBbl1;
                  {Key, _OldValue} ->
                     erlang:setelement(BblNum1, HashBbl1, lists:keyreplace(BblNum2, 1, HashBbl2, {BblNum2, lists:keyreplace(Key, 1, List, {Key, Value})}))
               end
         end
   end.

get(Key, HashBbl1) ->
   BblNum1 = erlang:phash2(Key, ?Solt_1) + 1,
   BblNum2 = erlang:phash(Key, ?Solt_2),
   HashBbl2 = erlang:element(BblNum1, HashBbl1),
   case lists:keyfind(BblNum2, 1, HashBbl2) of
      false ->
         undefined;
      {BblNum2, ValueList} ->
         case ValueList of
            {Key, Value} ->
               Value;
            List ->
               case lists:keyfind(Key, 1, List) of
                  false ->
                     undefined;
                  {Key, Value} ->
                     Value
               end
         end
   end.
