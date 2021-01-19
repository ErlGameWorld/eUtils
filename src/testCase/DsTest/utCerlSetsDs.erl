-module(utCerlSetsDs).
-compile([nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).

-export([start/2]).

start(Num, Pid) ->
   Ds = init(Num),
   Time1 = erlang:system_time(nanosecond),
   NewDsI = insert(Num, Ds),
   Time2 = erlang:system_time(nanosecond),
   NewDsR = read(Num, NewDsI),
   Time3 = erlang:system_time(nanosecond),
   NewDsU = update(Num, NewDsR),
   Time4 = erlang:system_time(nanosecond),
   NewDsF = for(Num, NewDsU),
   Time5 = erlang:system_time(nanosecond),
   delete(Num, NewDsF),
   Time6 = erlang:system_time(nanosecond),
   erlang:send(Pid, {over, self(), Time2 - Time1, Time3 - Time2, not_support, Time5 - Time4, Time6 - Time5}),
   exit(normal).

init(_Num) ->
   cerl_sets:new().

insert(0, Ds) ->
   Ds;
insert(Num, Ds) ->
   Key = utTestDs:makeK(Num),
   NewDs = cerl_sets:add_element(Key, Ds),
   insert(Num - 1, NewDs).

read(0, Ds) ->
   Ds;
read(Num, Ds) ->
   Key = utTestDs:makeK(Num),
   Value = cerl_sets:is_element(Key, Ds),
   read(Num - 1, Ds).

update(Num, Ds) ->
   Ds.

for(Num, Ds) ->
   Fun =
      fun(Value, Acc) ->
         Value
      end,
   cerl_sets:fold(Fun, [], Ds),
   Ds.

delete(0, Ds) ->
   ok;
delete(Num, Ds) ->
   Key = utTestDs:makeK(Num),
   NewDs = cerl_sets:del_element(Key, Ds),
   delete(Num - 1, NewDs).