-module(utPdDs).
-compile([nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).

-export([start/2]).

start(Num, Pid) ->
   Ds = init(Num),
   Time1 = erlang:system_time(nanosecond),
   NewDsI = insert(Num, Ds),
   Time2 = erlang:system_time(nanosecond),
   NewDsR = read(Num, NewDsI, undefined),
   Time3 = erlang:system_time(nanosecond),
   NewDsU = update(Num, NewDsR),
   Time4 = erlang:system_time(nanosecond),
   NewDsF = for(Num, NewDsU),
   Time5 = erlang:system_time(nanosecond),
   delete(Num, NewDsF),
   Time6 = erlang:system_time(nanosecond),
   erlang:send(Pid, {over, self(), Time2 - Time1, Time3 - Time2, Time4 - Time3, Time5 - Time4, Time6 - Time5}),
   exit(normal).

init(Num) ->
   undefined.

insert(0, Ds) ->
   Ds;
insert(Num, Ds) ->
   Key = utTestDs:makeK(Num),
   Value = utTestDs:makeV(Num),
   erlang:put(Key, Value),
   insert(Num - 1, Ds).

read(0, Ds, _V) ->
   Ds;
read(Num, Ds, _V) ->
   Key = utTestDs:makeK(Num),
   Value = erlang:get(Key),
   read(Num - 1, Ds, Value).

update(0, Ds) ->
   Ds;
update(Num, Ds) ->
   Key = utTestDs:makeK(Num),
   Value = utTestDs:makeV2(Num),
   erlang:put(Key, Value),
   update(Num - 1, Ds).

for(Num, Ds) ->
   Keylist = erlang:get(),
   for1(Keylist, undefined),
   Ds.

for1([], V) ->
   ok;
for1([H | T], _V) ->
   V = erlang:get(H),
   for1(T, V).

delete(0, Ds) ->
   ok;
delete(Num, Ds) ->
   Key = utTestDs:makeK(Num),
   erlang:erase(Key),
   delete(Num - 1, Ds).


