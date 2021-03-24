-module(utSPdDs).
-compile([nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).

-export([start/2]).

start(Num, Pid) ->
   Ds = init(Num),
   Time1 = erlang:monotonic_time(),
   NewDsI = insert(Num, Ds),
   Time2 = erlang:monotonic_time(),
   NewDsR = read(Num, NewDsI, undefined),
   Time3 = erlang:monotonic_time(),
   NewDsU = update(Num, NewDsR),
   Time4 = erlang:monotonic_time(),
   NewDsF = for(Num, NewDsU),
   Time5 = erlang:monotonic_time(),
   delete(Num, NewDsF),
   Time6 = erlang:monotonic_time(),
   erlang:send(Pid, {over, self(), Time2 - Time1, Time3 - Time2, Time4 - Time3, Time5 - Time4, Time6 - Time5, no_size}),
   exit(normal).

init(Num) ->
   undefined.

insert(0, Ds) ->
   Ds;
insert(Num, Ds) ->
   erlang:put(Num, Num),
   insert(Num - 1, Ds).

read(0, Ds, _V) ->
   Ds;
read(Num, Ds, _V) ->
   Value = erlang:get(Num),
   read(Num - 1, Ds, Value).

update(0, Ds) ->
   Ds;
update(Num, Ds) ->
   erlang:put(Num, Num + 1),
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
   erlang:erase(Num),
   delete(Num - 1, Ds).


