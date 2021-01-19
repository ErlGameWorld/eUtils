-module(utAtomicsDs).
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
   erlang:send(Pid, {over, self(), Time2 - Time1, Time3 - Time2, Time4 - Time3, Time5 - Time4, not_support}),
   exit(normal).

init(Num) ->
   atomics:new(Num, []).

insert(0, Ds) ->
   Ds;
insert(Num, Ds) ->
   atomics:put(Ds, Num, Num),
   insert(Num - 1, Ds).

read(0, Ds) ->
   Ds;
read(Num, Ds) ->
   Value = atomics:get(Ds, Num),
   read(Num - 1, Ds).

update(0, Ds) ->
   Ds;
update(Num, Ds) ->
   atomics:add(Ds, Num, 1),
   update(Num - 1, Ds).

for(0, Ds) ->
   Ds;
for(Num, Ds) ->
   atomics:get(Ds, Num),
   for(Num - 1, Ds).

delete(Num, Ds) ->
   ok.


