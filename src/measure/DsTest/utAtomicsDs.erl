-module(utAtomicsDs).
-compile([nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).

-export([start/2]).

start(Num, Pid) ->
   Ds = init(Num),
   Time1 = erlang:monotonic_time(),
   NewDsI = insert(Num, Ds),
   Time2 = erlang:monotonic_time(),
   NewDsR = read(Num, NewDsI),
   Time3 = erlang:monotonic_time(),
   NewDsU = update(Num, NewDsR),
   Time4 = erlang:monotonic_time(),
   NewDsF = for(Num, NewDsU),
   Time5 = erlang:monotonic_time(),
   delete(Num, NewDsF),
   Time6 = erlang:monotonic_time(),
   erlang:send(Pid, {over, self(), Time2 - Time1, Time3 - Time2, Time4 - Time3, Time5 - Time4, not_support, no_size}),
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


