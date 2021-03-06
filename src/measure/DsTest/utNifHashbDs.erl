-module(utNifHashbDs).
-compile([nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).

-export([start/2]).

start(Num, Pid) ->
   Ds = init(Num),
   Time1 = erlang:monotonic_time(),
   NewDsI = insert(Num, Ds),
   Time2 = erlang:monotonic_time(),
   NewDsR = read(Num, Ds, undefined),
   Time3 = erlang:monotonic_time(),
   NewDsU = update(Num, Ds),
   Time4 = erlang:monotonic_time(),
   NewDsF = for(Num, Ds),
   Time5 = erlang:monotonic_time(),
   delete(Num, Ds),
   Time6 = erlang:monotonic_time(),
   erlang:send(Pid, {over, self(), Time2 - Time1, Time3 - Time2, Time4 - Time3, Time5 - Time4, Time6 - Time5, no_size}),
   exit(normal).

init(Num) ->
   nifHashb:new().

insert(0, Ds) ->
   Ds;
insert(Num, Ds) ->
   Key = utTestDs:makeK(Num),
   Value = utTestDs:makeV(Num),
   nifHashb:put(Ds, Key, Value),
   insert(Num - 1, Ds).

read(0, Ds, _V) ->
   Ds;
read(Num, Ds, _V) ->
   Key = utTestDs:makeK(Num),
   Value = nifHashb:get(Ds, Key),
   read(Num - 1, Ds, Value).

update(0, Ds) ->
   Ds;
update(Num, Ds) ->
   Key = utTestDs:makeK(Num),
   Value = utTestDs:makeV2(Num),
   nifHashb:put(Ds, Key, Value),
   update(Num - 1, Ds).

for(Num, Ds) ->
   Ds.


delete(Num, Ds) ->
   ok.


