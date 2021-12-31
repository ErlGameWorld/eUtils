-module(utSFwQueueDs).
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
   erlang:send(Pid, {over, self(), Time2 - Time1, Time3 - Time2, not_support, not_support, not_support, ets:info(test_fwQueue, memory)}),
   exit(normal).

init(_Num) ->
   case fwQueue:new(test_fwQueue) of
      name_used ->
         fwQueue:clear(test_fwQueue);
      _ ->
         ok
   end.

insert(0, Ds) ->
   Ds;
insert(Num, _Ds) ->
   NewDs = fwQueue:in(test_fwQueue, Num),
   insert(Num - 1, NewDs).

read(0, Ds) ->
   Ds;
read(Num, Ds) ->
   Value = fwQueue:outF(test_fwQueue),
   read(Num - 1, Ds).

update(Num, Ds) ->
   Ds.

for(Num, Ds) ->
   Ds.

delete(Num, Ds) ->
   Ds.


