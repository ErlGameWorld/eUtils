-module(utSUtFifoDs).
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
   erlang:send(Pid, {over, self(), Time2 - Time1, Time3 - Time2, not_support, not_support, not_support, ets:info(test_fifo, memory)}),
   exit(normal).

init(_Num) ->
   case utFifo:new(test_fifo) of
      name_used ->
         utFifo:clear(test_fifo);
      _ ->
         ok
   end.

insert(0, Ds) ->
   Ds;
insert(Num, _Ds) ->
   NewDs = utFifo:in(test_fifo, Num),
   insert(Num - 1, NewDs).

read(0, Ds) ->
   Ds;
read(Num, Ds) ->
   Value = utFifo:out(test_fifo),
   read(Num - 1, Ds).

update(Num, Ds) ->
   Ds.

for(Num, Ds) ->
   Ds.

delete(Num, Ds) ->
   Ds.


