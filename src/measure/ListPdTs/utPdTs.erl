-module(utPdTs).
-compile([nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).

-export([start/2]).

start(Num, Pid) ->
   Ds = init(Num),
   Time1 = erlang:monotonic_time(),
   NewDsI = insert(Num, Ds),
   Time2 = erlang:monotonic_time(),
   NewDsR = read(Num, NewDsI, undefined),
   Time3 = erlang:monotonic_time(),
   delete(Num, NewDsR),
   Time4 = erlang:monotonic_time(),
   erlang:send(Pid, {over, self(), Time2 - Time1, Time3 - Time2, Time4 - Time3}),
   exit(normal).

init(Num) ->
   undefined.

insert(0, Ds) ->
   Ds;
insert(Num, Ds) ->
   Value = utListPdTs:makeV(Num),
   erlang:put(Num, Value),
   insert(Num - 1, Ds).

read(0, Ds, _V) ->
   Ds;
read(Num, Ds, _V) ->
   V = erlang:get(Num),
   read(Num - 1, Ds, V).

delete(0, _Ds) ->
   ok;
delete(Num, Ds) ->
   erlang:erase(Num),
   delete(Num - 1, Ds).


