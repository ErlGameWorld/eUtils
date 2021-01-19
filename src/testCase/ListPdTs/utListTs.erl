-module(utListTs).
-compile([nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).

-export([start/2]).

start(Num, Pid) when Num =< 32768 ->
   Ds = init(Num),
   Time1 = erlang:system_time(nanosecond),
   NewDsI = insert(Num, Ds),
   Time2 = erlang:system_time(nanosecond),
   NewDsU = read(Num, NewDsI),
   Time3 = erlang:system_time(nanosecond),
   delete(NewDsU),
   Time4 = erlang:system_time(nanosecond),
   erlang:send(Pid, {over, self(), Time2 - Time1, Time3 - Time2, Time4 - Time3}),
   exit(normal);
start(_Num, Pid) ->
   erlang:send(Pid, {over, self(), skip, skip, skip, skip, skip}),
   exit(normal).

init(_Num) ->
   [].

insert(0, Ds) ->
   Ds;
insert(Num, Ds) ->
   Value = utListPdTs:makeV(Num),
   insert(Num - 1, [Value | Ds]).

read(_Num, Ds) ->
   lists:reverse(Ds).

delete([]) ->
   ok;
delete([_H | T]) ->
   delete(T).


