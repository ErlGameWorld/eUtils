-module(utPTermDs).
-compile([nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).

-export([start/2]).

start(Num, Pid) when Num =< 131072 ->
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
   erlang:send(Pid, {over, self(), Time2 - Time1, Time3 - Time2, not_support, not_support, not_support}),
   exit(normal);
start(Num, Pid) ->
   erlang:send(Pid, {over, self(), skip, skip, skip, skip, skip}),
   exit(normal).

init(Num) ->
   undefined.

insert(0, Ds) ->
   Ds;
insert(Num, Ds) ->
   Key = utTestDs:makeK(Num),
   Value = utTestDs:makeV(Num),
   persistent_term:put(Key, Value),
   insert(Num - 1, Ds).

read(0, Ds) ->
   Ds;
read(Num, Ds) ->
   Key = utTestDs:makeK(Num),
   persistent_term:get(Key),
   read(Num - 1, Ds).

update(Num, Ds) ->
   Ds.

for(Num, Ds) ->
   Ds.

delete(Num, Ds) ->
   ok.


