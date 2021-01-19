-module(utTupleDs).
-compile([nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).

-export([start/2]).

start(Num, Pid) when Num =< 65536 ->
   Ds = init(Num),
   Time1 = erlang:system_time(nanosecond),
   NewDsI = insert(Num, Ds),
   Time2 = erlang:system_time(nanosecond),
   NewDsR = read(Num, NewDsI, undefined),
   Time3 = erlang:system_time(nanosecond),
   NewDsU = update(Num, NewDsR),
   Time4 = erlang:system_time(nanosecond),
   NewDsF = for(Num, NewDsU, undefined),
   Time5 = erlang:system_time(nanosecond),
   delete(Num, NewDsF),
   Time6 = erlang:system_time(nanosecond),
   erlang:send(Pid, {over, self(), Time2 - Time1, Time3 - Time2, Time4 - Time3, Time5 - Time4, not_support}),
   exit(normal);
start(Num, Pid) ->
   erlang:send(Pid, {over, self(), skip, skip, skip, skip, skip}),
   exit(normal).

init(Num) ->
   erlang:make_tuple(Num, undefined).

insert(0, Ds) ->
   Ds;
insert(Num, Ds) ->
   % Key = utTestDs:makeK(Num),
   NewDs = erlang:setelement(Num, Ds, utTestDs:makeV(Num)),
   insert(Num - 1, NewDs).

read(0, Ds, _V) ->
   Ds;
read(Num, Ds, _V) ->
   % Key = utTestDs:makeK(Num),
   Value = erlang:element(Num, Ds),

   read(Num - 1, Ds, Value).

update(0, Ds) ->
   Ds;
update(Num, Ds) ->
   % Key = utTestDs:makeK(Num),
   NewDs = erlang:setelement(Num, Ds, utTestDs:makeV2(Num)),
   update(Num - 1, NewDs).

for(0, Ds, _V) ->
   Ds;
for(Num, Ds, _V) ->
   Value = erlang:element(Num, Ds),
   for(Num - 1, Ds, Value).

delete(Num, Ds) ->
   ok.


