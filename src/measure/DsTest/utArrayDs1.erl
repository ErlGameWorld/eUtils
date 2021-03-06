-module(utArrayDs1).
-compile([nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).

-export([start/2]).

start(Num, Pid) when Num =< 8192 ->
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
   erlang:send(Pid, {over, self(), Time2 - Time1, Time3 - Time2, Time4 - Time3, Time5 - Time4, not_support, utTermSize:byteSize(NewDsI)}),
   exit(normal);
start(Num, Pid) ->
   erlang:send(Pid, {over, self(), skip, skip, skip, skip, skip, no_size}),
   exit(normal).

init(Num) ->
   utArray:new(Num).

insert(0, Ds) ->
   Ds;
insert(Num, Ds) ->
   % Key = utTestDs:makeK(Num),
   NewDs = utArray:set(Num, utTestDs:makeV(Num), Ds),
   insert(Num - 1, NewDs).

read(0, Ds) ->
   Ds;
read(Num, Ds) ->
   % Key = utTestDs:makeK(Num),
   Value = utArray:get(Num, Ds),
   read(Num - 1, Ds).

update(0, Ds) ->
   Ds;
update(Num, Ds) ->
   NewDs = utArray:set(Num, utTestDs:makeV2(Num), Ds),
   update(Num - 1, NewDs).


for(Num, Ds) ->
   Ds.

delete(Num, Ds) ->
   ok.


