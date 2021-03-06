-module(utNifArrayDs).
-compile([nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).

-export([start/2]).

start(Num, Pid) ->
   Ds = init(Num),
   Time1 = erlang:monotonic_time(),
   NewDsI = insert(Num - 1, Ds),
   Time2 = erlang:monotonic_time(),
   NewDsR = read(Num - 1, Ds),
   Time3 = erlang:monotonic_time(),
   NewDsU = update(Num - 1, Ds),
   Time4 = erlang:monotonic_time(),
   NewDsF = for(Num - 1, Ds),
   Time5 = erlang:monotonic_time(),
   delete(Num - 1, Ds),
   Time6 = erlang:monotonic_time(),
   erlang:send(Pid, {over, self(), Time2 - Time1, Time3 - Time2, Time4 - Time3, Time5 - Time4, not_support, no_size}),
   exit(normal).

init(Num) ->
   nifArray:new(Num).

insert(0, Ds) ->
   % Key = utTestDs:makeK(0),
   nifArray:put(Ds, 0, utTestDs:makeV(0));
insert(Num, Ds) ->
   % Key = utTestDs:makeK(Num),
   nifArray:put(Ds, Num, utTestDs:makeV(Num)),
   insert(Num - 1, Ds).

read(0, Ds) ->
   % Key = utTestDs:makeK(0),
   Value = nifArray:get(Ds, 0),
   Ds;
read(Num, Ds) ->
   % Key = utTestDs:makeK(Num),
   Value = nifArray:get(Ds, Num),
   read(Num - 1, Ds).

update(0, Ds) ->
   % Key = utTestDs:makeK(0),
   nifArray:put(Ds, 0, utTestDs:makeV2(0));
update(Num, Ds) ->
   % Key = utTestDs:makeK(Num),
   nifArray:put(Ds, Num, utTestDs:makeV2(Num)),
   update(Num - 1, Ds).

for(0, Ds) ->
   Value = nifArray:get(Ds, 0),
   Ds;
for(Num, Ds) ->
   Value = nifArray:get(Ds, Num),
   for(Num - 1, Ds).

delete(Num, Ds) ->
   ok.


