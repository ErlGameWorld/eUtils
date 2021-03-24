-module(utSArrayDs).
-compile([nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).

-export([start/2]).

start(Num, Pid) ->
   Ds = init(Num),
   Time1 = erlang:monotonic_time(),
   NewDsI = insert(Num - 1, Ds),
   Time2 = erlang:monotonic_time(),
   NewDsR = read(Num - 1, NewDsI),
   Time3 = erlang:monotonic_time(),
   NewDsU = update(Num - 1, NewDsR),
   Time4 = erlang:monotonic_time(),
   NewDsF = for(Num - 1, NewDsU),
   Time5 = erlang:monotonic_time(),
   delete(Num - 1, NewDsF),
   Time6 = erlang:monotonic_time(),
   erlang:send(Pid, {over, self(), Time2 - Time1, Time3 - Time2, Time4 - Time3, Time5 - Time4, not_support, utTermSize:byteSize(NewDsI)}),
   exit(normal).

init(Num) ->
   array:new(Num, fixed).

insert(0, Ds) ->
   array:set(0, 0, Ds);
insert(Num, Ds) ->
   NewDs = array:set(Num, Num, Ds),
   insert(Num - 1, NewDs).

read(0, Ds) ->
   Value = array:get(0, Ds),
   Ds;
read(Num, Ds) ->
   Value = array:get(Num, Ds),
   read(Num - 1, Ds).

update(0, Ds) ->
   array:set(0, 1, Ds);
update(Num, Ds) ->
   % Key = utTestDs:makeK(Num),
   NewDs = array:set(Num, Num + 1, Ds),
   update(Num - 1, NewDs).

for(0, Ds) ->
   Value = array:get(0, Ds),
   Ds;
for(Num, Ds) ->
   Value = array:get(Num, Ds),
   for(Num - 1, Ds).

delete(Num, Ds) ->
   ok.


