-module(utDictDs).
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
   erlang:send(Pid, {over, self(), Time2 - Time1, Time3 - Time2, Time4 - Time3, Time5 - Time4, Time6 - Time5, utTermSize:byteSize(NewDsI)}),
   exit(normal).

init(_Num) ->
   dict:new().

insert(0, Ds) ->
   Ds;
insert(Num, Ds) ->
   Key = utTestDs:makeK(Num),
   Value = utTestDs:makeV(Num),
   NewDs = dict:store(Key, Value, Ds),
   insert(Num - 1, NewDs).

read(0, Ds) ->
   Ds;
read(Num, Ds) ->
   Key = utTestDs:makeK(Num),
   Value = dict:find(Key, Ds),
   read(Num - 1, Ds).

update(0, Ds) ->
   Ds;
update(Num, Ds) ->
   Key = utTestDs:makeK(Num),
   Value = utTestDs:makeV2(Num),
   NewDs = dict:store(Key, Value, Ds),
   update(Num - 1, NewDs).

for(Num, Ds) ->
   Fun =
      fun(Key, Value, Acc) ->
         Value
      end,
   List = dict:fold(Fun, [], Ds),
   Ds.

delete(0, Ds) ->
   ok;
delete(Num, Ds) ->
   Key = utTestDs:makeK(Num),
   NewDs = dict:erase(Key, Ds),
   delete(Num - 1, NewDs).


