-module(utSDictDs).
-compile([nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).

-export([start/2]).

start(Num, Pid) ->
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
   erlang:send(Pid, {over, self(), Time2 - Time1, Time3 - Time2, Time4 - Time3, Time5 - Time4, Time6 - Time5}),
   exit(normal).

init(_Num) ->
   dict:new().

insert(0, Ds) ->
   Ds;
insert(Num, Ds) ->
   NewDs = dict:store(Num, Num, Ds),
   insert(Num - 1, NewDs).

read(0, Ds) ->
   Ds;
read(Num, Ds) ->
   Value = dict:find(Num, Ds),
   read(Num - 1, Ds).

update(0, Ds) ->
   Ds;
update(Num, Ds) ->
   NewDs = dict:store(Num, Num+1, Ds),
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
   NewDs = dict:erase(Num, Ds),
   delete(Num - 1, NewDs).


