-module(utSMapsDs).
-compile([nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).

-export([start/2]).

start(Num, Pid) ->
   Ds = init(Num),
   Time1 = erlang:monotonic_time(),
   NewDsI = insert(Num, Ds),
   Time2 = erlang:monotonic_time(),
   NewDsR = read(Num, NewDsI, 0),
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
   maps:new().

insert(0, Ds) ->
   Ds;
insert(Num, Ds) ->
   NewDs = maps:put(Num, Num, Ds),
   insert(Num - 1, NewDs).

read(0, Ds, _V) ->
   Ds;
read(Num, Ds, _V) ->
   case Ds of
      #{Num := Value} ->
         read(Num - 1, Ds, Value);
      _ ->
         read(Num - 1, Ds, undefined)
   end.

update(0, Ds) ->
   Ds;
update(Num, Ds) ->
   NewDs = maps:update(Num, Num + 1, Ds),
   update(Num - 1, NewDs).

for(Num, Ds) ->
   Fun =
      fun(Key, Value, Acc) ->
         Value
      end,
   List = maps:fold(Fun, [], Ds),
   Ds.

delete(0, Ds) ->
   ok;
delete(Num, Ds) ->
   NewDs = maps:remove(Num, Ds),
   delete(Num - 1, NewDs).


