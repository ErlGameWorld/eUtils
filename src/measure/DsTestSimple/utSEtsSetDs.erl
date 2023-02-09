-module(utSEtsSetDs).
-compile([nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).
-compile([export_all]) .
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
   erlang:send(Pid, {over, self(), Time2 - Time1, Time3 - Time2, Time4 - Time3, Time5 - Time4, Time6 - Time5, ets:info(Ds, memory) * erlang:system_info(wordsize)}),
   exit(normal).

init(_Num) ->
   ets:new(test_ets, [set, public, {write_concurrency, true}]).

insert(0, Ds) ->
   Ds;
insert(Num, Ds) ->
   ets:insert(Ds, {Num, Num}),
   insert(Num - 1, Ds).

read(0, Ds) ->
   Ds;
read(Num, Ds) ->
   Value = ets:lookup(Ds, Num),
   read(Num - 1, Ds).

update(0, Ds) ->
   Ds;
update(Num, Ds) ->
   ets:update_element(Ds, Num, {2, Num + 1}),
   update(Num - 1, Ds).

for(Num, Ds) ->
   Fun =
      fun({Key, Value}, Acc) ->
         Value
      end,
   List = ets:foldl(Fun, [], Ds),
   Ds.

delete(0, Ds) ->
   ok;
delete(Num, Ds) ->
   ets:delete(Ds, Num),
   delete(Num - 1, Ds).


