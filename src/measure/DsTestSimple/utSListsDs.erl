-module(utSListsDs).
-compile([nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).

-export([start/2]).

start(Num, Pid) when Num =< 32768 ->
   Ds = init(Num),
   Time1 = erlang:monotonic_time(),
   NewDsI = insert(Num, Ds),
   Time2 = erlang:monotonic_time(),
   NewDsR = read(Num, NewDsI),
   Time3 = erlang:monotonic_time(),
   NewDsU = update(Num, NewDsR),
   Time4 = erlang:monotonic_time(),
   NewDsF = for(NewDsU, NewDsU),
   Time5 = erlang:monotonic_time(),
   delete(Num, NewDsF),
   Time6 = erlang:monotonic_time(),
   erlang:send(Pid, {over, self(), Time2 - Time1, Time3 - Time2, Time4 - Time3, Time5 - Time4, Time6 - Time5, utTermSize:byteSize(NewDsI)}),
   exit(normal);
start(Num, Pid) ->
   erlang:send(Pid, {over, self(), skip, skip, skip, skip, skip, no_size}),
   exit(normal).

init(_Num) ->
   [].

insert(0, Ds) ->
   Ds;
insert(Num, Ds) ->
   insert(Num - 1, [{Num, Num} | Ds]).

read(0, Ds) ->
   Ds;
read(Num, Ds) ->
   Value = lists:keyfind(Num, 1, Ds),
   read(Num - 1, Ds).

update(0, Ds) ->
   Ds;
update(Num, Ds) ->
   NewDs = lists:keyreplace(Num, 1, Ds, {Num, Num + 1}),
   update(Num - 1, NewDs).

for([], Array) ->
   Array;
for([H | T], Array) ->
   for(T, Array).

delete(0, _List) ->
   ok;
delete(Num, Ds) ->
   NewDs = lists:keydelete(Num, 1, Ds),
   delete(Num - 1, NewDs).


