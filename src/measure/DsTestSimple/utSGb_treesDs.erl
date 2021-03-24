-module(utSGb_treesDs).
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
   gb_trees:empty().

insert(0, Ds) ->
   Ds;
insert(Num, Ds) ->
   NewDs = gb_trees:enter(Num, Num, Ds),
   insert(Num - 1, NewDs).

read(0, Ds) ->
   Ds;
read(Num, Ds) ->
   Value = gb_trees:lookup(Num, Ds),
   read(Num - 1, Ds).

update(0, Ds) ->
   Ds;
update(Num, Ds) ->
   NewDs = gb_trees:update(Num, Num + 1, Ds),
   update(Num - 1, NewDs).

for(Num, Ds) ->
   List = gb_trees:to_list(Ds),
   for1(List),
   Ds.

for1([]) ->
   ok;
for1([H | T]) ->
   for1(T).

delete(0, Ds) ->
   ok;
delete(Num, Ds) ->
   NewDs = gb_trees:delete_any(Num, Ds),
   delete(Num - 1, NewDs).


