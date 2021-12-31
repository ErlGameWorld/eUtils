-module(utSeLfqDs).
-compile([nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).

-compile([export_all]).

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
   erlang:send(Pid, {over, self(), Time2 - Time1, Time3 - Time2, not_support, not_support, not_support, no_size}),
   ok.

init(_Num) ->
   {ok, Ref} = eLfq:new(),
   Ref.

insert(0, Ds) ->
   Ds;
insert(Num, Ds) ->
   eLfq:in(Ds, Num),
   insert(Num - 1, Ds).

read(0, Ds) ->
   Ds;
read(Num, Ds) ->
   Value = eLfq:tryOut(Ds),
   read(Num - 1, Ds).

update(Num, Ds) ->
   Ds.

for(Num, Ds) ->
   Ds.

delete(Num, Ds) ->
   Ds.


