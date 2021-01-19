-module(utTimeCase).

-compile([export_all, nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).

t1(N) ->
   tt1(N, 0).

tt1(0, Time) ->
   Time;
tt1(N, _Time) ->
   Time = time(),
   tt1(N - 1, Time).

t2(N) ->
   tt2(N, 0).

tt2(0, Date) ->
   Date;
tt2(N, _Date) ->
   Date = date(),
   tt2(N - 1, Date).

t3(N) ->
   tt3(N, 0).

tt3(0, Ms) ->
   Ms;
tt3(N, _Ms) ->
   Ms = erlang:system_time(millisecond),
   tt3(N - 1, Ms).

t4(N) ->
   tt4(N, 0).

tt4(0, Ns) ->
   Ns;
tt4(N, _Ns) ->
   Ns = erlang:system_time(second),
   tt4(N - 1, Ns).

t5(N) ->
   tt5(N, 0).

tt5(0, Tt) ->
   Tt;
tt5(N, _Tt) ->
   Tt = os:timestamp(),
   tt5(N - 1, Tt).

t6(N) ->
   tt6(N, 0).

tt6(0, Tt) ->
   Tt;
tt6(N, _Tt) ->
   {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
   Tt = MegaSecs * 1000000 + Secs,
   tt6(N - 1, Tt).
