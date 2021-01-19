-module(utProMsg).

-compile([export_all, nowarn_function, nowarn_unused_vars, nowarn_export_all]).

test(N) ->
   new(N, self()),
   receive
      Msg ->
         io:format("time end is ~p~n", [erlang:system_time(millisecond)]),
         Msg
   end.

new(0, Pid) ->
   erlang:spawn(?MODULE, pro, [0, Pid]);
new(N, Pid) ->
   NewPid = erlang:spawn(?MODULE, pro, [N, Pid]),
   new(N - 1, NewPid).

pro(0, Pid) ->
   Term = utGenTerm:genBinary(2000),
   io:format("time start is ~p~n", [erlang:system_time(millisecond)]),
   Pid ! Term;
pro(_N, Pid) ->
   receive
      Msg ->
         Pid ! Msg
   end.

test1(N) ->
   new1(N, self()),
   receive
      Msg ->
         io:format("time end is ~p~n", [erlang:system_time(millisecond)]),
         Msg
   end.

new1(0, Pid) ->
   erlang:spawn(?MODULE, pro1, [0, Pid]);
new1(N, Pid) ->
   NewPid = erlang:spawn(?MODULE, pro1, [N, Pid]),
   new1(N - 1, NewPid).

pro1(0, Pid) ->
   Term = utGenTerm:genString(2000),
   io:format("time start is ~p~n", [erlang:system_time(millisecond)]),
   Pid ! Term;
pro1(_N, Pid) ->
   receive
      Msg ->
         Pid ! Msg
   end.


