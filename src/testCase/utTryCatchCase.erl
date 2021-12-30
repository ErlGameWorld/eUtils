-module(utTryCatchCase).

-compile([export_all, nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).

-export([
   loopTest/4
   , testTryCatch/1
   , testTryCatch/2
   , testCatch/1
   , testCatch/2
   , testTryCatch2/1
   , testTryCatch2/2
]).

t1() ->
   Pid = spawn(fun() -> do_t1(1) end),
   send_msg(Pid, 100000).

t2() ->
   Pid = spawn(fun() -> do_t2(1) end),
   send_msg(Pid, 100000).

send_msg(Pid, 0) ->
   io:format("IMY***********send_msg over ~p ~n  ~p ~n", [0, Pid]),
   ok;
send_msg(Pid, N) ->
   Pid ! <<2:(N)>>,
   % timer:sleep(1),
   send_msg(Pid, N - 1).

do_t1(N) ->
   case N == 1 orelse N == 90000 of
      true ->
         io:format("IMY************************* ~p~n ~n", [self()]),
         erlang:garbage_collect(),
         Result = erlang:process_info(self(), [memory, garbage_collection]),
         io:format("IMY************************* ~p~n  ~w ~n", [N, Result]);
         %io:format("IMY&&&&&&&&&&&&&&&&&&&&&&&&& ~p~n backtrace:~p~n~n", [N, erlang:process_display(self(), backtrace)]);
      _ ->
         ignore
   end,


   try
      receive
         _ ->
            ok
      %% do_t1(N + 1)
      end,
      do_t1(N + 1)
   catch
      _:_ ->
         do_t1(N + 1)
   end.

do_t2(N) ->
   case N == 1 orelse N == 90000 of
      true ->
         erlang:garbage_collect(),
         Result = erlang:process_info(self(), [memory, garbage_collection]),
         io:format("IMY************************* ~p ~n  ~w ~n", [N, Result]),
         io:format("IMY&&&&&&&&&&&&&&&&&&&&&&&&& ~p ~n backtrace:~w~n~n", [N, erlang:process_display(self(), backtrace)]);
      _ ->
         %io:format("IMY************************* ~p ~n", [N]),
         ignore
   end,
   receive
      _ ->
         do_t2(N + 1)
   end.

desult3(N, State) ->
   %io:format("IMy*********************desult3 ~p~n",[N]),
   do_t3(N).

desult4(N, State) ->
   do_t4(N).

t3() ->
   Pid = spawn(fun() -> do_t3(100000) end).

t4() ->
   Pid = spawn(fun() -> do_t4(100000) end).

t5() ->
   Pid = spawn(fun() -> do_exit() end).

do_t3(0) ->
   io:format("IMy*********************dfdfdffd33333333");
do_t3(N) ->
   case N == 1 orelse N == 99999 of
      true ->
         erlang:garbage_collect(),
         Result = erlang:process_info(self(), [memory, garbage_collection]),
         io:format("IMY************************* ~p~n  ~w ~n", [N, Result]),
         io:format("IMY&&&&&&&&&&&&&&&&&&&&&&&&& ~p~n backtrace:~p~n~n", [N, erlang:process_display(self(), backtrace)]);
      _ ->
         ignore
   end,

   try utFunCallCase:tt9(2) of
      Ret ->
         desult3(N - 1, Ret)
   catch
      _:_ ->
         desult3(N - 1, error)
   end.
do_t4(0) ->
   io:format("IMy*********************dfdfdffd44444444444");
do_t4(N) ->
   case N == 1 orelse N == 99999 of
      true ->
         erlang:garbage_collect(),
         Result = erlang:process_info(self(), [memory, garbage_collection]),
         io:format("IMY************************* ~p ~n  ~w ~n", [N, Result]),
         io:format("IMY&&&&&&&&&&&&&&&&&&&&&&&&& ~p ~n backtrace:~w~n~n", [N, erlang:process_display(self(), backtrace)]);
      _ ->
         %io:format("IMY************************* ~p ~n", [N]),
         ignore
   end,
   Ret1 = try utFunCallCase:tt9(2) of
             Ret ->
                Ret
          catch
             _:_ ->
                error
          end,
   do_t4(N - 1).

do_t5(0) ->
   io:format("IMy*********************dfdfdffd55555555555555");
do_t5(N) ->
   case N == 1 orelse N == 99999 of
      true ->
         erlang:garbage_collect(),
         Result = erlang:process_info(self(), [memory, garbage_collection]),
         io:format("IMY************************* ~p ~n  ~w ~n", [N, Result]),
         io:format("IMY&&&&&&&&&&&&&&&&&&&&&&&&& ~p ~n backtrace:~w~n~n", [N, erlang:process_display(self(), backtrace)]);
      _ ->
         %io:format("IMY************************* ~p ~n", [N]),
         ignore
   end,
   try utFunCallCase:tt9(2) of
      Ret ->
         do_t5(N - 1)
   catch
      _:_ ->
         error
   end.


do_t6(0) ->
   io:format("IMy*********************dfdfdffd55555555555555");

do_t6(N) ->
   case N == 1 orelse N == 99999 of
      true ->
         erlang:garbage_collect(),
         Result = erlang:process_info(self(), [memory, garbage_collection]),
         io:format("IMY************************* ~p ~n  ~w ~n", [N, Result]);
      %io:format("IMY&&&&&&&&&&&&&&&&&&&&&&&&& ~p ~n backtrace:~w~n~n", [N, erlang:process_display(self(), backtrace)]);
      _ ->
         %io:format("IMY************************* ~p ~n", [N]),
         ignore
   end,
   try utFunCallCase:tt9(2) of
      Ret ->
         do_t6(N - 1)
   catch
      _:_ ->
         error
      %after
      %   ok
   end.

do_t7(0) ->
   io:format("IMy********************11111");
do_t7(N) ->
   io:format("IMy********************22222"),
   try utFunCallCase:tt9(2) of
      _Ret ->
         io:format("IMy********************3333"),
         do_t7(N - 1)
   catch
      throw:ret ->
         io:format("IMy********************4444"),
         do_t7(N - 1);
      _:_ ->
         io:format("IMy********************5555"),
         error
   end.

do_exit() ->
   try
      io:format("IMY**************************do_exit try111~n"),
      io:format("IMY**************************do_exit try222~n"),
      io:format("IMY**************************do_exit try333~n"),
      io:format("IMY**************************do_exit try444~n"),
      erlang:raise(error, test, []),
      io:format("IMY**************************do_exit try555~n")
   catch
      _A ->
         io:format("IMY**************************do_exit catch ~p ~n", [_A])
   after
      io:format("IMY**************************do_exit after ~n")
   end.

do_exit1() ->
   try
      io:format("IMY**************************do_exit try111~n"),
      io:format("IMY**************************do_exit try222~n"),
      io:format("IMY**************************do_exit try333~n"),
      io:format("IMY**************************do_exit try444~n"),
      erlang:raise(error, test, []),
      io:format("IMY**************************do_exit try555~n")
   catch
      _A:_ ->
         io:format("IMY**************************do_exit catch ~p ~p ~p ~n", [_A, 1, 1])
   after
      io:format("IMY**************************do_exit after ~n")
   end.


loopTest(0, _, _, _) -> ok;
loopTest(Times, Fun, Type, IsLoop) ->
   ?MODULE:Fun(Type, IsLoop),
   loopTest(Times - 1, Fun, Type, IsLoop).

testNoCatch(Type) ->
   testNoCatch(Type, false).

testNoCatch(Type, _TestLoop) ->
   makeException(Type).

testTryCatch(Type) ->
   testTryCatch(Type, false).

testTryCatch(Type, TestLoop) ->
   try
      makeException(Type)
   catch
      Type:Reason ->
         case TestLoop of
            true -> ok;
            false -> io:format("try .. catch block caught exception of ~p: ~p~n", [Type, Reason])
         end
   end.

testTryCatch2(Type) ->
   testTryCatch2(Type, false).

testTryCatch2(Type, TestLoop) ->
   try
      makeException(Type)
   catch
      Type:Reason:Strace ->
         case TestLoop of
            true -> Strace;
            false ->
               S1 = utParseStack:parseStack(Strace),
               S2 = utParseStack:parseStack(element(2, erlang:process_info(self(), current_stacktrace))),
               io:format(S1),
               io:format(S2),
               io:format("try .. catch block caught exception of ~p: ~p: ~n ~s:~n", [Type, Reason, io_lib:format(S1, [])]),
               io:format("try .. catch block caught exception of ~p: ~p: ~n ~s:~n", [Type, Reason, io_lib:format(S2, [])])

         end
   end.

testCatch(Type) -> testCatch(Type, false).

testCatch(Type, TestLoop) ->
   case catch makeException(Type) of
      ignore -> ignore;
      Exception ->
         case TestLoop of
            true -> ok;
            false -> io:format("catch block caught exception of ~p~n", [Exception])
         end
   end.


makeException(Type) ->
   utStrace1:test(Type).

makeException2(Type) ->
   case Type of
      throw -> erlang:throw(test);
      error -> erlang:error(test);
      exit -> erlang:exit(test);
      _ -> ignore
   end.

comRet(_, _) ->
   ok.

throwRet(_, _) ->
   catch throw(ok).

tryThrowRet(_, _) ->
   try throw(ok) of
      ok ->
         ok
   catch ok ->
      ok
   end.

comRet1(_, _) ->
   A = {tuple, 11},
   F = A,
   case F of
      {tt, 44} ->
         ok;
      _ ->
         A
   end,
   ok.

throwRet1(_, _) ->
   A = {tuple, 11},
   F = A,
   case F of
      {tt, 44} ->
         ok;
      _ ->
         A
   end,
   catch throw(ok).

e1(Data) ->
   utStrace1:test(Data).
   %io:format("IMY*************e1").

e2(Data) ->
   e3(Data),
   io:format("IMY*************e2").

e3(Data) ->
   e4(Data),
   io:format("IMY*************e3").

e4(Data) ->
   e5(Data),
   io:format("IMY*************e4").

e5(Data) ->
   e6(Data),
   io:format("IMY*************e5").

e6(_Data) ->
   e1(_Data),
   io:format("IMY*************e6"),
   eBig(_Data),
   io:format("IMY*************big6"),
   {ok, _Data}.

eBig(Data) ->
   e1(Data).

tryBig1(Data) ->
   try eBig(Data)
   catch
      C:R ->
         {C, R}
   end.

tryBig2(Data) ->
   try eBig(Data)
   catch
      C:R:S ->
         {C, R, S}
   end.

tryBig3(Data) ->
   eBig(Data).

whileBig1(Data) ->
   tryBig1(Data),
   timer:sleep(20),
   whileBig1(Data).

whileBig2(Data) ->
   tryBig2(Data),
   timer:sleep(20),
   whileBig2(Data).

whileBig3(Data) ->
   tryBig3(Data),
   timer:sleep(20),
   whileBig3(Data).

whileBig(Fun) ->
   Data = utGenTerm:genString(1),
   io:format("IMY************************* ~p~n", [utTermSize:byteSize(Data)]),
   erlang:spawn(fun() -> ?MODULE:Fun(Data) end).

whileTrace(N) ->
   Data = utGenTerm:genString(N),
   io:format("IMY************************* ~p~n", [utTermSize:byteSize(Data)]),
   P1 = erlang:spawn(fun() -> whileBig1(Data) end),
   P2 = erlang:spawn(fun() -> whileBig2(Data) end),
   P3 = erlang:spawn(fun() -> whileBig3(Data) end),
   io:format("IMY************************* ~p ~p ~p ~p~n", [utTermSize:byteSize(Data), P1, P2, P3]).


