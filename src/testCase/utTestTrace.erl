-module(utTestTrace).

-compile([export_all, nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).


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

   try test:tt9(2) of
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
   Ret1 = try test:tt9(2) of
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
   try test:tt9(2) of
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
   try test:tt9(2) of
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
   try test:tt9(2) of
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




