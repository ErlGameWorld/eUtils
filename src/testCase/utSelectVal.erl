-module(utSelectVal).

-compile([export_all, nowarn_function, nowarn_unused_vars, nowarn_export_all]).

write(N) ->
   Head = <<"-module(testCfg", (integer_to_binary(N))/binary, ").\n-compile([export_all, nowarn_function, nowarn_unused_vars, nowarn_export_all]).\n">>,
   HandStr = makeStr(N, Head),
   ok = file:write_file("./genCfg/utTestCfg" ++ integer_to_list(N) ++ ".erl", HandStr).

makeStr(0, BinStr) ->
   <<BinStr/binary, "hand(_) -> undefined.">>;
makeStr(N, BinStr) ->
   PN = rand:uniform(10),
   VStr = << <<"V", (integer_to_binary(VN))/binary, ", ">> || VN <- lists:seq(1, PN)>>,
   Str = <<"hand({test", (integer_to_binary(N))/binary, ", ", VStr/binary, "V100}) ->\n\t", (integer_to_binary(N))/binary, ";\n">>,
   makeStr(N - 1, <<BinStr/binary, Str/binary>>).

write2(N) ->
   Head = <<"-module(testHand", (integer_to_binary(N))/binary, ").\n-compile([export_all, nowarn_function, nowarn_unused_vars, nowarn_export_all]).\n">>,
   HandStr = makeStr2(N, Head),
   ok = file:write_file("./genCfg/utTestHand" ++ integer_to_list(N) ++ ".erl", HandStr).

makeStr2(0, BinStr) ->
   <<BinStr/binary, "hand(_, _) -> undefined.">>;
makeStr2(N, BinStr) ->
   PN = rand:uniform(8),
   VStr = << <<"V", (integer_to_binary(VN))/binary, ", ">> || VN <- lists:seq(1, PN)>>,
   Str = <<"hand(test", (integer_to_binary(N))/binary, ", {test", (integer_to_binary(N))/binary, ", ", VStr/binary, "V100}) ->\n\t", (integer_to_binary(N))/binary, ";\n">>,
   makeStr2(N - 1, <<BinStr/binary, Str/binary>>).

writeGet(N) ->
   Head = <<"-module(utTestGet", (integer_to_binary(N))/binary, ").\n-compile([export_all, nowarn_function, nowarn_unused_vars, nowarn_export_all]).\n\n">>,
   HandStr = makeStrGet(1, N, Head),
   ok = file:write_file("./genCfg/utTestGet" ++ integer_to_list(N) ++ ".erl", HandStr).

makeStrGet(Index, N, BinStr) when Index > N ->
   <<BinStr/binary, "hand(_) -> undefined.">>;
makeStrGet(Index, N, BinStr) ->
   Str = <<"hand(", (integer_to_binary(Index))/binary, ") -> ", (integer_to_binary(Index))/binary, ";\n">>,
   makeStrGet(Index + 1, N, <<BinStr/binary, Str/binary>>).

writeWhen(N) ->
   Head = <<"-module(utTestWhen", (integer_to_binary(N))/binary, ").\n-compile([export_all, nowarn_function, nowarn_unused_vars, nowarn_export_all]).\n\n">>,
   HandStr = makeStrWhen(1, N, Head),
   ok = file:write_file("./genCfg/utTestWhen" ++ integer_to_list(N) ++ ".erl", HandStr).

makeStrWhen(Index, N, BinStr) when Index > N ->
   <<BinStr/binary, "hand(_) -> undefined.">>;
makeStrWhen(Index, N, BinStr) ->
   Str = <<"hand(X) when X < ", (integer_to_binary(Index))/binary, " -> ", (integer_to_binary(Index))/binary, ";\n">>,
   makeStrWhen(Index + 1, N, <<BinStr/binary, Str/binary>>).

call1(N, Y) ->
   KVList = [{{value, test, Index, Index}, {value, test, Index, Index + 2}} || Index <- lists:seq(1, Y)],
   utKvsToBeam:load(ttttt, KVList),
   io:format("IMY********load ok~n"),
   T = erlang:system_time(microsecond),
   A = ?MODULE,
   Fun = get,
   call1(N, Y, T, A, Fun, 0).

call1(0, A1, A2, _A, _Fun, _V) ->
   Key = rand:uniform(A1),
   V = ttttt:get({value, test, Key, Key}),
   {erlang:system_time(microsecond) - A2, V};
call1(N, A1, A2, A, Fun, _V) ->
   Key = rand:uniform(A1),
   V = ttttt:get({value, test, Key, Key}),
   call1(N - 1, A1, A2, A, Fun, V).

get1({value, test, 1500, 1500}) ->
   {value, test, 1500, 1500}.

call2(N, Y) ->
   A = utFunCallCase,
   Fun = get1,
   T = erlang:system_time(microsecond),
   call2(N, Y, T, A, Fun, 0).

call2(0, _A1, A2, A, Fun, _V) ->
   V = ?MODULE:Fun({value, test, 1500, 1500}),
   {erlang:system_time(microsecond) - A2, V};
call2(N, A1, A2, A, Fun, _Key) ->
   Key = rand:uniform(A1),
   _V = ?MODULE:Fun({value, test, 1500, 1500}),
   call2(N - 1, A1, A2, A, Fun, Key).

call3(N, Y) ->
   A = ?MODULE,
   Fun = get1,
   T = erlang:system_time(microsecond),
   call3(N, Y, T, A, Fun, 0).

call3(0, _A1, A2, A, Fun, _K) ->
   _V = erlang:apply(A, Fun, [{value, test, 1500, 1500}]),
   {erlang:system_time(microsecond) - A2, _K};
call3(N, A1, A2, A, Fun, _key) ->
   Key = rand:uniform(A1),
   _V = erlang:apply(A, Fun, [{value, test, 1500, 1500}]),
   call3(N - 1, A1, A2, A, Fun, Key).

call4(N, Y) ->
   A = ?MODULE,
   Fun = get1,
   T = erlang:system_time(microsecond),
   call4(N, Y, T, A, Fun, 0).

call4(0, _A1, A2, A, Fun, _K) ->
   erlang:apply(A, Fun, [{value, test, 1500, 1500}]),
   {erlang:system_time(microsecond) - A2, _K};
call4(N, A1, A2, A, Fun, _key) ->
   Key = rand:uniform(A1),
   Args = [{value, test, 1500, 1500}],
   erlang:apply(A, Fun, Args),
   call4(N - 1, A1, A2, A, Fun, Key).

-define(AcList, [{eTimeout, 11, 22}, {u_eTimeout, 22}, {sTimeout, 111, 222, 333}, c_eTimeout, c_sTimeout, {nextEvent, 222, fdfd}, {doAfter, args}]).

ht11(0, _Fun) ->
   ok;
ht11(N, Fun) ->
   ?MODULE:Fun(?AcList, 1, ttt, [], true, fasle, false, [], []),
   ht11(N - 1, Fun).

%% 下面两中写法 并无差别
doPAL([], _CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, DoAfter, Timeouts, NextEvents) ->
   {CycleData, Debug, IsPostpone, IsHibernate, DoAfter, Timeouts, NextEvents};
doPAL([OneAction | LeftActions], CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, DoAfter, Timeouts, NextEvents) ->
   case OneAction of
      {reply, From, Reply} ->
         doPAL(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, DoAfter, Timeouts, NextEvents);
      {eTimeout, _Time, _TimeoutMsg} ->
         doPAL(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, DoAfter, [OneAction | Timeouts], NextEvents);
      {sTimeout, _Time, _TimeoutMsg} ->
         doPAL(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, DoAfter, [OneAction | Timeouts], NextEvents);
      {{gTimeout, _Name}, _Time, _TimeoutMsg} ->
         doPAL(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, DoAfter, [OneAction | Timeouts], NextEvents);
      {eTimeout, _Time, _TimeoutMsg, _Options} ->
         doPAL(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, DoAfter, [OneAction | Timeouts], NextEvents);
      {sTimeout, _Time, _TimeoutMsg, _Options} ->
         doPAL(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, DoAfter, [OneAction | Timeouts], NextEvents);
      {{gTimeout, _Name}, _Time, _TimeoutMsg, _Options} ->
         doPAL(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, DoAfter, [OneAction | Timeouts], NextEvents);
      {u_eTimeout, _TimeoutMsg} ->
         doPAL(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, DoAfter, [OneAction | Timeouts], NextEvents);
      {u_sTimeout, _TimeoutMsg} ->
         doPAL(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, DoAfter, [OneAction | Timeouts], NextEvents);
      {{u_gTimeout, _Name}, _TimeoutMsg} ->
         doPAL(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, DoAfter, [OneAction | Timeouts], NextEvents);
      %case OneAction of
      c_eTimeout ->
         doPAL(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, DoAfter, [OneAction | Timeouts], NextEvents);
      c_sTimeout ->
         doPAL(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, DoAfter, [OneAction | Timeouts], NextEvents);
      {c_gTimeout, _Name} ->
         doPAL(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, DoAfter, [OneAction | Timeouts], NextEvents);
      {isHibernate, NewIsHibernate} ->
         doPAL(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, NewIsHibernate, DoAfter, Timeouts, NextEvents);
      {isPostpone, NewIsPostpone} ->
         doPAL(LeftActions, CallbackForm, CycleData, Debug, NewIsPostpone, IsHibernate, DoAfter, Timeouts, NextEvents);
      {doAfter, Args} ->
         doPAL(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, {true, Args}, Timeouts, NextEvents);
      {nextEvent, Type, Content} ->
         %% 处理next_event动作
         doPAL(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, DoAfter, Timeouts, [{Type, Content} | NextEvents])
      %_ ->
      %case OneAction of
      %   c_eTimeout when CallbackForm == 1 ->
      %      doPAL(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, DoAfter, [OneAction | Timeouts], NextEvents);
      %   c_sTimeout when CallbackForm == 1 ->
      %      doPAL(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, DoAfter, [OneAction | Timeouts], NextEvents);
      %   {c_gTimeout, _Name} when CallbackForm == 1 ->
      %      doPAL(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, DoAfter, [OneAction | Timeouts], NextEvents);
      %   {isHibernate, NewIsHibernate} ->
      %      doPAL(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, NewIsHibernate, DoAfter, Timeouts, NextEvents);
      %   {isPostpone, NewIsPostpone} when (not NewIsPostpone orelse CallbackForm == 1) ->
      %      doPAL(LeftActions, CallbackForm, CycleData, Debug, NewIsPostpone, IsHibernate, DoAfter, Timeouts, NextEvents);
      %   {doAfter, Args} when CallbackForm == 1 ->
      %      doPAL(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, {true, Args}, Timeouts, NextEvents);
      %   {nextEvent, Type, Content} when CallbackForm == 1 orelse CallbackForm == 2 ->
      %      %% 处理next_event动作
      %      doPAL(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, DoAfter, Timeouts, [{Type, Content} | NextEvents]);
      %   _ActRet ->
      %      doPAL(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, DoAfter, Timeouts, NextEvents)
      %end
   end.

doPAL2([], _CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, DoAfter, Timeouts, NextEvents) ->
   {CycleData, Debug, IsPostpone, IsHibernate, DoAfter, Timeouts, NextEvents};
doPAL2([OneAction | LeftActions], CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, DoAfter, Timeouts, NextEvents) ->
   case OneAction of
      {reply, From, Reply} ->
         ok1;
      {eTimeout, _Time, _TimeoutMsg} when CallbackForm == 1 ->
         ok2;
      {sTimeout, _Time, _TimeoutMsg} ->
         ok3;
      {{gTimeout, _Name}, _Time, _TimeoutMsg} ->
         ok4;
      {c_gTimeout, _Name} when CallbackForm == 1 ->
         ok5;
      {eTimeout, _Time, _TimeoutMsg, _Options} ->
         ok6;
      {sTimeout, _Time, _TimeoutMsg, _Options} ->
         ok7;
      {{gTimeout, _Name}, _Time, _TimeoutMsg, _Options} ->
         ok8;
      {u_eTimeout, _TimeoutMsg} when CallbackForm == 1 ->
         ok9;
      {u_sTimeout, _TimeoutMsg} ->
         ok10;
      {{u_gTimeout, _Name}, _TimeoutMsg} ->
         ok;
      c_eTimeout when CallbackForm == 1 ->
         ok;
      c_sTimeout when CallbackForm == 1 ->
         ok;
      {c_gTimeout, _Name} when CallbackForm == 1 ->
         ok;
      {isHibernate, NewIsHibernate} ->
         ok;
      {isPostpone, NewIsPostpone} when (not NewIsPostpone orelse CallbackForm == 1) ->
         ok;
      {doAfter, Args} when CallbackForm == 1 ->
         ok;
      {nextEvent, Type, Content} when CallbackForm == 1 orelse CallbackForm == 2 ->
         ok;
      %% 处理next_event动作
      _ActRet ->
         ok
   end.

-define(htList, [
   {test1, kkk1, 22},
   {test2, kkk},
   {test3, kkk, 66, yy},
   {test4, kkk, tt, tt},
   {test5, kkk, [11, 55, yy]},
   {test6, kkk, 1, 2, 3},
   {test7, gfg}
]).


hht1(0, _Fun) ->
   ok;
hht1(N, Fun) ->
   hand({test4, kkk, 1, 2}),
   hht1(N - 1, Fun).

hht2(0, _Fun) ->
   ok;
hht2(N, Fun) ->
   hand1(test4, {test4, kkk, 1, 2}),
   hht2(N - 1, Fun).

hand({test1, kkk1, _}) ->
   vv1;
hand({test2, kkk}) ->
   vv2;
hand({test3, kkk, 66, yy}) ->
   vv3;
hand({test4, kkk, _, _}) ->
   vv4;
hand({test5, kkk, [11, 55, yy]}) ->
   vv5;
hand({test6, kkk, _, _, _}) ->
   vv6;
hand({test7, _}) ->
   vv7;
hand(_) ->
   vv8.

hand1(test1, {test1, kkk1, _}) ->
   vv1;
hand1(test2, {test2, kkk}) ->
   vv2;
hand1(test3, {test3, kkk, 66, yy}) ->
   vv3;
hand1(test4, {test4, kkk, _, _}) ->
   vv4;
hand1(test5, {test5, kkk, [11, 55, yy]}) ->
   vv5;
hand1(test6, {test6, kkk, _, _, _}) ->
   vv6;
hand1(test7, {test7, _}) ->
   vv7;
hand1(_, _) ->
   vv8.

hand2({test1, kkk, 34}) ->
   vv1;
hand2({test2, kkk}) ->
   vv2;
hand2({test3, kkk, 66, yy}) ->
   vv3;
hand2({test4, kkk, {11, 22}}) ->
   vv4;
hand2({test5, kkk, [11, 55, yy]}) ->
   vv5;
hand2({test6, kkk, _, _, _}) ->
   vv6;
hand2({test7, kkk, _}) ->
   vv7;
hand2(_) ->
   vv8.

hand3(test1) ->
   vv1;
hand3(test2) ->
   vv2;
hand3(test3) ->
   vv3;
hand3(test4) ->
   vv4;
hand3(test5) ->
   vv5;
hand3(test6) ->
   vv6;
hand3(test7) ->
   vv7;
hand3(_) ->
   vv8.


