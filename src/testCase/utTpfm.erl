-module(utTpfm).


-compile([export_all, nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).

-record(cycleData, {
   module = test
   , isEnter = false
   , hibernateAfter = infinity
   , isHibernate = false
   , lastStatus = init_status
   , lastState = #{11 => 555, 44 => 434}
   , postponed = [1, 3, "dffd", "fdf"]
   , timers = #{etime => {aaa, fdfd}}
}).

tt1(N) ->
   tt1(N, #cycleData{}, [123421, 434, 34], {12222, 343, "fdfd"}, fff, "ArgD").

tt1(0, CycleData, ArgA, ArgB, ArgC, ArgD) ->
   ok;
tt1(N, #cycleData{lastState = LastState, isEnter = IsEnter, hibernateAfter = HibernateAfter, module = Module} = CycleData, ArgA, ArgB, ArgC, ArgD) ->
   I1 = is_atom(LastState),
   I2 = is_atom(IsEnter),
   I3 = is_atom(HibernateAfter),
   I4 = is_atom(Module),
   _Ret = I1 andalso I2 andalso I3 andalso I4 andalso true,
   tt1(N - 1, CycleData, ArgA, ArgB, ArgC, ArgD).

tt2(N) ->
   tt2(N, #cycleData{}, [123421, 434, 34], {12222, 343, "fdfd"}, fff, "ArgD").

tt2(0, CycleData, ArgA, ArgB, ArgC, ArgD) ->
   ok;
tt2(N, CycleData, ArgA, ArgB, ArgC, ArgD) ->
   LastState = element(#cycleData.lastState, CycleData),
   IsEnter = element(#cycleData.isEnter, CycleData),
   HibernateAfter = element(#cycleData.hibernateAfter, CycleData),
   Module = element(#cycleData.module, CycleData),
   I1 = is_atom(LastState),
   I2 = is_atom(IsEnter),
   I3 = is_atom(HibernateAfter),
   I4 = is_atom(Module),
   _Ret = I1 andalso I2 andalso I3 andalso I4 andalso true,
   tt2(N - 1, CycleData, ArgA, ArgB, ArgC, ArgD).

tt3(N) ->
   tt3(N, #cycleData{}, [123421, 434, 34], {12222, 343, "fdfd"}, fff, "ArgD").

tt3(0, CycleData, ArgA, ArgB, ArgC, ArgD) ->
   ok;
tt3(N, CycleData, ArgA, ArgB, ArgC, ArgD) ->
   NewCycleData = CycleData#cycleData{module = tttt, lastState = #{11 => 22, 22 => 33}, isEnter = false},
   tt3(N - 1, NewCycleData, ArgA, ArgB, ArgC, ArgD).

tt4(N) ->
   tt4(N, #cycleData{}, [123421, 434, 34], {12222, 343, "fdfd"}, fff, "ArgD").

tt4(0, CycleData, ArgA, ArgB, ArgC, ArgD) ->
   ok;
tt4(N, CycleData, ArgA, ArgB, ArgC, ArgD) ->
   New1 = setelement(#cycleData.module, CycleData, tttt),
   New2 = setelement(#cycleData.lastState, New1, #{11 => 22, 22 => 33}),
   New3 = setelement(#cycleData.isEnter, New2, false),
   tt4(N - 1, New3, ArgA, ArgB, ArgC, ArgD).

tt5(N) ->
   tt5(N, #cycleData{}, [123421, 434, 34]).

tt5(0, CycleData, AA) ->
   ok;
tt5(N, CycleData, AA) ->
   tt5(N - 1, CycleData, AA).

tt6(N) ->
   tt6(N, test, false, infinity, false, init_status, #{11 => 555, 44 => 434}, [1, 3, "dffd", "fdf"], #{etime => {aaa, fdfd}}, [123421, 434, 34]).

tt6(0, A1, B, C, D, E, F, G, H, AA) ->
   ok;
tt6(N, A1, B, C, D, E, F, G, H, AA) ->
   tt6(N - 1, A1, B, C, D, E, F, G, H, AA).

tt7(0) ->
   ok;
tt7(N) ->
   tt7(N - 1).


tt8(N) ->
   tt8(N, #cycleData{}).

tt8(0, CycleData) ->
   ok;
tt8(N, #cycleData{module = Module, lastState = Lasst, postponed = Postponed} = AA) ->
   A = setelement(#cycleData.module, AA, ttt),
   B = setelement(#cycleData.isEnter, A, trye),
   %% B = setelement(#cycleData.lastState, A, #{22 => 555, 55 => 434}),
   %% C = setelement(#cycleData.postponed, B, [1,3,"fdf", "dffd"]),
   tt8(N - 1, B).

tt88(N) ->
   tt88(N, #cycleData{}).

tt88(0, CycleData) ->
   ok;
tt88(N, #cycleData{module = Module, lastState = Lasst, postponed = Postponed} = AA) ->
   %%C = setelement(#cycleData.postponed, AA, [1,3,"fdf", "dffd"]),
   %%B = setelement(#cycleData.lastState, C, #{22 => 555, 55 => 434}),
   B = setelement(#cycleData.isEnter, AA, trye),
   A = setelement(#cycleData.module, B, ttt),
   tt88(N - 1, A).

tt888(N) ->
   tt888(N, #cycleData{}).

tt888(0, CycleData) ->
   ok;
tt888(N, #cycleData{module = Module, lastState = Lasst, postponed = Postponed} = AA) ->
   A = AA#cycleData{isEnter = trye, module = ttt},
   tt888(N - 1, A).

tt9(N) ->
   Data = #cycleData{},
   put(a, Data#cycleData.module),
   put(b, Data#cycleData.isEnter),
   put(c, Data#cycleData.hibernateAfter),
   put(d, Data#cycleData.isHibernate),
   put(e, Data#cycleData.lastStatus),
   put(f, Data#cycleData.lastState),
   put(g, Data#cycleData.postponed),
   tt10(N).

tt10(0) ->
   ok;
tt10(N) ->
   A = get(a),
   put(a, ttt),
   B = get(f),
   put(f, #{22 => 555, 55 => 434}),
   C = get(g),
   put(g, [1, 3, "fdf", "dffd"]),
   _Ret = A orelse B orelse C orelse true,
   tt10(N - 1).

c6(N) ->
   cc6(N, 0).

cc6(0, F) ->
   F;
cc6(N, _F) ->
   F = N + 0.0,
   cc6(N - 1, F).

c7(N) ->
   cc7(N, 0).

cc7(0, F) ->
   F;
cc7(N, _F) ->
   F = float(N),
   cc7(N - 1, F).

s1(0, Fun) ->
   ok;
s1(N, Fun) ->
   ?MODULE:Fun(),
   s1(N - 1, Fun).

st1() ->
   size(<<"fdfdfdd:fdffd:\rn\n:fdfd fd df df dfddfdf">>).

st2() ->
   byte_size(<<"fdfdfdd:fdffd:\rn\n:fdfd fd df df dfddfdf">>).

st3() ->
   iolist_size(<<"fdfdfdd:fdffd:\rn\n:fdfd fd df df d:fddfdf">>).

st4() ->
   size(<<"fdfdfdd:fdffd:\rn\n:fdfd fd df df dfddfdf">>).

gm(0, Fun) ->
   ok;
gm(N, Fun) ->
   [?MODULE:Fun(M) || M <- [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]],
   gm(N - 1, Fun).

%% 这个更快
getMonth(1) ->
   <<"Jan">>;
getMonth(2) ->
   <<"Feb">>;
getMonth(3) ->
   <<"Mar">>;
getMonth(4) ->
   <<"Apr">>;
getMonth(5) ->
   <<"May">>;
getMonth(6) ->
   <<"Jun">>;
getMonth(7) ->
   <<"Jul">>;
getMonth(8) ->
   <<"Aug">>;
getMonth(9) ->
   <<"Sep">>;
getMonth(10) ->
   <<"Oct">>;
getMonth(11) ->
   <<"Nov">>;
getMonth(12) ->
   <<"Dec">>.

getMonth2(Month) ->
   element(Month, {<<"Jan">>, <<"Feb">>, <<"Mar">>, <<"Apr">>, <<"May">>, <<"Jun">>, <<"Jul">>, <<"Aug">>, <<"Sep">>, <<"Oct">>, <<"Nov">>, <<"Dec">>}).

-define(Month, #{1 => <<"Jan">>, 2 => <<"Feb">>, 3 => <<"Mar">>, 4 => <<"Apr">>, 5 => <<"May">>, 6 => <<"Jun">>, 7 => <<"Jul">>, 8 => <<"Aug">>, 9 => <<"Sep">>, 10 => <<"Oct">>, 11 => <<"Nov">>, 12 => <<"Dec">>}).
getMonth3(Month) ->
   case ?Month of
      #{Month := MonthStr} ->
         MonthStr;
      _ ->
         <<"">>
   end.

-define(List, [1, 2, 3234235, <<"fdsfasf">>, <<"fdsfasf111111111111111111111111">>, [3434, 43, 434], tryrer, {rqwrer, 342144}, #{23424 => "fdsfsdafsaf"}, {432143, "fdsaf", 76767}]).

ht(0, _Fun) ->
   ok;
ht(N, Fun) ->
   [?MODULE:Fun(Term) || Term <- ?List],
   ht(N - 1, Fun).

hash2(Term) ->
   erlang:phash2(Term, 256).

hashn1(Term) ->
   nifHashb:hash1(Term, 256).

hashn2(Term) ->
   nifHashb:hash2(Term, 256).

hashn3(Term) ->
   nifHashb:hash3(Term, 256).

ht1(0, _Fun, Term) ->
   ok;
ht1(N, Fun, Term) ->
   ?MODULE:Fun(Term),
   ht1(N - 1, Fun, Term).

hash4(Term) ->
   erlang:phash2(Term, 256).

ttT(0, Fun) ->
   ?MODULE:Fun(0);
ttT(N, Fun) ->
   ?MODULE:Fun(N),
   ttT(N - 1, Fun).

nifT(N) ->
   [nifArray:test(Term) || Term <- ?List].

nifT1(N) ->
   [nifArray:test1(Term) || Term <- ?List].

cb(N, Len, Fun) ->
   Bin = utGenTerm:genBinary(Len),
   cddo(N, Bin, Fun).

cddo(0, Bin, Fun) ->
   nifHashb:Fun(Bin, Bin);
cddo(N, Bin, Fun) ->
   nifHashb:Fun(Bin, Bin),
   cddo(N - 1, Bin, Fun).

cc1(0, A1, A2, _V) ->
   A1 / A2;
cc1(N, A1, A2, _V) ->
   V = A1 / A2,
   cc1(N - 1, A1, A2, V).

cc2(0, A1, A2, _V) ->
   A1 div A2;
cc2(N, A1, A2, _V) ->
   V = A1 div A2,
   cc2(N - 1, A1, A2, V).

call1(N, Y) ->
   KVList = [{{value, test, Index, Index+1}, {value, test, Index, Index+2}} || Index <- lists:seq(1, Y)],
   utKvsToBeam:load(ttttt, KVList),
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
         doPAL2(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, DoAfter, Timeouts, NextEvents);
      {eTimeout, _Time, _TimeoutMsg} when CallbackForm == 1 ->
         doPAL2(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, DoAfter, [OneAction | Timeouts], NextEvents);
      {sTimeout, _Time, _TimeoutMsg} ->
         doPAL2(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, DoAfter, [OneAction | Timeouts], NextEvents);
      {{gTimeout, _Name}, _Time, _TimeoutMsg} ->
         doPAL2(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, DoAfter, [OneAction | Timeouts], NextEvents);
      {c_gTimeout, _Name} when CallbackForm == 1 ->
         doPAL2(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, DoAfter, [OneAction | Timeouts], NextEvents);
      {eTimeout, _Time, _TimeoutMsg, _Options} ->
         doPAL2(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, DoAfter, [OneAction | Timeouts], NextEvents);
      {sTimeout, _Time, _TimeoutMsg, _Options} ->
         doPAL2(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, DoAfter, [OneAction | Timeouts], NextEvents);
      {{gTimeout, _Name}, _Time, _TimeoutMsg, _Options} ->
         doPAL2(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, DoAfter, [OneAction | Timeouts], NextEvents);
      {u_eTimeout, _TimeoutMsg} when CallbackForm == 1 ->
         doPAL2(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, DoAfter, [OneAction | Timeouts], NextEvents);
      {u_sTimeout, _TimeoutMsg} ->
         doPAL2(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, DoAfter, [OneAction | Timeouts], NextEvents);
      {{u_gTimeout, _Name}, _TimeoutMsg} ->
         doPAL2(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, DoAfter, [OneAction | Timeouts], NextEvents);
      c_eTimeout when CallbackForm == 1 ->
         doPAL2(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, DoAfter, [OneAction | Timeouts], NextEvents);
      c_sTimeout when CallbackForm == 1 ->
         doPAL2(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, DoAfter, [OneAction | Timeouts], NextEvents);
      {c_gTimeout, _Name} when CallbackForm == 1 ->
         doPAL2(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, DoAfter, [OneAction | Timeouts], NextEvents);
      {isHibernate, NewIsHibernate} ->
         doPAL2(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, NewIsHibernate, DoAfter, Timeouts, NextEvents);
      {isPostpone, NewIsPostpone} when (not NewIsPostpone orelse CallbackForm == 1) ->
         doPAL2(LeftActions, CallbackForm, CycleData, Debug, NewIsPostpone, IsHibernate, DoAfter, Timeouts, NextEvents);
      {doAfter, Args} when CallbackForm == 1 ->
         doPAL2(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, {true, Args}, Timeouts, NextEvents);
      {nextEvent, Type, Content} when CallbackForm == 1 orelse CallbackForm == 2 ->
         %% 处理next_event动作
         doPAL2(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, DoAfter, Timeouts, [{Type, Content} | NextEvents]);
      _ActRet ->
         doPAL2(LeftActions, CallbackForm, CycleData, Debug, IsPostpone, IsHibernate, DoAfter, Timeouts, NextEvents)
   end.

lr(0) ->
   ok;
lr(N) ->
   lists:reverse([]),
   lr(N - 1).

tsList(0, _, SceneId, LineId, ExtraArgs) ->
   ok;
tsList(N, _, SceneId, LineId, ExtraArgs) ->
   tsList(N - 1, [SceneId, LineId, ExtraArgs], SceneId, LineId, ExtraArgs).

tsTuple(0, _, SceneId, LineId, ExtraArgs) ->
   ok;
tsTuple(N, _, SceneId, LineId, ExtraArgs) ->
   tsList(N - 1, {SceneId, LineId, ExtraArgs}, SceneId, LineId, ExtraArgs).

xxxT(0, A, B, _) ->
   A * 1000000 + B;
xxxT(N,  A, B, _) ->
   xxxT(N -1, A, B, A * 1000000 + B).

bslT(0, A, B, _) ->
   A bsl 20 + B;
bslT(N, A, B, _) ->
   bslT(N -1, A, B, A bsl 20 + B).

tttT(0, A, B, _) ->
   {A, B};
tttT(N, A, B, _) ->
   tttT(N -1, A, B, {A, B}).

listLoopIn(X) ->
   X.

listLoop1(List) ->
   [listLoopIn(One) || One <- List],
   ok.

listLoop2([]) ->
   ok;
listLoop2([One | List]) ->
   listLoopIn(One),
   listLoop2(List).

listLoop3(List) ->
   lists:foreach(fun(X) -> listLoopIn(X) end, List).

bits(0, _Fun) ->
   ok;
bits(N, Fun) ->
   [?MODULE:Fun(BitSize) || BitSize <- [3, 7, 16, 31, 55, 63, 64]],
   bits(N - 1, Fun).

%% 生成一个指定大小且全为1的bitstring
bits_1(BitSize) when BitSize <    8 -> <<Bits:BitSize/bits, _/bits>> = <<16#FF>>, Bits;
bits_1(BitSize) when BitSize =:=  8 -> <<255>>;
bits_1(BitSize) when BitSize <   16 -> Tail = bits_1(BitSize - 8), <<255, Tail/bits>>;
bits_1(BitSize) when BitSize =:= 16 -> <<255, 255>>;
bits_1(BitSize) when BitSize <   24 -> Tail = bits_1(BitSize - 16), <<255, 255, Tail/bits>>;
bits_1(BitSize) when BitSize =:= 24 -> <<255, 255, 255>>;
bits_1(BitSize) when BitSize <   32 -> Tail = bits_1(BitSize - 24), <<255, 255, 255, Tail/bits>>;
bits_1(BitSize) when BitSize =:= 32 -> <<255, 255, 255, 255>>;
bits_1(BitSize) when BitSize <   40 -> Tail = bits_1(BitSize - 32), <<255, 255, 255, 255, Tail/bits>>;
bits_1(BitSize) when BitSize =:= 40 -> <<255, 255, 255, 255, 255>>;
bits_1(BitSize) when BitSize <   48 -> Tail = bits_1(BitSize - 40), <<255, 255, 255, 255, 255, Tail/bits>>;
bits_1(BitSize) when BitSize =:= 48 -> <<255, 255, 255, 255, 255, 255>>;
bits_1(BitSize) when BitSize <   56 -> Tail = bits_1(BitSize - 48), <<255, 255, 255, 255, 255, 255, Tail/bits>>;
bits_1(BitSize) when BitSize =:= 56 -> <<255, 255, 255, 255, 255, 255, 255>>;
bits_1(BitSize) when BitSize <   64 -> Tail = bits_1(BitSize - 56), <<255, 255, 255, 255, 255, 255, 255, Tail/bits>>;
bits_1(BitSize) when BitSize =:= 64 -> <<255, 255, 255, 255, 255, 255, 255, 255>>.

bits_2(BitSize) ->
   <<((1 bsl BitSize) - 1):BitSize>>.

testHex(0, Mod, Fun, Args, _) ->
   Mod:Fun(Args);
testHex(N, Mod, Fun, Args, _) ->
   testHex(N - 1, Mod, Fun, Args, Mod:Fun(Args)).

tTuple(0, _) ->
   ok;
tTuple(N, {tag, OldN}) ->
   tTuple(N -1, {tag, OldN}).

tPd(0, _) ->
   ok;
tPd(N, _) ->
   erlang:put(pdVpSize, 0),
   tPd(N -1, erlang:get(pdVpSize)).

load1(0, _List) ->
   ok;
load1(N, List) ->
   utKvsToBeam:load(load1, List),
   load1(N - 1, List).

load2(0, _List) ->
   ok;
load2(N, List) ->
   Str = << <<"get(", (utTypeCast:toBinary(Key))/binary, ") -> ", (utTypeCast:toBinary(Value))/binary, ";">> || {Key, Value} <- List>>,
   LastStr = <<Str/binary, "get(_) -> undefine.">>,
   utStrToBeam:load(load2, [{get, 1}], binary_to_list(LastStr)),
   load2(N - 1, List).

etsT(Cnt, Num) ->
   Tab = ets:new(test, [ordered_set]),
   etsInsert(Num, Tab),
   etsT(Cnt, {50000, Num div 2}, Tab),
   ets:delete(Tab).

-record(etsT, {id, v1, v2}).

etsInsert(0, Tab) ->
   ets:insert(Tab, {etsT, 50000, 500, 5001}),
   ok;
etsInsert(N, Tab) ->
   ets:insert(Tab, {etsT, N, N, N + 1}),
   etsInsert(N - 1, Tab).

-include_lib("stdlib/include/ms_transform.hrl").
etsT(Cnt, Key, Tab) ->
   Ms = ets:fun2ms(fun({K, _V}) when Key > K -> true end),
   utTc:ts(Cnt, ets, select_count, [Tab, Ms]).

etsM(Cnt, Num, Key) ->
   case ets:info(test, size) of
      undefined ->
         ets:new(test, [named_table, {keypos, #etsT.id}, set]),
         etsInsert(Num, test);
      _ ->
         ignore
   end,
   etsM(Cnt, Key).

etsM(Cnt, Key) ->
   utTc:ts(Cnt, ets, match, [test, #etsT{id = Key, v1 = '$1', v2 = '$2'}]),
   ets:match(test, #etsT{id = Key, v1 = '$1', v2 = '$2'}).


lMakeList(0, Acc) ->
   Acc;
lMakeList(Num, Acc) ->
   NewAcc = [{rand:uniform(1000000), rand:uniform(1000000), rand:uniform(1000000)} | Acc],
   lMakeList(Num - 1, NewAcc).

lSort(Cnt, Num) ->
   SList = lMakeList(Num, []),
   utTc:ts(Cnt, lists, sort, [SList]).

bp(0, _Str) ->
   ok;
bp(Cnt, Str) ->
   binary:split(Str, <<"~">>),
   bp(Cnt - 1, Str).


bp1(Cnt, Str) ->
   Pt = binary:compile_pattern(<<"~">>),
   persistent_term:put(aaaaa, Pt),
   bp11(Cnt, Str).

bp11(0, _Str) ->
   ok;
bp11(Cnt, Str) ->
   binary:split(Str, persistent_term:get(aaaaa)),
   bp11(Cnt - 1, Str).

sp(Bool) ->
   spawn_link(fun() -> erlang:process_flag(trap_exit, Bool), loop() end).

sp2(Bool) ->
   P = spawn_link(fun() -> erlang:process_flag(trap_exit, Bool), loop() end),
   link(P),
   P.

a(P) ->
   erlang:is_process_alive(P).

loop() ->
   receive
      Msg ->
         io:format("receive ~p ~p~n", [self(), Msg]),
         loop()
   end.


