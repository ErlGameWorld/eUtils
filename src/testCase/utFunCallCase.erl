-module(utFunCallCase).

-compile([export_all, nowarn_function, nowarn_unused_vars, nowarn_export_all]).

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

-record(handler, {
   module = test
   , id = false
   , state = {fd, 34, #{1 => 1343}, 32}
   , supId = [12, 34, 45]
}).

tt1(N) ->
   tt1(N, #cycleData{}, [123421, 434, 34], {12222, 343, "fdfd"}, fff, "ArgD").

tt1(0, CycleData, ArgA, ArgB, ArgC, ArgD) ->
   ok;
tt1(N, #cycleData{lastState = LastState, isEnter = IsEnter, hibernateAfter = HibernateAfter, module = Module} = CycleData, ArgA, ArgB, ArgC, ArgD) ->
   _ = is_atom(LastState),
   _ = is_atom(IsEnter),
   _ = is_atom(HibernateAfter),
   _ = is_atom(Module),
   tt1(N - 1, CycleData, ArgA, ArgB, ArgC, ArgD).

tt2(N) ->
   tt2(N, #cycleData{}, [123421, 434, 34], {12222, 343, "fdfd"}, fff, "ArgD").

tt2(0, _CycleData, _ArgA, _ArgB, _ArgC, _ArgD) ->
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
   NewCycleData = erlang:setelement(#cycleData.isHibernate, CycleData, true),
   tt5(N - 1, NewCycleData, AA).

tt6(N) ->
   tt6(N, test, false, infinity, false, init_status, #{11 => 555, 44 => 434}, [1, 3, "dffd", "fdf"], #{etime => {aaa, fdfd}}, [123421, 434, 34], test, false, infinity, false, init_status, #{11 => 555, 44 => 434}, [1, 3, "dffd", "fdf"], #{etime => {aaa, fdfd}}, [123421, 434, 34]).

tt6(0, A1, B, C, D, E, F, G, H, AA, A11, B1, C1, D1, E1, F1, G1, H1, AA1) ->
   ok;
tt6(N, A1, B, C, D, E, F, G, H, AA, A11, B1, C1, D1, E1, F1, G1, H1, AA1) ->
   tt6(N - 1, H1,  true, D, E, F, G,  A11, B1, C1, H, AA, D1, E1, F1, G1,A1, B, AA1).

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
   A = AA#cycleData{postponed = [1, 3, "fdf", "dffd"], module = ttt, lastState = #{22 => 555, 55 => 434}},
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
   _Ret = A orelse B orelse C orelse false,
   tt10(N - 1).

tt11(N) ->
   tt11(N, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 0).

tt11(0, A, B, C, D, E, F, G, H, I, J, BB) ->
   A;
tt11(N, A, B, C, D, E, F, G, H, I, J, BB1) ->
   BB = A + B + C + D + E + F + G + H + I + J,
   tt11(N - 1, A, B, C, D, E, F, G, H, I, J, BB).

tt12(N) ->
   tt12(N, 10, 0).

tt12(0, _A, AA) ->
   _A;
tt12(N, A, A1) ->
   AA = A + A + A + A + A + A + A + A + A + A,
   tt12(N - 1, A, AA).

tt13(N) ->
   put({pd_epm, {test, 1}}, #handler{}),
   put({pd_epm, {test, 2}}, #handler{}),
   put({pd_epm, {test, 3}}, #handler{}),
   put({pd_epm, {test, 4}}, #handler{}),
   put({pd_epm, {test, 5}}, #handler{}),
   put({pd_epm, {test, 6}}, #handler{}),
   put({pd_epm, {test, 7}}, #handler{}),
   put({pd_epm, {test, 8}}, #handler{}),
   put({pd_epm, {test, 9}}, #handler{}),
   put({pd_epm, {test, 10}}, #handler{}),
   put(pd_list, [{test, 1}, {test, 2}, {test, 3}, {test, 4}, {test, 5}, {test, 6}, {test, 7}, {test, 8}, {test, 9}, {test, 10}]),
   tt133(N, 0),
   tt134(N).

tt133(0, Ret) ->
   Ret;
tt133(N, _Ret) ->
   Ret = get(pd_list),
   tt133(N - 1, Ret).

tt134(0) ->
   ok;
tt134(N) ->
   A = get({pd_epm, {test, 5}}),
   put({pd_epm, {test, 5}}, A#handler{state = [332, 34, 3, 4, 3, 53, 6]}),
   tt134(N - 1).

%,#handler{id = {test, 6}},#handler{id = {test, 7}},#handler{id = {test, 8}},#handler{id = {test, 9}},#handler{id = {test, 10}}

tt14(N) ->
   List = [#handler{id = {test, 1}}, #handler{id = {test, 2}}, #handler{id = {test, 3}}, #handler{id = {test, 4}}, #handler{id = {test, 5}}],
   tt144(N, List),
   tt145(N, List).


tt144(0, List) ->
   ok;
tt144(N, List) ->
   List,
   tt144(N - 1, List).

search({Mod, Id}, [Ha | _MSL]) when Ha#handler.module =:= Mod,
   Ha#handler.id =:= Id ->
   {ok, Ha};
search(Mod, [Ha | _MSL]) when Ha#handler.module =:= Mod,
   not Ha#handler.id ->
   {ok, Ha};
search(Handler, [_ | MSL]) ->
   search(Handler, MSL);
search(_, []) ->
   false.

replace({Mod, Id}, [Ha | MSL], NewHa) when Ha#handler.module =:= Mod,
   Ha#handler.id =:= Id ->
   [NewHa | MSL];
replace(Mod, [Ha | MSL], NewHa) when Ha#handler.module =:= Mod,
   not Ha#handler.id ->
   [NewHa | MSL];
replace(Handler, [Ha | MSL], NewHa) ->
   [Ha | replace(Handler, MSL, NewHa)];
replace(_, [], NewHa) ->
   [NewHa].

tt145(0, List) ->
   ok;
tt145(N, List) ->
   case lists:keyfind({test, 2}, #handler.id, List) of
      #handler{} = A ->
         NewList = lists:keyreplace({test, 3}, #handler.id, List, A#handler{state = [332, 34, 3, 4, 3, 53, 6]}),
         tt145(N - 1, NewList);
      _ ->
         tt145(N - 1, List)
   end.

tt15(N) ->
   D = dict:new(),
   D1 = dict:store({test, 1}, #handler{id = 1}, D),
   tt15(N, D1).

tt15(0, CycleData) ->
   ok;
tt15(N, D1) ->
   New = case dict:find({test, 2}, D1) of
            error ->
               dict:store({test, 2}, #handler{id = 2}, D1);
            {ok, Value} ->
               dict:store({test, 2}, #handler{id = 2}, D1)
         end,
   tt15(N - 1, New).

tt16(N) ->
   put({test, 1}, #handler{id = 1}),
   tt166(N).

tt166(0) ->
   ok;
tt166(N) ->
   Ret = get({test, 2}),
   put({test, 2}, #handler{id = Ret}),
   tt166(N - 1).

get1({value, test, 1500, 1500}) ->
   {value, test, 1500, 1500}.
