-module(utTestAtomics).

-compile([export_all, nowarn_function, nowarn_unused_vars, nowarn_export_all]).


tt1(N) ->
   Ref = ets:new(test, [public, set]),
   ets:insert(Ref, {rate, 1}),
   tt1(N, Ref).

tt1(0, Ref) ->
   io:format("IMY************ets ~p~n", [ets:tab2list(Ref)]),
   ok;
tt1(N, Ref) ->
   ets:update_counter(Ref, rate, {2, 1}),
   tt1(N - 1, Ref).

tt2(N) ->
   Ref = atomics:new(1, []),
   atomics:put(Ref, 1, 1),
   tt2(N, Ref).

tt2(0, Ref) ->
   io:format("IMY************atomics ~p~n", [atomics:get(Ref, 1)]),
   ok;
tt2(N, Ref) ->
   atomics:add(Ref, 1, 1),
   tt2(N - 1, Ref).

tt3(N) ->
   put(rate, 1),
   Ref = make_ref(),
   tt3(N, Ref).

tt3(0, _Ref) ->
   io:format("IMY************dic ~p~n", [get(rate)]),
   ok;
tt3(N, Ref) ->
   put(rate, get(rate) + 1),
   tt3(N - 1, Ref).

tt4(N) ->
   Ref = atomics:new(1, []),
   atomics:put(Ref, 1, 1),
   persistent_term:put(ref, Ref),
   ttt4(N).

ttt4(0) ->
   io:format("IMY************dic ~p~n", [atomics:get(persistent_term:get(ref), 1)]),
   ok;
ttt4(N) ->
   atomics:add_get(persistent_term:get(ref), 1, 1),
   ttt4(N - 1).

tt5(N) ->
   put(ref, make_ref()),
   ttt5(N, 0).

ttt5(0, _Ref) ->
   io:format("IMY************dic ~p~n", [get(ref)]),
   ok;
ttt5(N, _Ref) ->
   Ref = get(ref),
   ttt5(N - 1, Ref).

tt6(N) ->
   Ref = atomics:new(1, []),
   atomics:put(Ref, 1, 1),
   tt6(N, Ref).

tt6(0, Ref) ->
   io:format("IMY************atomics ~p~n", [atomics:get(Ref, 1)]),
   ok;
tt6(N, Ref) ->
   atomics:get(Ref, 1),
   tt6(N - 1, Ref).

tt7(0) ->
   io:format("IMY***************  ~p~n", [erlang:system_time(milli_seconds)]);
tt7(N) ->
   erlang:system_time(milli_seconds),
   tt7(N - 1).

tt8(0) ->
   {M, S, Ms} = os:timestamp(),
   A = M * 1000000000 + S * 1000 + Ms div 1000,
   io:format("IMY************ets ~p~n", [A]);
tt8(N) ->
   {M, S, Ms} = os:timestamp(),

   tt8(N - 1).

t9(N) ->
   put(test, []),
   tt9(N).
tt9(0) ->
   ok;
tt9(N) ->
   put(test, [N | erase(a)]),
   tt9(N - 1).

t10(N) ->
   put(test, []),
   tt10(N).
tt10(0) ->
   ok;
tt10(N) ->
   put(test, [N | get(a)]),
   tt10(N - 1).

a1(N) ->
   Ref = atomics:new(1, []),
   persistent_term:put(ref, Ref),
   a1(N, 0).

a1(0, _Ref) ->
   io:format("IMY************a1 ~p~n", [persistent_term:get(ref)]);
a1(N, _Ref) ->
   Ref = persistent_term:get(ref),
   a1(N - 1, Ref).

a2(N) ->
   Ref = atomics:new(1, []),
   utKvsToBeam:load(utTestRef, [{ref, erlang:ref_to_list(Ref)}]),
   a2(N, 0).

a2(0, _Ref) ->
   io:format("IMY************a2 ~p~n", [utTestRef:get(ref)]);
a2(N, _Ref) ->
   Ref = erlang:list_to_ref(utTestRef:get(ref)),
   a2(N - 1, Ref).














