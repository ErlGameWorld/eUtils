-module(utTestHand).

-compile([export_all, nowarn_function, nowarn_unused_vars, nowarn_export_all]).

test(0, Key, _) ->
   utTestCfg1028:hand(Key);
test(N, Key, _) ->
   test(N - 1, Key, utTestCfg1028:hand(Key)).

test2(0, Key, Key2, _) ->
   utTestHand1028:hand(Key, Key2);
test2(N, Key, Key2, _) ->
   test2(N - 1, Key, Key2, utTestHand1028:hand(Key, Key2)).

test3(0, Key2, _) ->
   utTestHand1028:hand(element(1, Key2), Key2);
test3(N, Key2, _) ->
   test3(N - 1, Key2, utTestHand1028:hand(element(1, Key2), Key2)).
