-module(utTestBinary).

-compile([export_all, nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).

genBinary(N) ->
   list_to_binary([X rem 256 || X <- lists:seq(1, N)]).

genList(N) ->
   ([X rem 256 || X <- lists:seq(1, N)]).

t0(N, Y) ->
   Bin = genBinary(Y),
   tt0(N, Bin).

tt0(0, Bin) ->
   ok;
tt0(N, Bin) ->
   [X || <<X:16>> <= Bin],
   tt0(N - 1, Bin).


t1(N, Y) ->
   Bin = genBinary(Y),
   tt1(N, Bin).

tt1(0, Bin) ->
   ok;
tt1(N, Bin) ->
   [X || <<X:32>> <= Bin],
   tt1(N - 1, Bin).

t2(N, Y) ->
   Bin = genBinary(Y),
   tt2(N, Bin, Y div 2).

tt2(0, Bin, Len) ->
   ok;
tt2(N, Bin, Len) ->
   deInt16List(Len, Bin, []),
   tt2(N - 1, Bin, Len).

t3(N, Y) ->
   Bin = genBinary(Y),
   tt3(N, Bin, Y div 4).

tt3(0, Bin, Len) ->
   ok;
tt3(N, Bin, Len) ->
   deInt32List(Len, Bin, []),
   tt3(N - 1, Bin, Len).

deInt16List(0, MsgBin, RetList) ->
   {lists:reverse(RetList), MsgBin};
deInt16List(N, MsgBin, RetList) ->
   <<Int:16/big-signed, LeftBin/binary>> = MsgBin,
   deInt16List(N - 1, LeftBin, [Int | RetList]).

deInt32List(0, MsgBin, RetList) ->
   {lists:reverse(RetList), MsgBin};
deInt32List(N, MsgBin, RetList) ->
   <<Int:32/big-signed, LeftBin/binary>> = MsgBin,
   deInt32List(N - 1, LeftBin, [Int | RetList]).

-define(int16(V), <<V:16/big>>).
-define(int32(V), <<V:32/big>>).

t4(N, Y) ->
   List = genList(Y),
   tt4(N, List).

tt4(0, List) ->
   ok;
tt4(N, List) ->
   _ = [<<V:64/big>> || V <- List],
   tt4(N - 1, List).


t5(N, Y) ->
   List = genList(Y),
   tt5(N, List).

tt5(0, List) ->
   ok;
tt5(N, List) ->
   _ = [<<(length(List)):16/big>>, [<<V:64/big>> || V <- List]],
   tt5(N - 1, List).

t44(N, Y) ->
   List = genList(Y),
   tt44(N, List).

tt44(0, List) ->
   ok;
tt44(N, List) ->
   <<<<V:64/big>> || V <- List>>,
   tt44(N - 1, List).


t6(N, Y) ->
   List = genList(Y),
   tt6(N, List).

tt6(0, List) ->
   ok;
tt6(N, List) ->
   _ = [<<(length(List)):16/big>>, <<<<V:64/big>> || V <- List>>],
   tt6(N - 1, List).

t7(N, Y) ->
   List = genList(Y),
   tt7(N, List).

tt7(0, List) ->
   ok;
tt7(N, List) ->
   _ = <<(<<(length(List)):16/big>>)/binary, (<<<<V:64/big>> || V <- List>>)/binary>>,
   tt7(N - 1, List).

t8(N, Y) ->
   List = genList(Y),
   tt4(N, List).

tt8(0, List) ->
   ok;
tt8(N, List) ->
   <<begin case V rem 2 == 1 of true -> <<1>>; _ -> <<0>> end end || V <- List>>,
tt4(N - 1, List).






