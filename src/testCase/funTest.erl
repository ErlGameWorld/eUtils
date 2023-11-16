-module(funTest).

-include("utComMisc.hrl").

-compile([export_all, nowarn_function, nowarn_unused_vars, nowarn_export_all]).

test2(A) ->
   Fun1 = fun(A1, A2, A3, A4, A5, Fun1) ->
      if A1 > 100 ->
         A1 + A2;
         true ->
            Fun1(A1, A2, A3, A4, A5, A2) end
   end,
   Fun1(A, A, A, A, A, Fun1).

test3(A) ->
   _ = [O * 2 || O <- A],
   ok.

-include("utTime.hrl").

-define(types, [1, 1.1, [], [1], {1}, #{}, <<"123">>, self()]).

type1() ->
   [dataType(One) || One <-  ?types],
   ok.

dataType(Data) when is_list(Data) -> list_do;
dataType(Data) when is_integer(Data) -> integer_do;
dataType(Data) when is_binary(Data) -> binary_do;
dataType(Data) when is_function(Data) -> function_do;
dataType(Data) when is_tuple(Data) -> tuple_do;
dataType(Data) when is_atom(Data) -> atom_do;
dataType(Data) when is_float(Data) -> float_do;
dataType(Data) when is_number(Data) -> number_do;
dataType(Data) when is_pid(Data) -> pid_do;
dataType(Data) when is_port(Data) -> port_do;
dataType(_Data) -> not_know_do.

type2() ->
   [erts_internal:term_type(One) || One <-  ?types],
   ok.

dataType(nil, Data) when is_list(Data) -> list_do;
dataType(list, Data) when is_list(Data) -> list_do;
dataType(fixnum, Data) when is_integer(Data) -> integer_do;
dataType(bignum, Data) when is_integer(Data) -> integer_do;
dataType(refc_binary, Data) when is_binary(Data) -> binary_do;
dataType(heap_binary, Data) when is_binary(Data) -> binary_do;
dataType(sub_binary, Data) when is_binary(Data) -> binary_do;
dataType(tuple, Data) when is_tuple(Data) -> tuple_do;
dataType(atom, Data) when is_atom(Data) -> atom_do;
dataType(hfloat, Data) when is_float(Data) -> float_do;
dataType(pid, Data) when is_pid(Data) -> pid_do;
dataType(pid, Data) when is_pid(Data) -> pid_do;
dataType(external_pid, Data) when is_port(Data) -> port_do;
dataType(_, _Data) -> not_know_do.

type3() ->
   [dataType(eTermType:termType(One), One) || One <-  ?types],
   ok.

ok(0) ->
   ok;
ok(N) ->
   ok(N - 1).

test(A) ->
   ?CASE(lists:keyfind(a, 1, A), false, none, {_, V}, V).

ams1(A) ->  ok.
ams2(A) -> fun() -> A end.

amf1(0, A, B, C, _) -> ok;
amf1(N, A, B, C, _) ->
   amf1(N - 1, A, B, C, 1) .

amf2(0, A, B, C, _) -> ok;
amf2(N, A, B, C, _) ->
   Fun = fun(One1) -> X = (One1 * A * B * C), Y = B-C, Z = X / Y, Z end,
   amf2(N - 1, A, B, C, Fun).

ok1(0, A, _) ->
   ok;
ok1(N, A, _) ->
   IS =  A == 1 orelse A == 2 orelse A == 3 orelse A == 4 orelse A == 5 orelse A == 6,
   ok1(N - 1, A, IS).

ok2(0, A, _) ->
   ok;
ok2(N, A, _) ->
   IS = lists:member(A, [1, 2, 3, 4, 5, 6]),
   ok2(N - 1, A, IS).


test2() ->
   {Time, _} = timer:tc(?MODULE, compare, [100000000]),
   io:format("Time : ~p~n", [Time]),
   {Time2, _} = timer:tc(?MODULE, compare2, [100000000]),
   io:format("Time2: ~p~n", [Time2]).

compare(0) ->
   ok;
compare(N)  when N =/= (N-1) orelse N =:= N  ->
   compare(N - 1).

compare2(0) ->
   ok;
compare2(N) when N /= (N-1) orelse N == N ->
   compare(N - 1).


timer(_, _) ->
   I = atomics:add_get(persistent_term:get(cnt), 1, 1),
   io:format("IMY******* ~p~n", [I]) ,
   case I of
   	1000000 ->
   		io:format("end time ~p ~n", [erlang:system_time(millisecond)]);
   	_ ->
   		ignore
   end,
   ok.

test(N, Time) ->
   io:format("start time1 ~p ~n", [erlang:system_time(millisecond)]),
   persistent_term:put(cnt, atomics:new(1, [])),
   gTimer:startWork(16),
   doTest(N, Time).
doTest(0, Time) ->
   io:format("start time2 ~p ~n", [erlang:system_time(millisecond)]),
   gTimer:setTimer(rand:uniform(Time), {?MODULE, timer, []});
doTest(N, Time) ->
   gTimer:setTimer(rand:uniform(Time), {?MODULE, timer, []}),
   doTest(N - 1, Time).


timer(_) ->
   %io:format("IMY******* ~p~n", [I]) ,
   % case I of
   %    1000000 ->
   %       io:format("end time ~p ~n", [erlang:system_time(millisecond)]);
   %    _ ->
   %       ignore
   % end,
   ok.


testBBB(Bin1) ->
   Bin2 = term_to_binary(Bin1),
   Bin = <<1:1, Bin2/binary>>,
   io:format("IMY**********testBBB  ~p size ~p ~p ~p ~n", [self(), bit_size(Bin), erts_internal:term_type(Bin), binaryAddr:getBinAddr(Bin2)]),
   spawn(?MODULE, tt, [Bin]).

tt(B) ->
   <<_A:1, Bin/binary>> = B,
   io:format("IMY**********ttxxxxx ~p size ~p ~p ~p ~n", [self(), bit_size(B), erts_internal:term_type(B), binaryAddr:getBinAddr(Bin)]).


testCCC() ->
   B1 = <<1:(8 * 64)>>,                      % 小于64字节
   B2 = <<1:(8 * 65)>>,                      % 大于64字节
   B3 = <<B1/binary, B2/binary, 1:1>>,       % 混入1bit的bitstring
   <<B4:64/binary, B5:65/binary, _:1>> = B3,
   io:format("Pid:~w~n B1:~p~n B2:~p~n B3:~p~n B4:~p~n B5:~p~n",[
      self(),
      {erts_internal:term_type(B1), binaryAddr:getBinAddr(B1)},
      {erts_internal:term_type(B2), binaryAddr:getBinAddr(B2)},
      {erts_internal:term_type(B3), binaryAddr:getBinAddr(B3)},
      {erts_internal:term_type(B4), binaryAddr:getBinAddr(B4)},
      {erts_internal:term_type(B5), binaryAddr:getBinAddr(B5)}
   ]),
   spawn(?MODULE, ttt, [B3, erlang:size(B1), erlang:size(B2)]).

testDDD(B1, B2) ->
   B3 = <<B1/binary, B2/binary, 1:1>>,       % 混入1bit的bitstring
   <<B4:(erlang:size(B1))/binary, B5:(erlang:size(B2))/binary, _:1>> = B3,
   io:format("Pid:~w~n B1:~p~n B2:~p~n B3:~p~n B4:~p~n B5:~p~n",[
      self(),
      {erts_internal:term_type(B1), binaryAddr:getBinAddr(B1)},
      {erts_internal:term_type(B2), binaryAddr:getBinAddr(B2)},
      {erts_internal:term_type(B3), binaryAddr:getBinAddr(B3)},
      {erts_internal:term_type(B4), binaryAddr:getBinAddr(B4)},
      {erts_internal:term_type(B5), binaryAddr:getBinAddr(B5)}
   ]),
   spawn(?MODULE, ttt, [B3, erlang:size(B1), erlang:size(B2)]).

ttt(B, Size1, Size2) ->
   <<B1:Size1/binary, B2:Size2/binary, _:1>> = B,  % 匹配出小于64字节和大于64字节的binary数据
   io:format("Pid:~w~n B1:~p~n B2:~p~n",[
      self(),
      {erts_internal:term_type(B1), binaryAddr:getBinAddr(B1)},
      {erts_internal:term_type(B2), binaryAddr:getBinAddr(B2)}
   ]).

testEEE(B1, B2) ->
   B3 = <<1:1, B1/binary, B2/binary>>,       % 混入1bit的bitstring
   <<_:1, B4:(erlang:size(B1))/binary, B5:(erlang:size(B2))/binary>> = B3,
   io:format("Pid:~w~n B1:~p~n B2:~p~n B3:~p~n B4:~p~n B5:~p~n",[
      self(),
      {erts_internal:term_type(B1), binaryAddr:getBinAddr(B1)},
      {erts_internal:term_type(B2), binaryAddr:getBinAddr(B2)},
      {erts_internal:term_type(B3), binaryAddr:getBinAddr(B3)},
      {erts_internal:term_type(B4), binaryAddr:getBinAddr(B4)},
      {erts_internal:term_type(B5), binaryAddr:getBinAddr(B5)}
   ]),
   spawn(?MODULE, tttt, [B3, erlang:size(B1), erlang:size(B2)]).

tttt(B, Size1, Size2) ->
   <<_:1, B1:Size1/binary, B2:Size2/binary>> = B,  % 匹配出小于64字节和大于64字节的binary数据
   io:format("Pid:~w~n B1:~p~n B2:~p~n",[
      self(),
      {erts_internal:term_type(B1), binaryAddr:getBinAddr(B1)},
      {erts_internal:term_type(B2), binaryAddr:getBinAddr(B2)}
   ]).


