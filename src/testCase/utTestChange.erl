-module(utTestChange).

-compile([export_all, nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).

-record(test, {a, b, c, d}).

test() ->
   A = #test{a = 1, b = <<"abcdefg">>, c = 1, d = 0},
   io:format("IMY*********************t111 ~p ~p~n", [binaryAddr:getBinAddr(A#test.b), A]),
   B = A#test{a = 2},
   io:format("IMY*********************t222 ~p ~p~n", [binaryAddr:getBinAddr(B#test.b), B]),
   C = A#test{c = 2},
   io:format("IMY*********************t333 ~p ~p~n", [binaryAddr:getBinAddr(C#test.b), C]),

   Key = <<"aaaaaaaa">>,
   A1 = #{a => 1, b => <<"abcdefg">>, c => 1, Key => aa},
   keyAddr(A1, kkk111),
   io:format("IMY*********************m111 ~p ~p~n", [binaryAddr:getBinAddr(maps:get(b, A1)), A1]),
   B1 = A1#{a := 2},
   keyAddr(B1, kkk222),
   io:format("IMY*********************m222 ~p ~p~n", [binaryAddr:getBinAddr(maps:get(b, B1)), B1]),
   C1 = A1#{c := 2},
   keyAddr(C1, kkk3333),
   io:format("IMY*********************m333 ~p ~p~n", [binaryAddr:getBinAddr(maps:get(b, C1)), C1]),
   D1 = maps:put(tt, vv, A1),
   keyAddr(D1, kkk4444),
   io:format("IMY*********************m444 ~p ~p~n", [binaryAddr:getBinAddr(maps:get(b, C1)), D1]).

keyAddr(Map, Tag) ->
   List = maps:keys(Map),
   [begin io:format("IMY********************* ~p ~p~n", [Tag, binaryAddr:getBinAddr(Key)]) end || Key <- List, is_binary(Key) ].