-module(utTestMd5).

-compile([export_all, nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).

t1(N, Len) ->
   Bin = list_to_binary([X rem 256 || X <- lists:seq(1, Len)]),
   tt1(N, Bin, <<>>).

%% 这种方式更快
tt1(0, _Bin, Md5) ->
   Md5;
tt1(N, Bin, _Md5) ->
   Md5 = erlang:md5(Bin),
   tt1(N - 1, Bin, Md5).

t2(N, Len) ->
   Bin = list_to_binary([X rem 256 || X <- lists:seq(1, Len)]),
   tt2(N, Bin, <<>>).

tt2(0, _Bin, Md5) ->
   Md5;
tt2(N, Bin, _Md5) ->
   Md5 = crypto:hash(md5, Bin),
   tt2(N - 1, Bin, Md5).

t3(N, Len) ->
   Bin = list_to_binary([X rem 256 || X <- lists:seq(1, Len)]),
   Md5Bin = erlang:md5(Bin),
   tt3(N, Md5Bin, <<>>).

tt3(0, _Md5Bin, HexStr) ->
   HexStr;
tt3(N, Md5Bin, _HexStr) ->
   <<Hash:128/integer>> = Md5Bin,
   HexStr = string:to_lower(integer_to_list(Hash, 16)),
   tt3(N - 1, Md5Bin, HexStr).

t4(N, Len) ->
   Bin = list_to_binary([X rem 256 || X <- lists:seq(1, Len)]),
   Md5Bin = erlang:md5(Bin),
   tt4(N, Md5Bin, <<>>).

tt4(0, _Md5Bin, HexStr) ->
   HexStr;
tt4(N, Md5Bin, _HexStr) ->
   HexStr = lists:flatten([io_lib:format("~2.16.0b", [X]) || X <- binary_to_list(Md5Bin)]),
   tt4(N - 1, Md5Bin, HexStr).

%% 这种方式更加快 快好几倍呢
t5(N, Len) ->
   Bin = list_to_binary([X rem 256 || X <- lists:seq(1, Len)]),
   Md5Bin = erlang:md5(Bin),
   tt5(N, Md5Bin, <<>>).

tt5(0, _Md5Bin, HexStr) ->
   HexStr;
tt5(N, Md5Bin, _HexStr) ->
   HexStr = utHex:binaryToHex(Md5Bin),
   tt5(N - 1, Md5Bin, HexStr).

deHex(H) when H >= $a, H =< $f -> H - $a + 10;
deHex(H) when H >= $A, H =< $F -> H - $A + 10;
deHex(H) when H >= $0, H =< $9 -> H - $0.

t6(N) ->
   HexBin = "b59c67bf196a4758191e42f76670ceba",
   tt6(N, HexBin, []).

tt6(0, _HexBin, List) ->
   List;
tt6(N, HexBin, _List) ->
   HexBin1 = list_to_binary(HexBin),
   List = [(deHex(X1) * 16 + deHex(X2)) || <<X1:8, X2:8>> <= HexBin1],
   tt6(N - 1, HexBin, List).

%% 这种方式更加快 快好几倍呢
t7(N) ->
   Hex = "b59c67bf196a4758191e42f76670ceba",
   tt7(N, Hex).

tt7(0, Hex) ->
   {String, _} =
      lists:foldr(
         fun(E, {Acc, nolow}) ->
            {Acc, deHex(E)};
            (E, {Acc, LO}) ->
               {[deHex(E) * 16 + LO | Acc], nolow}
         end,
         {[], nolow}, Hex),
   String;
tt7(N, Hex) ->
   {String, _} =
      lists:foldr(
         fun(E, {Acc, nolow}) ->
            {Acc, deHex(E)};
            (E, {Acc, LO}) ->
               {[deHex(E) * 16 + LO | Acc], nolow}
         end,
         {[], nolow}, Hex),
   String,
   tt7(N - 1, Hex).

u4(0, Fun) ->
   ?MODULE:Fun();
u4(N, Fun) ->
   ?MODULE:Fun(),
   u4(N - 1, Fun).

uuid() ->
   erlang:md5(term_to_binary({erlang:system_time(nanosecond), rand:uniform(134217727), make_ref()})).
uuid2() ->
   term_to_binary({erlang:system_time(nanosecond), rand:uniform(134217727), make_ref()}).

get_uuid() ->
   <<(crypto:strong_rand_bytes(8))/bytes,
      (erlang:term_to_binary(erlang:timestamp()))/bytes>>.

u1(0) ->
   crypto:strong_rand_bytes(16);
u1(N) ->
   crypto:strong_rand_bytes(16),
   u1(N - 1).


u2(0) ->
   erlang:md5(term_to_binary({erlang:system_time(nanosecond), rand:uniform(134217727), make_ref()}));
u2(N) ->
   erlang:md5(term_to_binary({erlang:system_time(nanosecond), rand:uniform(134217727), make_ref()})),
   u2(N - 1).

u3(0) ->
   crypto:strong_rand_bytes(4);
u3(N) ->
   crypto:strong_rand_bytes(4),
   u3(N - 1).

u33(0) ->
   rand:uniform(2100000000);
u33(N) ->
   rand:uniform(2100000000),
   u33(N - 1).

u333(0) ->
   erlang:localtime();
u333(N) ->
   erlang:localtime(),
   u333(N - 1).

u4(0) ->
   erlang:md5(integer_to_list(erlang:phash2({os:system_time(micro_seconds), make_ref(), make_ref()})));
u4(N) ->
   erlang:md5(integer_to_list(erlang:phash2({os:system_time(micro_seconds), make_ref(), make_ref()}))),
   u1(N - 1).

