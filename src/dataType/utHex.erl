-module(utHex).

-compile([export_all, nowarn_export_all]).

-define(Hex(X), (getHex(X)):16).

-spec binaryToHex(Bin :: binary()) -> string().
binaryToHex(Bin) ->
   HexBin = <<<<?Hex(X)>> || <<X:8>> <= Bin>>,
   erlang:binary_to_list(HexBin).

-spec binaryToHexBin(Bin :: binary()) -> binary().
binaryToHexBin(Bin) ->
   <<<<?Hex(X)>> || <<X:8>> <= Bin>>.

-spec hexToString(Hex :: string()) -> string().
hexToString(Hex) ->
   {String, _} =
      lists:foldr(
         fun(E, {Acc, nolow}) ->
            {Acc, deHex(E)};
            (E, {Acc, LO}) ->
               {[deHex(E) * 16 + LO | Acc], nolow}
         end,
         {[], nolow}, Hex),
   String.

-spec hexToBinary(Hex :: string()) -> string().
hexToBinary(Hex) ->
   HexBin = list_to_binary(Hex),
   <<<<(deHex(X1) * 16 + deHex(X2))>> || <<X1:8, X2:8>> <= HexBin>>.

-spec hexBinToString(HexBin :: binary()) -> string().
hexBinToString(HexBin) ->
   [(deHex(X1) * 16 + deHex(X2)) || <<X1:8, X2:8>> <= HexBin].

-spec hexBinToBinary(HexBin :: binary()) -> string().
hexBinToBinary(HexBin) ->
   <<<<(deHex(X1) * 16 + deHex(X2))>> || <<X1:8, X2:8>> <= HexBin>>.

-spec integerToHex(I :: integer()) -> string().
integerToHex(I) ->
   erlang:integer_to_list(I, 16).

-spec hexToInteger(Hex :: string()) -> integer().
hexToInteger(Hex) ->
   lists:foldl(fun(E, Acc) -> Acc * 16 + deHex(E) end, 0, Hex).

-spec rawBinaryToInteger(Bin :: binary()) -> integer().
rawBinaryToInteger(Bin) ->
   rawBinaryToInteger(Bin, 0).

-spec rawBinaryToInteger(Bin :: binary(), Acc :: integer()) -> integer().
rawBinaryToInteger(<<>>, Acc) ->
   Acc;
rawBinaryToInteger(<<X:8, Rest/binary>>, Acc) ->
   rawBinaryToInteger(Rest, Acc * 256 + X).

-spec integerToRawBinary(I :: integer()) -> binary().
integerToRawBinary(I) ->
   integerToRawBinary(I, 16, []).

-spec integerToRawBinary(I :: integer(), Len :: pos_integer()) -> binary().
integerToRawBinary(I, Len) ->
   integerToRawBinary(I, Len, []).

integerToRawBinary(I, Len, Acc) when I < 256 ->
   NewAcc = [I | Acc],
   ZeroPadding = lists:duplicate(Len - length(NewAcc), 0),
   NewAcc2 = [ZeroPadding | NewAcc],
   list_to_binary(NewAcc2);
integerToRawBinary(I, Len, Acc) ->
   Div = I div 256,
   Rem = I rem 256,
   integerToRawBinary(Div, Len, [Rem | Acc]).

deHex(H) when H >= $a, H =< $f -> H - $a + 10;
deHex(H) when H >= $A, H =< $F -> H - $A + 10;
deHex(H) when H >= $0, H =< $9 -> H - $0.

-compile({inline, [getHex/1]}).
-spec getHex(X :: non_neg_integer()) -> integer().
getHex(X) ->
   element(X + 1,
      {
         16#3030, 16#3031, 16#3032, 16#3033, 16#3034, 16#3035, 16#3036,
         16#3037, 16#3038, 16#3039, 16#3061, 16#3062, 16#3063, 16#3064,
         16#3065, 16#3066, 16#3130, 16#3131, 16#3132, 16#3133, 16#3134,
         16#3135, 16#3136, 16#3137, 16#3138, 16#3139, 16#3161, 16#3162,
         16#3163, 16#3164, 16#3165, 16#3166, 16#3230, 16#3231, 16#3232,
         16#3233, 16#3234, 16#3235, 16#3236, 16#3237, 16#3238, 16#3239,
         16#3261, 16#3262, 16#3263, 16#3264, 16#3265, 16#3266, 16#3330,
         16#3331, 16#3332, 16#3333, 16#3334, 16#3335, 16#3336, 16#3337,
         16#3338, 16#3339, 16#3361, 16#3362, 16#3363, 16#3364, 16#3365,
         16#3366, 16#3430, 16#3431, 16#3432, 16#3433, 16#3434, 16#3435,
         16#3436, 16#3437, 16#3438, 16#3439, 16#3461, 16#3462, 16#3463,
         16#3464, 16#3465, 16#3466, 16#3530, 16#3531, 16#3532, 16#3533,
         16#3534, 16#3535, 16#3536, 16#3537, 16#3538, 16#3539, 16#3561,
         16#3562, 16#3563, 16#3564, 16#3565, 16#3566, 16#3630, 16#3631,
         16#3632, 16#3633, 16#3634, 16#3635, 16#3636, 16#3637, 16#3638,
         16#3639, 16#3661, 16#3662, 16#3663, 16#3664, 16#3665, 16#3666,
         16#3730, 16#3731, 16#3732, 16#3733, 16#3734, 16#3735, 16#3736,
         16#3737, 16#3738, 16#3739, 16#3761, 16#3762, 16#3763, 16#3764,
         16#3765, 16#3766, 16#3830, 16#3831, 16#3832, 16#3833, 16#3834,
         16#3835, 16#3836, 16#3837, 16#3838, 16#3839, 16#3861, 16#3862,
         16#3863, 16#3864, 16#3865, 16#3866, 16#3930, 16#3931, 16#3932,
         16#3933, 16#3934, 16#3935, 16#3936, 16#3937, 16#3938, 16#3939,
         16#3961, 16#3962, 16#3963, 16#3964, 16#3965, 16#3966, 16#6130,
         16#6131, 16#6132, 16#6133, 16#6134, 16#6135, 16#6136, 16#6137,
         16#6138, 16#6139, 16#6161, 16#6162, 16#6163, 16#6164, 16#6165,
         16#6166, 16#6230, 16#6231, 16#6232, 16#6233, 16#6234, 16#6235,
         16#6236, 16#6237, 16#6238, 16#6239, 16#6261, 16#6262, 16#6263,
         16#6264, 16#6265, 16#6266, 16#6330, 16#6331, 16#6332, 16#6333,
         16#6334, 16#6335, 16#6336, 16#6337, 16#6338, 16#6339, 16#6361,
         16#6362, 16#6363, 16#6364, 16#6365, 16#6366, 16#6430, 16#6431,
         16#6432, 16#6433, 16#6434, 16#6435, 16#6436, 16#6437, 16#6438,
         16#6439, 16#6461, 16#6462, 16#6463, 16#6464, 16#6465, 16#6466,
         16#6530, 16#6531, 16#6532, 16#6533, 16#6534, 16#6535, 16#6536,
         16#6537, 16#6538, 16#6539, 16#6561, 16#6562, 16#6563, 16#6564,
         16#6565, 16#6566, 16#6630, 16#6631, 16#6632, 16#6633, 16#6634,
         16#6635, 16#6636, 16#6637, 16#6638, 16#6639, 16#6661, 16#6662,
         16#6663, 16#6664, 16#6665, 16#6666
      }).
