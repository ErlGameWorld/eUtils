-module(utGenCode).

-compile([export_all, nowarn_function, nowarn_unused_vars, nowarn_export_all]).

write(N, Mod) ->
   Head = <<"-module(", (list_to_binary(Mod))/binary, ").\n-compile([export_all, nowarn_function, nowarn_unused_vars, nowarn_export_all]).\n">>,
   HandStr = makeStr(N, Head),
   ok = file:write_file("./" ++ Mod ++ ".erl", HandStr).

makeStr(0, BinStr) ->
   <<BinStr/binary, "getV(_) -> undefined.">>;
makeStr(N, BinStr) ->
   Str = <<"getV(", (integer_to_binary(N))/binary, ")-> ", (integer_to_binary(N))/binary, ";\n">>,
   makeStr(N - 1, <<BinStr/binary, Str/binary>>).

