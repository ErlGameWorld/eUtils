-module(utTestBinAddr).

-compile([export_all, nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).

getBin11() ->
   <<"fdsafsdf范德萨发送发送"/utf8>>.

getBin12() ->
   <<"fdsafsdf范德萨发送发送"/utf8>>.

getBin21() ->
   <<"fdsafsdfsdfdsfasfdsffffffffffffffffffffffffffffffffffffffffffffffffffffHJlhdsjflshdalfkhsaklfhsdafjkdshfkla范德萨发送发送"/utf8>>.

getBin22() ->
   <<"fdsafsdfsdfdsfasfdsffffffffffffffffffffffffffffffffffffffffffffffffffffHJlhdsjflshdalfkhsaklfhsdafjkdshfkla范德萨发送发送"/utf8>>.

test() ->
   A1 = <<"fdsafsdf范德萨发送发送"/utf8>>,
   A2 = <<"fdsafsdf范德萨发送发送"/utf8>>,
   B = <<"fdsafsdfsdfdsfasfdsffffffffffffffffffffffffffffffffffffffffffffffffffffHJlhdsjflshdalfkhsaklfhsdafjkdshfkla范德萨发送发送"/utf8>>,
   io:format("IMY************************~n~p\n~p\n~p\n~p\n~p\n~p~n~p\n~p\n ddd ~p\n",
      [
         binaryAddr:getBinAddr(getBin11()),
         binaryAddr:getBinAddr(getBin12()),
         binaryAddr:getBinAddr(utTestBinAddr2:getBin11()),
         binaryAddr:getBinAddr(utTestBinAddr2:getBin12()),
         binaryAddr:getBinAddr(getBin21()),
         binaryAddr:getBinAddr(getBin22()),
         binaryAddr:getBinAddr(A1),
         binaryAddr:getBinAddr(A2),
         binaryAddr:getBinAddr(B)
      ]).





