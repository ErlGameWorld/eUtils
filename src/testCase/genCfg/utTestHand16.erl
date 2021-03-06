-module(utTestHand16).
-compile([export_all, nowarn_function, nowarn_unused_vars, nowarn_export_all]).
hand(test16, {test16, V1, V2, V100}) ->
	16;
hand(test15, {test15, V1, V2, V3, V4, V100}) ->
	15;
hand(test14, {test14, V1, V2, V100}) ->
	14;
hand(test13, {test13, V1, V2, V3, V4, V5, V100}) ->
	13;
hand(test12, {test12, V1, V2, V100}) ->
	12;
hand(test11, {test11, V1, V2, V3, V4, V5, V100}) ->
	11;
hand(test10, {test10, V1, V2, V3, V4, V5, V100}) ->
	10;
hand(test9, {test9, V1, V2, V3, V4, V5, V6, V100}) ->
	9;
hand(test8, {test8, V1, V2, V3, V4, V100}) ->
	8;
hand(test7, {test7, V1, V2, V3, V100}) ->
	7;
hand(test6, {test6, V1, V100}) ->
	6;
hand(test5, {test5, V1, V2, V3, V100}) ->
	5;
hand(test4, {test4, V1, V100}) ->
	4;
hand(test3, {test3, V1, V2, V3, V100}) ->
	3;
hand(test2, {test2, V1, V100}) ->
	2;
hand(test1, {test1, V1, V2, V3, V4, V5, V6, V100}) ->
	1;
hand(_, _) -> undefined.