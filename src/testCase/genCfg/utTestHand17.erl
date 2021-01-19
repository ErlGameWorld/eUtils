-module(utTestHand17).
-compile([export_all, nowarn_function, nowarn_unused_vars, nowarn_export_all]).
hand({test16, V1, V2, V100}, Test16) ->
	16;
hand({test15, V1, V2, V3, V4, V100}, Test15) ->
	15;
hand({test14, V1, V2, V100}, Test14) ->
	14;
hand({test13, V1, V2, V3, V4, V5, V100}, Test13) ->
	13;
hand({test12, V1, V2, V100}, Test12) ->
	12;
hand(_, _) -> undefined.