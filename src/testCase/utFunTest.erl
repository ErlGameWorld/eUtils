-module(utFunTest).

-compile([export_all, nowarn_function, nowarn_unused_vars, nowarn_export_all]).

test(A) ->
	Fun = fun(B, F) ->
		Ret = if A > 10 ->
			A + B;
			A > 5 ->
				F(A * B, F);
			true ->
				B
		end,
		io:format("~p~n", [Ret]),
		Ret + B
	      end,
	Fun(1, Fun).


test(A, Cnt) ->
	 <<_:Cnt, A1:32, _/bitstring>> = A,
	A1.