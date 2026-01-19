-module(utTypeCast).

-compile([export_all, nowarn_export_all]).

toFloat(Value) when is_list(Value) -> list_to_float(Value);
toFloat(Value) when is_binary(Value) -> binary_to_float(Value);
toFloat(Value) when is_integer(Value) -> erlang:float(Value);
toFloat(Value) when is_float(Value) -> Value.

toList(undefined) -> "undefined";
toList(null) -> "null";
toList(Value) when is_tuple(Value) -> tuple_to_list(Value);
toList(Value) when is_binary(Value) -> binary_to_list(Value);
toList(Value) when is_bitstring(Value) -> bitstring_to_list(Value);
toList(Value) when is_integer(Value) -> integer_to_list(Value);
toList(Value) when is_float(Value) -> float_to_list(Value, [{decimals, 6}, compact]);
toList(Value) when is_atom(Value) -> atom_to_list(Value);
toList(Value) when is_list(Value) -> Value;
toList([Tuple | PropList] = Value) when is_list(PropList) and is_tuple(Tuple) ->
	lists:map(fun({K, V}) -> {toList(K), toList(V)} end, [Value]).

%% to_list(Term) when is_binary(Term) ->
%%    case unicode:characters_to_binary(Term, utf8, utf8) of
%%       Term ->
%%          unicode:characters_to_list(Term);
%%       _ ->
%%          binary_to_list(Term)
%%    end.

toBinary(Value) when is_integer(Value) -> integer_to_binary(Value);
toBinary(Value) when is_list(Value) -> list_to_binary(Value);
toBinary(Value) when is_float(Value) -> float_to_binary(Value, [{decimals, 6}, compact]);
toBinary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
toBinary(Value) when is_binary(Value) -> Value;
toBinary([Tuple | PropList] = Value) when is_list(PropList) and is_tuple(Tuple) ->
	lists:map(fun({K, V}) -> {toBinary(K), toBinary(V)} end, Value);
toBinary(Value) -> term_to_binary(Value).

toInteger(undefined) -> undefined;
toInteger(Value) when is_float(Value) -> trunc(Value);
toInteger(Value) when is_list(Value) -> list_to_integer(Value);
toInteger(Value) when is_binary(Value) -> binary_to_integer(Value);
toInteger(Value) when is_tuple(Value) -> toInteger(tuple_to_list(Value));
toInteger(Value) when is_integer(Value) -> Value.


dataType(Data) when is_list(Data) -> list;
dataType(Data) when is_integer(Data) -> integer;
dataType(Data) when is_binary(Data) -> binary;
dataType(Data) when is_function(Data) -> function;
dataType(Data) when is_tuple(Data) -> tuple;
dataType(Data) when is_map(Data) -> map;
dataType(Data) when is_atom(Data) -> atom;
%%dataType(Data) when is_boolean(Data) -> boolean;
dataType(Data) when is_bitstring(Data) -> bitstring;
dataType(Data) when is_float(Data) -> float;
dataType(Data) when is_number(Data) -> number;
dataType(Data) when is_reference(Data) -> reference;
dataType(Data) when is_pid(Data) -> pid;
dataType(Data) when is_port(Data) -> port;
dataType(_Data) -> not_know.

%% Trim the binary
-spec trim(Bin :: binary()) -> binary().
trim(Bin) when is_binary(Bin) -> trimHead(trimTail(Bin)).

%% Trim head of binary
-spec trimHead(Bin :: binary()) -> binary().
trimHead(<<>>) -> <<>>;
trimHead(<<C, BinTail/binary>> = Bin) ->
	case is_whitespace(C) of
		true -> trimHead(BinTail);
		false -> Bin
	end.

%% Trim tail of binary
-spec trimTail(Bin :: binary()) -> binary().
trimTail(<<>>) -> <<>>;
trimTail(Bin) ->
	Size = byte_size(Bin) - 1,
	<<BinHead:Size/binary, C>> = Bin,
	case is_whitespace(C) of
		true -> trimTail(BinHead);
		false -> Bin
	end.

%% Check if the char is a whitespace
-spec is_whitespace(char()) -> true | false.
is_whitespace($\s) -> true;
is_whitespace($\t) -> true;
is_whitespace($\n) -> true;
is_whitespace($\r) -> true;
is_whitespace(_) -> false.

%% term序列化, term转为string
term_to_string(Term) ->
	unicode:characters_to_binary(eFmt:formatIol("~0tp", [Term])).

%% term反序列化, string转换为term
string_to_term(String) ->
	Str = case is_binary(String) of true -> unicode:characters_to_list(String);  _ -> String end,
	
	case erl_scan:string(Str ++ ".") of
		{ok, Tokens, _} ->
			case erl_parse:parse_term(Tokens) of
				{ok, Term} -> {ok, Term};
				Error -> {error, Error}
			end;
		Error ->
			{error, Error}
	end.

%% term序列化，term转换为string格式，e.g., [{a},1] => "[{a},1]"
term_to_string1(Term) ->
	binary_to_list(list_to_binary(io_lib:format("~w", [Term]))).

%% term序列化，term转换为bitstring格式，e.g., [{a},1] => <<"[{a},1]">>
term_to_bitstring(Term) ->
	erlang:list_to_bitstring(io_lib:format("~w", [Term])).

%% term反序列化，string转换为term，e.g., "[{a},1]"  => [{a},1]
string_to_term1(String) ->
	case String of
		[] -> [];
		_ ->
			case erl_scan:string(String ++ ".") of
				{ok, Tokens, _} ->
					case erl_parse:parse_term(Tokens) of
						{ok, Term} -> Term;
						_Err -> undefined
					end;
				_Error ->
					undefined
			end
	end.

%%将列表转换为string [a,b,c] -> "a,b,c"
list_to_string(List) ->
	case List == [] orelse List == "" of
		true -> "";
		false ->
			F = fun(E) ->
				tool:to_list(E) ++ ","
				end,
			L1 = [F(E) || E <- List],
			L2 = lists:concat(L1),
			string:substr(L2, 1, length(L2) - 1)
	end.

%% term反序列化，bitstring转换为term，e.g., <<"[{a},1]">>  => [{a},1]
bitstring_to_term(undefined) -> undefined;
bitstring_to_term(BitString) ->
	string_to_term(tool:to_list(BitString)).

termToBase64(Term) ->
	base64:encode(term_to_binary(Term)).

base64ToTerm(Base64String) ->
	binary_to_term(base64:decode(Base64String)).
