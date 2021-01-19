-module(utTuple).

-define(LIST_TO_RECORD(RecName, List), list_to_tuple([RecName])).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
	copy_elements/2,
	update/2,
	update_counter/2,
	filter_value/2,
	update/3
]).

copy_elements(DestTuple, SrcTuple) ->
	DestList = tuple_to_list(DestTuple),
	SrcList = tuple_to_list(SrcTuple),
	List = util_list:copy_elements(DestList, SrcList),
	list_to_tuple(List).

update([], _FunMap, Tuple) ->
	Tuple;
update([{K, V} | T], FunMap, Tuple) ->
	List = FunMap(K),
	UpdateList = make_update_list(List, Tuple, []),
	NewTuple = update1(UpdateList, V),
	update(T, FunMap, NewTuple).

update1([], V) ->
	V;
update1([{K, R} | T], V) ->
	NV = setelement(K, R, V),
	update1(T, NV).

make_update_list([], _R, List) ->
	List;
make_update_list([K | T], R, List) ->
	NR = element(K, R),
	make_update_list(T, NR, [{K, R} | List]).

update(Tuple, UpList) ->
	lists:foldl(fun({Index, Value}, TupleAcc) ->
		setelement(Index, TupleAcc, Value)
	end, Tuple, UpList).

update_counter(Tuple, UpList) ->
	lists:foldl(fun({Index, Change}, TupleAcc) ->
		Value = element(Index, TupleAcc),
		setelement(Index, TupleAcc, Value + Change)
	end, Tuple, UpList).

filter_value(Tuple, Value) ->
	List = tuple_to_list(Tuple),
	ListNew = lists:map(fun(Val) ->
		case Val == Value of
			true -> undefined;
			false -> Val
		end
	end, List),
	list_to_tuple(ListNew).

