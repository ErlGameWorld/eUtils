%%%-------------------------------------------------------------------
%%% @author fox
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%% nif
%%% @end
%%% Created : 16. 十月 2017 16:05
%%%-------------------------------------------------------------------
-module(nif).
-author("fox").

%% API
-export([init/0, test/0]).

init() ->
   skiplist:init("./nif_skiplist"),
   ok.

test() ->
   init(),
   A = skiplist:new(),
   skiplist:insert(A, 10, 10),
   skiplist:insert(A, 5, 5),
   skiplist:insert(A, 8, 8),
   skiplist:insert(A, 1, 1),
   skiplist:insert(A, 4, 4),
   skiplist:insert(A, 7, 7),
   io:format("~p~n", [skiplist:to_list(A)]),

   skiplist:delete(A, 7, 7),
   io:format("~p~n", [skiplist:to_list(A)]),

   io:format("get=~p~n", [skiplist:index_of_score(A, 5)]),
   io:format("get=~p~n", [skiplist:index_of_score(A, 6)]),
   io:format("get=~p~n", [skiplist:index_of_score(A, 15)]),

   io:format("at=~p~n", [skiplist:at(A, 1)]),
   io:format("at=~p~n", [skiplist:at(A, 5)]),
   io:format("at=~p~n", [skiplist:at(A, 6)]),

   io:format("range=~p~n", [skiplist:range(A, 1, 10)]),
   io:format("range=~p~n", [skiplist:range(A, 2, 3)]),
   io:format("range=~p~n", [skiplist:range(A, 8, 10)]),

   io:format("range_with_score=~p~n", [skiplist:range_with_score(A, 1, 10)]),
   io:format("range_with_score=~p~n", [skiplist:range_with_score(A, 2, 3)]),
   io:format("range_with_score=~p~n", [skiplist:range_with_score(A, 8, 10)]),

   erlang:statistics(wall_clock),
   loop_test(30 * 1000, A),
   {_, Time} = erlang:statistics(wall_clock),
   io:format("time=~p~n", [Time]),

%%    io:format("range_by_score=~p~n", [skiplist:range_by_score(A, -10, 10)]),
%%    io:format("range_by_score=~p~n", [skiplist:range_by_score(A, 2, 3)]),
   io:format("range_by_score=~p~n", [skiplist:range_by_score(A, 101, 10)]),

   skiplist:free(A).

loop_test(0, _) ->
   ok;
loop_test(N, L) ->
   case rand:uniform(500) < 300 of
      true ->
         skiplist:insert(L, rand:uniform(100), rand:uniform(20));
      _ ->
         skiplist:delete(L, rand:uniform(100), rand:uniform(20))
   end,
   loop_test(N - 1, L).

