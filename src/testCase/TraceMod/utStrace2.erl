-module(utStrace2).

-export([test/1]).

test(Data) ->
   utStrace3:test(Data),
   {ok, Data}.

