-module(utStrace3).

-export([test/1]).

test(Data) ->
   utStrace4:test(Data),
   {ok, Data}.

