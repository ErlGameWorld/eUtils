-module(utStrace4).

-export([test/1]).

test(Data) ->
   utStrace5:test(Data),
   {ok, Data}.

