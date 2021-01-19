-module(utStrace1).

-export([test/1]).

test(Data) ->
   utStrace2:test(Data),
   {ok, Data}.

