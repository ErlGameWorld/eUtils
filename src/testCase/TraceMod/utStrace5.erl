-module(utStrace5).

-export([test/1]).

test(Data) ->
   timer:sleep(30),
   utStrace1:test(Data),
   {ok, Data}.

