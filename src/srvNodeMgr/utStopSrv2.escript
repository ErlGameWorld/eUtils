#!/usr/bin/env escript
%% -*- erlang -*
%%! -name stop-source-node@127.0.0.1 -setcookie 123 -hidden

-define(AIM_NODE, 'stopNode@127.0.0.1').

main(_Args) ->
   io:format("IMY***************** cuer node info ~p ~p ~n", [node(), erlang:get_cookie()]),
   case net_adm:ping(?AIM_NODE) of
      pong ->
         io:format("the aims server:~p is online~n", [?AIM_NODE]),
         io:format("to close it~n"),
         timer:sleep(10000),
         rpc:call(?AIM_NODE, init, stop, []),
         halt(0);
      _ ->
         io:format("the aims server:~p is online~n", [?AIM_NODE]),
         halt(1)
   end.
