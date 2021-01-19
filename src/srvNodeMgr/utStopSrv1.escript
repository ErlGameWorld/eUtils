#!/usr/bin/env escript

-define(AIM_NODE, 'stopNode@127.0.0.1').
-define(CUR_NODE, 'stop-source-node@127.0.0.1').          %% 这个参数可以在上面直接设置
-define(COOKIE, '123').                             %% 这个参数可以在上面直接设置

main(_Arge) ->
   net_kernel:start([?CUR_NODE, longnames]),
   erlang:set_cookie(?CUR_NODE, ?COOKIE),
   io:format("IMY***************** cuer node info ~p ~p ~n", [node(), erlang:get_cookie()]),
   case net_adm:ping(?AIM_NODE) of
      pong ->
         io:format("the aims server:~p is online~n", [?AIM_NODE]),
         io:format("to close it~n"),
         rpc:call(?AIM_NODE, init, stop, []),
         halt(0);
      _ ->
         io:format("the aims server:~p is not online~n", [?AIM_NODE]),
         halt(1)
   end.
