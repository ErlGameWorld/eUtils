-module(utStrace5).

-export([test/1]).

test(Data) ->
   %% 这段代码用来测试 try catch 堆栈
   utTryCatchCase:makeException2(Data).

   %% 这段代码用来测试非尾递归堆栈大小模拟测试
   %% timer:sleep(30),
   %% utStrace1:test(Data),
   %% {ok, Data}.


