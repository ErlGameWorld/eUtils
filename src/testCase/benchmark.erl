-module(benchmark).

-export([t1/0,t2/0, t3/0]).

t1() ->
   DateTime = {date(),time()},
   Fun = fun(_) -> erlang:universaltime_to_posixtime(DateTime) end,
   timer:tc(lists,foreach,[Fun,lists:seq(1, 1000000)]).

t3() ->
   DateTime = {date(),time()},
   Fun = fun(_) -> erlang:localtime_to_universaltime(DateTime) end,
   timer:tc(lists,foreach,[Fun,lists:seq(1, 1000000)]).

t2() ->
   DateTime = {date(),time()},
   TimeZone = 8,
   Fun = fun(_) -> calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200 - TimeZone * 3600 end,
   timer:tc(lists,foreach,[Fun,lists:seq(1, 1000000)]).