-module(testQueue).

-compile([export_all]).

initFq(Name) ->
   fwQueue:new(Name).

insertFq(Name) ->
   fwQueue:in(Name, {ttttt, 13421324, <<"fdsafsdffds">>}).

readFq(Name) ->
   fwQueue:outF(Name).

initShq() ->
   {ok, Pid} = shq:start(),
   Pid.

insertShq(Pid) ->
   shq:in(Pid, {ttttt, 13421324, <<"fdsafsdffds">>}).

readShq(Pid) ->
   shq:out(Pid).

initShq2() ->
   {ok, Pid} = shq2:start(),
   Pid.

insertShq2(Pid) ->
   shq2:in(Pid, {ttttt, 13421324, <<"fdsafsdffds">>}).

readShq2(Pid) ->
   shq2:out(Pid).
