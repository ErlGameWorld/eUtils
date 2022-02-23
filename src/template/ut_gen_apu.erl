-module(ut_gen_apu).
-behaviour(ut_gen_apu).

-compile(inline).
-compile({inline_size, 128}).

%% EXPORT API
-export([
   start_link/0
   , xxxCall/3
   , xxxCast/2
]).

-export([
   init/1
   , handleInfo/2
   , terminate/2
   , code_change/3
]).

-define(SERVER, ?MODULE).
-record(state, {}).

-define(ERR, io:format).

%% ********************************************  API *******************************************************************
start_link() ->
   ut_gen_apu:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ********************************************  callback **************************************************************
init(_Args) ->
   {ok, #state{}}.

xxxCall({xxxCall, Arg1, Arg2}, _State, _From) ->
   {Arg1, Arg2},
   {reply, ok}.

xxxCast({xxxCast, Arg1, Arg2}, _State) ->
   {Arg1, Arg2},
   kpS.

handleInfo(_Msg, _State) ->
   ?ERR("~p info receive unexpect msg ~p ~n ", [?MODULE, _Msg]),
   kpS.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.
%% ****************************************************** logic ********************************************************



