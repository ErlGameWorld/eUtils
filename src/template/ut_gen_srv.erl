-module(ut_gen_srv).
-behaviour(ut_gen_srv).

-compile(inline).
-compile({inline_size, 128}).

%% EXPORT API
-export([
   start_link/0
]).

-export([
   init/1
   , handleCall/3
   , handleCast/2
   , handleInfo/2
   , terminate/2
   , code_change/3
]).

-define(SERVER, ?MODULE).
-record(state, {}).

-define(ERR, io:format).

%% ********************************************  API *******************************************************************
start_link() ->
   ut_gen_srv:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ********************************************  callback **************************************************************
init(_Args) ->
   {ok, #state{}}.

handleCall(_Msg, _State, _FROM) ->
   ?ERR("~p call receive unexpect msg ~p ~n ", [?MODULE, _Msg]),
   {reply, ok}.

%% 默认匹配
handleCast(_Msg, _State) ->
   ?ERR("~p cast receive unexpect msg ~p ~n ", [?MODULE, _Msg]),
   kpS.

handleInfo(_Msg, _State) ->
   ?ERR("~p info receive unexpect msg ~p ~n ", [?MODULE, _Msg]),
   kpS.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.
%% ****************************************************** logic ********************************************************



