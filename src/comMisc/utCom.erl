-module(utCom).

-export([
	cancel_timer/1
]).

%% 取消定时器，返回integer或false
cancel_timer(TimerRef) ->
	is_reference(TimerRef) andalso erlang:cancel_timer(TimerRef).