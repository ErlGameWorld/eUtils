-module(utArray).

%% 基于tuple封装的数组
%% 基于性能测试结果和出于性能考虑数组大小不要超过128
-import(erlang, [make_tuple/2, make_tuple/3]).
-export([
   new/1
   , new/2
   , new/3
   , set/3
   , get/2
   , size/1
]).

new(Size) ->
   erlang:make_tuple(Size, undefined).

new(Size, InitialValue) ->
   erlang:make_tuple(Size, InitialValue).

new(Size, DefaultValue, InitList) ->
   erlang:make_tuple(Size, DefaultValue, InitList).

set(Idx, Value, UtArray) ->
   erlang:setelement(Idx, UtArray, Value).

get(Idx, UtArray) ->
   erlang:element(Idx, UtArray).

size(UtArray) ->
   erlang:tuple_size(UtArray).

