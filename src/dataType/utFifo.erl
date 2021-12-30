-module(utFifo).

-export([
   new/1
   , del/1
   , in/2
   , out/1
   , clear/1
   , size/1
]).

-spec new(Name :: atom()) -> ok | name_used.
new(Name) ->
   case ets:info(Name, id) of
      undefined ->
         ets:new(Name, [ordered_set, public, named_table, {write_concurrency, true}]);
      _ ->
         name_used
   end.

-spec del(Name :: atom()) -> ok.
del(Name) ->
   ets:delete(Name).

-spec in(Name :: atom(), Value :: term()) -> true.
in(Name, Value) ->
   ets:insert(Name, {erlang:unique_integer(), Value}).

-spec out(Name :: atom()) -> empty | Value :: term().
out(Name) ->
   do_out(Name).

do_out(Name) ->
   case ets:first(Name) of
      '$end_of_table' ->
         empty;
      Key ->
         case ets:take(Name, Key) of
            [] ->
               do_out(Name);
            [{_, Value}] ->
               Value
         end
   end.

-spec clear(Name :: atom()) -> ok.
clear(Name) ->
   ets:delete_all_objects(Name).

-spec size(Name :: atom()) -> Size :: integer() | undefined.
size(Name) ->
   ets:info(Name, size).

