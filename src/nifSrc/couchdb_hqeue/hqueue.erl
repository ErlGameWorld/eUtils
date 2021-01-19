% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http:%www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(hqueue).


-on_load(init/0).


-export([
   new/0,
   new/1,

   extract_max/1,
   insert/3,

   from_list/1,
   from_list/2,
   to_list/1,

   heap_size/1,
   info/1,
   is_empty/1,
   max_elems/1,
   size/1,

   resize_heap/2,
   scale_by/2,
   set_max_elems/2
]).


-define(NOT_LOADED, not_loaded(?LINE)).


-type hqueue() :: term().
-type hqueue_priority() :: float(). %% this should be non_neg_float()
-type hqueue_val() :: term().
-type hqueue_elem() :: {hqueue_priority(), hqueue_val()}.
-type hqueue_option() :: {max_elems, pos_integer()}
| {heap_size, pos_integer()}.
-type hqueue_stat() :: {max_elems, pos_integer()}
| {heap_size, pos_integer()}
| {size, non_neg_integer()}.

-export_type([hqueue/0]).


-spec new() -> {ok, hqueue()}.
new() ->
   new([]).


-spec new([hqueue_option()]) -> {ok, hqueue()}.
new(_Options) ->
   ?NOT_LOADED.


%% Extraction order is undefined for entries with duplicate priorities
-spec extract_max(hqueue()) -> hqueue_elem() | {error, empty}.
extract_max(_HQ) ->
   ?NOT_LOADED.


-spec insert(hqueue(), hqueue_priority(), hqueue_val()) -> ok | {error, full}.
insert(_HQ, _Priority, _Val) ->
   ?NOT_LOADED.


-spec size(hqueue()) -> integer().
size(_HQ) ->
   ?NOT_LOADED.


-spec max_elems(hqueue()) -> integer().
max_elems(_HQ) ->
   ?NOT_LOADED.


%% Returns old max elems or error if NewMaxElems < size(HQ)
-spec set_max_elems(hqueue(), pos_integer()) -> pos_integer()
| {error, too_small}.
set_max_elems(_HQ, _NewMaxElems) ->
   ?NOT_LOADED.


-spec is_empty(hqueue()) -> boolean().
is_empty(HQ) ->
   hqueue:size(HQ) =:= 0.


-spec to_list(hqueue()) -> [hqueue_elem()].
to_list(_HQ) ->
   ?NOT_LOADED.


-spec from_list([hqueue_elem()]) -> {ok, hqueue()}.
from_list(Elems) ->
   from_list(Elems, []).


-spec from_list([hqueue_elem()], [hqueue_option()]) -> {ok, hqueue()}.
from_list(Elems, Options) ->
   {ok, HQ} = ?MODULE:new(Options),
   lists:foreach(fun({Priority, Val}) ->
      ?MODULE:insert(HQ, Priority, Val)
                 end, Elems),
   {ok, HQ}.


-spec scale_by(hqueue(), float()) -> ok.
scale_by(_HQ, _Factor) ->
   ?NOT_LOADED.


%% Returns old heap size or error if NewHeapSize < size(HQ)
-spec resize_heap(hqueue(), pos_integer()) -> pos_integer()
| {error, too_small}.
resize_heap(_HQ, _NewHeapSize) ->
   ?NOT_LOADED.


-spec heap_size(hqueue()) -> pos_integer().
heap_size(_HQ) ->
   ?NOT_LOADED.


-spec info(hqueue()) -> [hqueue_stat()].
info(HQ) ->
   [
      {heap_size, hqueue:heap_size(HQ)},
      {max_elems, hqueue:max_elems(HQ)},
      {size, hqueue:size(HQ)}
   ].



init() ->
   PrivDir = case code:priv_dir(?MODULE) of
                {error, _} ->
                   EbinDir = filename:dirname(code:which(?MODULE)),
                   AppPath = filename:dirname(EbinDir),
                   filename:join(AppPath, "priv");
                Path ->
                   Path
             end,
   erlang:load_nif(filename:join(PrivDir, "hqueue"), 0).


not_loaded(Line) ->
   erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).
