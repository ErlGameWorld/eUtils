-module(eLfq).

-on_load(init/0).

-define(NotLoaded, erlang:nif_error({nif_not_loaded, module, ?MODULE, line, ?LINE})).

-export([
   % create the queue
   new/0

   % delete the queue
   , del/1

   % Allocates more memory if necessary
   , in/2                         % (item) : bool
   , ins/2                        % (item_first, count) : bool

   % Attempts to dequeue from the queue (never allocates)
   , tryOut/1                     % (item&) : bool
   , tryOuts/2                    % (item_first, max) : size_t

   % A not-necessarily-accurate count of the total number of elements
   , size/1

]).

-spec init() -> ok | {error, {Reason :: load_failed | bad_lib | load | reload | upgrade | old_code, Text :: string()}}.
init() ->
   case code:priv_dir(?MODULE) of
      {error, _} ->
         case code:which(?MODULE) of
            Filename when is_list(Filename) ->
               SoName = filename:join([filename:dirname(Filename), "../priv", atom_to_list(?MODULE)]);
            _ ->
               SoName = filename:join("../priv", atom_to_list(?MODULE))
         end;
      Dir ->
         SoName = filename:join(Dir, atom_to_list(?MODULE))
   end,
   erlang:load_nif(SoName, 0).

-spec new() -> {ok, QueueRef :: reference()} | error.
new() ->
   ?NotLoaded.

-spec del(QueueRef :: reference()) -> ok.
del(_QueueRef) ->
   ?NotLoaded.

-spec in(QueueRef :: reference(), Data :: any()) -> true | false.
in(_QueueRef, _Data) ->
   ?NotLoaded.

-spec ins(QueueRef :: reference(), DataList :: [any()]) -> true | false.
ins(_QueueRef, _DataList) ->
   ?NotLoaded.

-spec tryOut(QueueRef :: reference()) -> Data :: any() | lfq_empty | lfq_error.
tryOut(_QueueRef) ->
   ?NotLoaded.

-spec tryOuts(QueueRef :: reference(), Cnt :: pos_integer()) -> DataList :: [any()].
tryOuts(_QueueRef, _Cnt) ->
   ?NotLoaded.

-spec size(QueueRef :: reference()) -> pos_integer().
size(_QueueRef) ->
   ?NotLoaded.