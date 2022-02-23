-module(eLfq).

-on_load(init/0).

-define(NotLoaded, erlang:nif_error({nif_not_loaded, module, ?MODULE, line, ?LINE})).

-export([
   % create the queue
   new/0

   % Allocates more memory if necessary
   , in/2                         % (item) : bool
   , in/3                         % (prod_token, item) : bool
   , ins/2                        % (item_first, count) : bool
   , ins/3                        % (prod_token, item_first, count) : bool

   % Fails if not enough memory to enqueue
   , tryIn/2                      % (item) : bool
   , tryIn/3                      % (prod_token, item) : bool
   , tryIns/2                     % (item_first, count) : bool
   , tryIns/3                     % (prod_token, item_first, count) : bool

   % Attempts to dequeue from the queue (never allocates)
   , tryOut/1                     % (item&) : bool
   , tryOut/2                     % (cons_token, item&) : bool
   , tryOuts/2                    % (item_first, max) : size_t
   , tryOuts/3                    % (cons_token, item_first, max) : size_t

   % If you happen to know which producer you want to dequeue from
   , tryOutByProd/2                 % (prod_token, item&) : bool
   , tryOutsByProd/3                % (prod_token, item_first, max) : size_t

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

-spec new() -> {ok, QueueRef :: reference()} | badarg | {error, Reason :: binary()}.
new() ->
   ?NotLoaded.

-spec in(QueueRef :: reference(), Data :: any()) -> true | {error, Reason :: binary()}.
in(_QueueRef, _Data) ->
   ?NotLoaded.

-spec in(QueueRef :: reference(), ProdToken :: any(), Data :: any()) -> true | {error, Reason :: binary()}.
in(_QueueRef, _ProdToken, _Data) ->
   ?NotLoaded.

-spec ins(QueueRef :: reference(), DataList :: [any()]) -> true | {error, Reason :: binary()}.
ins(_QueueRef, _DataList) ->
   ?NotLoaded.

-spec ins(QueueRef :: reference(), ProdToken :: any(), DataList :: [any()]) -> true | {error, Reason :: binary()}.
ins(_QueueRef, _ProdToken, _DataList) ->
   ?NotLoaded.

-spec tryIn(QueueRef :: reference(), Data :: any()) -> true | {error, Reason :: binary()}.
tryIn(_QueueRef, _Data) ->
   ?NotLoaded.

-spec tryIn(QueueRef :: reference(), ProdToken :: any(), Data :: any()) -> true | {error, Reason :: binary()}.
tryIn(_QueueRef, _ProdToken, _Data) ->
   ?NotLoaded.

-spec tryIns(QueueRef :: reference(), DataList :: [any()]) -> true | {error, Reason :: binary()}.
tryIns(_QueueRef, _DataList) ->
   ?NotLoaded.

-spec tryIns(QueueRef :: reference(), ProdToken :: any(), DataList :: [any()]) -> true | {error, Reason :: binary()}.
tryIns(_QueueRef, _ProdToken, _DataList) ->
   ?NotLoaded.

-spec tryOut(QueueRef :: reference()) -> Data :: any() | {error, Reason :: binary()}.
tryOut(_QueueRef) ->
   ?NotLoaded.

-spec tryOut(QueueRef :: reference(), ConsToken :: any()) -> Data :: any() | {error, Reason :: binary()}.
tryOut(_QueueRef, _ConsToken) ->
   ?NotLoaded.

-spec tryOuts(QueueRef :: reference(), Cnt :: pos_integer()) -> DataList :: [any()] | {error, Reason :: binary()}.
tryOuts(_QueueRef, _Cnt) ->
   ?NotLoaded.

-spec tryOuts(QueueRef :: reference(), ConsToken :: any(), Cnt :: pos_integer()) -> DataList :: [any()] | {error, Reason :: binary()}.
tryOuts(_QueueRef, _ConsToken, _Cnt) ->
   ?NotLoaded.

-spec tryOutByProd(QueueRef :: reference(), ProdToken :: any()) -> Data :: any() | {error, Reason :: binary()}.
tryOutByProd(_QueueRef, _ProdToken) ->
   ?NotLoaded.

-spec tryOutsByProd(QueueRef :: reference(), ProdToken :: any(), Cnt :: pos_integer()) -> DataList :: [any()] | {error, Reason :: binary()}.
tryOutsByProd(_QueueRef, _ProdToken, _Cnt) ->
   ?NotLoaded.

-spec size(QueueRef :: reference()) -> pos_integer() | {error, Reason :: binary()}.
size(_QueueRef) ->
   ?NotLoaded.
