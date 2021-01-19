-module(nifArray).

-on_load(init/0).

-export([
   new/1
   , get/2
   , put/3
   , test/1
   , test1/1
]).

-type nifArray() :: reference().

init() ->
   SoName =
      case code:priv_dir(?MODULE) of
         {error, _} ->
            case code:which(?MODULE) of
               Filename when is_list(Filename) ->
                  filename:join([filename:dirname(Filename), "../priv", "nifArray"]);
               _ ->
                  filename:join("../priv", "nifArray")
            end;
         Dir ->
            filename:join(Dir, "nifArray")
      end,
   erlang:load_nif(SoName, 0).

-spec new(Size :: integer()) -> nifArray().
new(_Size) ->
   erlang:nif_error({nif_not_loaded, module, ?MODULE, line, ?LINE}).

-spec get(Ref :: nifArray(), Index :: integer()) -> term().
get(_Ref, _Index) ->
   erlang:nif_error({nif_not_loaded, module, ?MODULE, line, ?LINE}).

-spec put(Ref :: nifArray(), Index :: integer(), Value :: term()) -> term().
put(_Ref, _Index, _Value) ->
   erlang:nif_error({nif_not_loaded, module, ?MODULE, line, ?LINE}).

test(Value) ->
   erlang:nif_error({nif_not_loaded, module, ?MODULE, line, ?LINE}).

test1(Value) ->
   Bin = term_to_binary(Value),
   Term = binary_to_term(Bin).


