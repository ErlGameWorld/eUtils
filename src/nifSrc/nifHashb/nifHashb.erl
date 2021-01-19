-module(nifHashb).

-on_load(init/0).

-export([
   new/0
   , get/2
   , put/3
   , hash1/2
   , hash2/2
   , hash3/2
   , cb1/2
   , cb2/2
   , compareBin1/2
   , compareBin2/2
]).

init() ->
   SoName =
      case code:priv_dir(?MODULE) of
         {error, _} ->
            case code:which(?MODULE) of
               Filename when is_list(Filename) ->
                  filename:join([filename:dirname(Filename), "../priv", "nifHashb"]);
               _ ->
                  filename:join("../priv", "nifHashb")
            end;
         Dir ->
            filename:join(Dir, "nifHashb")
      end,
   erlang:load_nif(SoName, 0).

new() ->
   erlang:nif_error({nif_not_loaded, module, ?MODULE, line, ?LINE}).

get(Ref, Key) ->
   erlang:nif_error({nif_not_loaded, module, ?MODULE, line, ?LINE}).

put(Ref, Key, Value) ->
   erlang:nif_error({nif_not_loaded, module, ?MODULE, line, ?LINE}).

hash1(Term, Range) ->
   erlang:nif_error({nif_not_loaded, module, ?MODULE, line, ?LINE}).

hash2(Term, Range) ->
   erlang:nif_error({nif_not_loaded, module, ?MODULE, line, ?LINE}).

hash3(Term, Range) ->
   erlang:nif_error({nif_not_loaded, module, ?MODULE, line, ?LINE}).

cb1(Term1, Term2) ->
   compareBin1(term_to_binary(Term1), term_to_binary(Term2)).

cb2(Term1, Term2) ->
   compareBin2(term_to_binary(Term1), term_to_binary(Term2)).

compareBin1(Bin1, Bin2) ->
   erlang:nif_error({nif_not_loaded, module, ?MODULE, line, ?LINE}).

compareBin2(Bin1, Bin2) ->
   erlang:nif_error({nif_not_loaded, module, ?MODULE, line, ?LINE}).



