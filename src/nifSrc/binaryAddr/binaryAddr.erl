-module(binaryAddr).

-export([getBinAddr/1]).

-on_load(init/0).

init() ->
   SoName =
      case code:priv_dir(?MODULE) of
         {error, _} ->
            case code:which(?MODULE) of
               Filename when is_list(Filename) ->
                  filename:join([filename:dirname(Filename), "../priv", "binaryAddr"]);
               _ ->
                  filename:join("../priv", "binaryAddr")
            end;
         Dir ->
            filename:join(Dir, "binaryAddr")
      end,
   erlang:load_nif(SoName, 0).

getBinAddr(_Bin) ->
   erlang:error({"NIF not implemented in nif_test at line", ?LINE}).