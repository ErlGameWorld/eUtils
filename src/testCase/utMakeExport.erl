-module(utMakeExport).

-export([makeExport/1]).

makeExport(SrcFile) ->
   case file:open(SrcFile, [read, binary]) of
      {ok, IoDevice} ->
         BinStr = doMathEveryLine(IoDevice, "", <<>>),
         file:close(IoDevice),
         file:write_file("export.config", BinStr);
      _ ->
         false
   end.

doMathEveryLine(IoDevice, Comment, BinStr) ->
   case file:read_line(IoDevice) of
      {ok, Data} ->
         case re:run(Data, "%%") of
            {match, _} ->
               doMathEveryLine(IoDevice, Data, BinStr);
            _ ->
               case re:run(Data, "-spec") of
                  {match, _} ->
                     {ok, DataFun} = file:read_line(IoDevice),
                     DataFunStr = binary:replace(DataFun, <<"->\n">>, <<"">>),
                     doMathEveryLine(IoDevice, "", <<BinStr/binary, DataFunStr/binary, "\t\t\t\t", Comment/binary>>);
                  _ ->
                     doMathEveryLine(IoDevice, Comment, BinStr)
               end
         end;
      _ ->
         BinStr
   end.
