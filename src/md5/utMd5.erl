-module(utMd5).

-export([
   getMd5/1                %% 获取md5码
   , getMd5Hex/1           %% 获取hex str格式的Md5码
   , getMd5HexBin/1        %% 获取hex binary 格式的Md5码
   , md5BinToHex/1         %% md5 to hex str
   , md5BinToHexBin/1      %% %% md5 to hex binary
   , hexToMd5Bin/1         %% hex str to md5
   , hexBinToMd5Bin/1      %% %% hex binary to md5
]).

-spec getMd5(Str :: iodata()) -> binary().
getMd5(Str) ->
   erlang:md5(Str).

-spec getMd5Hex(Str :: iodata()) -> string().
getMd5Hex(Str) ->
   utHex:binaryToHex(erlang:md5(Str)).

-spec getMd5HexBin(Str :: iodata()) -> binary().
getMd5HexBin(Str) ->
   utHex:binaryToHexBin(erlang:md5(Str)).

-spec(md5BinToHex(Md5Bin :: binary()) -> string()).
md5BinToHex(Md5Bin) ->
   utHex:binaryToHex(Md5Bin).

-spec(md5BinToHexBin(Md5Bin :: binary()) -> binary()).
md5BinToHexBin(Md5Bin) ->
   utHex:binaryToHexBin(Md5Bin).

-spec(hexToMd5Bin(Md5Hex :: string()) -> binary()).
hexToMd5Bin(Md5Hex) ->
   list_to_binary(utHex:hexToBinary(Md5Hex)).

-spec(hexBinToMd5Bin(Md5HexBin :: binary()) -> binary()).
hexBinToMd5Bin(Md5HexBin) ->
   utHex:hexBinToBinary(Md5HexBin).

