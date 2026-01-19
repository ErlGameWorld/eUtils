-module(utBinary).

-export([
	reverse/1
]).

%% 反转binary 这种效率高
reverse(Bin) ->
	Size = byte_size(Bin) * 8,
	<<X:Size/integer-little>> = Bin,
	<<X:Size/integer-big>>.
