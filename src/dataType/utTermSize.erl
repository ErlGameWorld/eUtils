-module(utTermSize).

-export([
   byteSize/1
   , byteSize/2
]).

-define(HEAP_BINARY_LIMIT, 64).

byteSize(Term) ->
   byteSize(Term, erlang:system_info(wordsize)).

byteSize(Term, WordSize) ->
   byteSizeTermLocal(Term, WordSize) + byteSizeTerms(Term).

byteSizeTerms(Term) when is_list(Term) ->
   byteSizeTermsInList(Term, 0);
byteSizeTerms(Term) when is_tuple(Term) ->
   if
      Term == {} ->
         0;
      true ->
         byteSizeTermsInTuple(1, erlang:tuple_size(Term), Term, 0)
   end;
byteSizeTerms(Term) when is_map(Term) ->
   maps:fold(
      fun(K, V, Bytes) ->
         byteSizeTerms(K) + byteSizeTerms(V) + Bytes
      end, 0, Term);
byteSizeTerms(Term) ->
   byteSizeTerm(Term).

byteSizeTermsInList([], SumSize) ->
   SumSize;
byteSizeTermsInList([Term | L], SumSize) ->
   TermSize = byteSizeTerms(Term),
   byteSizeTermsInList(L, TermSize + SumSize);
byteSizeTermsInList(Term, SumSize) ->
   % element of improper list
   byteSizeTerms(Term) + SumSize.

byteSizeTermsInTuple(Size, Size, Term, SumSize) ->
   byteSizeTerms(erlang:element(Size, Term)) + SumSize;
byteSizeTermsInTuple(I, Size, Term, SumSize) ->
   TermSize = byteSizeTerms(erlang:element(I, Term)),
   byteSizeTermsInTuple(I + 1, Size, Term, TermSize + SumSize).

byteSizeTerm(Term) ->
   byteSizeTermGlobal(Term).

byteSizeTermLocal(Term, WordSize) ->
   % stack/register size + heap size
   (1 + erts_debug:flat_size(Term)) * WordSize.

byteSizeTermGlobal(Term) when is_binary(Term) ->
   % global data storage within allocators
   BinarySize = erlang:byte_size(Term),
   if
      BinarySize > ?HEAP_BINARY_LIMIT ->
         % refc binary
         BinarySize;
      true ->
         % heap binary
         0
   end;
byteSizeTermGlobal(_) ->
   0.

-define(TEST, 64).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

internal_test() ->
   RefcBinary = <<1:((?HEAP_BINARY_LIMIT + 1) * 8)>>,
   HeapBinary = <<1:(?HEAP_BINARY_LIMIT * 8)>>,
   true = (7 == (1 + erts_debug:flat_size(RefcBinary))),
   % doesn't work in console shell
   % (process heap size of binary is excluded
   %  when executed in the console shell)
   true = (11 == (1 + erts_debug:flat_size(HeapBinary))),
   true = (4 == (1 + erts_debug:flat_size(<<1:8>>))),

   24 = byteSize(<<>>, 8),
   32 = byteSize(<<"abc">>, 8),
   32 = byteSize(<<$a, $b, $c>>, 8),
   8 = byteSize([], 8),
   24 = byteSize([0 | []], 8),
   24 = byteSize([1 | 2], 8), % itime_tmproper list
   16 = byteSize({}, 8),
   24 = byteSize({0}, 8),
   8 = byteSize(0, 8),
   8 = byteSize(erlang:self(), 8),
   8 = byteSize(atom, 8),
   88 = byteSize(#{1=>1, 2=>2, 3=>3}, 8),
   136 = byteSize(#{1=>RefcBinary, 2=>2, 3=>3}, 8) - erlang:byte_size(RefcBinary),
   ok.

-endif.

