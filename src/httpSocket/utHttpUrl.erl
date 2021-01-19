-module(utHttpUrl).

%% API
-export([]).
-export([
   url_encode_request_body/1,
   url_encode/1,
   integer_to_hex/1,
   get_ip_from_headers/2,
   get_x_real_ip/1
]).


url_encode_request_body(Args) ->
   lists:concat(
      lists:foldl(
         fun(Rec, []) -> [Rec]; (Rec, Ac) -> [Rec, "&" | Ac] end,
         [],
         [K ++ "=" ++ url_encode(V) || {K, V} <- Args]
      )
   ).


%% encode url params
url_encode(T) when is_binary(T) ->
   binary_to_list(T);
url_encode([H | T]) ->
   if
      H >= $a, $z >= H ->
         [H | url_encode(T)];
      H >= $A, $Z >= H ->
         [H | url_encode(T)];
      H >= $0, $9 >= H ->
         [H | url_encode(T)];
      H == $_; H == $.; H == $-; H == $/; H == $: -> % FIXME: more..
         [H | url_encode(T)];
      true ->
         case integer_to_hex(H) of
            [X, Y] ->
               [$%, X, Y | url_encode(T)];
            [X] ->
               [$%, $0, X | url_encode(T)]
         end
   end;

url_encode([]) -> [].

integer_to_hex(I) ->
   case catch erlang:integer_to_list(I, 16) of
      {'EXIT', _} ->
         old_integer_to_hex(I);
      Int ->
         Int
   end.
old_integer_to_hex(I) when I < 10 ->
   integer_to_list(I);
old_integer_to_hex(I) when I < 16 ->
   [I - 10 + $A];
old_integer_to_hex(I) when I >= 16 ->
   N = trunc(I / 16),
   old_integer_to_hex(N) ++ old_integer_to_hex(I rem 16).

get_ip_from_headers(PeerIp, Headers) ->
   RealIp = proplists:get_value(<<"x-real-ip">>, Headers),
   ForwardedForRaw = proplists:get_value(<<"x-forwarded-for">>, Headers),

   ForwardedFor = case ForwardedForRaw of
                     undefined -> undefined;
                     ForwardedForRaw ->
                        case re:run(ForwardedForRaw, "^(?<first_ip>[^\\,]+)",
                           [{capture, [first_ip], binary}]) of
                           {match, [FirstIp]} -> FirstIp;
                           _Any -> undefined
                        end
                  end,
   {ok, PeerAddr} = if
                       is_binary(RealIp) -> inet_parse:address(binary_to_list(RealIp));
                       is_binary(ForwardedFor) -> inet_parse:address(binary_to_list(ForwardedFor));
                       true -> {ok, PeerIp}
                    end,
   PeerAddr.




get_x_real_ip(Headers) ->
   RealIp = proplists:get_value(<<"x-real-ip">>, Headers),
   RealIp.