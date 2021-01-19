-module(utSocket).

-export([
   socketToIpStr/1
   , socketToIpTuple/1
   , socket2port/1
   , str2ip/1
]).


%% socket转IP
socketToIpStr(Socket) ->
   case inet:peername(Socket) of
      {ok, {{A, B, C, D}, _}} ->
         string:join(lists:map(fun com_type:to_list/1, [A, B, C, D]), ".");
      _ ->
         ""
   end.

%% 获取ip
socketToIpTuple(Socket) ->
   case catch inet:peername(Socket) of
      {ok, {Ip, _Port}} ->
         Ip;
      _ ->
         case catch ssl:peername(Socket) of
            {ok, {Ip, _Port}} ->
               Ip;
            _ ->
               {0, 0, 0, 0}
         end
   end.


socket2port(Socket) ->
   case inet:peername(Socket) of
      {ok, {_, Port}} -> Port;
      _ -> 0
   end.

str2ip(IP) when is_list(IP) ->
   [A1, A2, A3, A4] = string:tokens(IP, "."),
   {list_to_integer(A1), list_to_integer(A2), list_to_integer(A3), list_to_integer(A4)};
str2ip(IP) when is_tuple(IP) ->
   IP.