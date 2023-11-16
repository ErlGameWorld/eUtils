-module(utSocket).

-export([
   socketToIpStr/1
   , socketToIpTuple/1
   , socket2port/1
   , str2ip/1
   , ipToStr/1
   , getOpts/1
]).


%% socket转IP
socketToIpStr(Socket) ->
   try {ok, {TIp, _TPort}} = inet:peername(Socket), ipToStr(TIp)
   catch _:_ ->
      try {ok, {SIp, _SPort}} = ssl:peername(Socket), ipToStr(SIp)
      catch _:_ ->
         "0.0.0.0"
      end
   end.

ipToStr({A, B, C, D}) ->
   <<_:8, IpStr>> = <<<<".", (integer_to_binary(O))/binary>> || O <- [A, B, C, D]>>,
   IpStr;
ipToStr(Ip) ->
   <<_:8, IpStr>> = <<<<":", (integer_to_binary(O, 16))/binary>> || O <- tuple_to_list(Ip)>>,
   IpStr.

%% 获取ip
socketToIpTuple(Socket) ->
   try {ok, {TIp, _TPort}} = inet:peername(Socket), TIp
   catch _:_ ->
      try {ok, {SIp, _SPort}} = ssl:peername(Socket), SIp
      catch _:_ ->
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

getOpts(Socket) ->
   inet:getopts(Socket, [active, broadcast, buffer, delay_send, dontroute, exit_on_close, header, high_watermark, ipv6_v6only, keepalive, linger, low_watermark, mode, nodelay, packet, packet_size, priority, read_packets, recbuf, reuseaddr, send_timeout, sndbuf]).
   