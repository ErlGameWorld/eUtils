-module(utHttpOn).

%% API
-export([]).

%% API
-export([
   req/6,      %% Post, Put, Delete Only
   req_get/3,  %% Get Only
   url_encode/1
]).

-export([get_unique_ref/1, get_conv_ref/0]).

get_conv_ref() ->
   get_unique_ref(20).
get_unique_ref(Length) ->
   IntPass = random_num(36, Length),
   list_to_binary(lists:flatten(io_lib:format("~.36B", [IntPass]))).

random_num(NumeralSystemBase, Length) ->
   Min = round(math:pow(NumeralSystemBase, Length - 1)),
   Max = round(math:pow(NumeralSystemBase, Length)),
   crypto:rand_uniform(Min, Max).

req(Method, Url, Headers, ContType, Body, HttpOpts)
   when Method == post orelse Method == put orelse Method == delete ->
   HttpClOpts = [{sync, true}, {body_format, binary}],
   Resp = httpc:request(Method, {eu_types:to_list(Url), Headers, ContType, Body}, HttpOpts, HttpClOpts),
   minimize_resp(Resp).

req_get(Url, Headers, HttpOpts) ->
   HttpClOpts = [{sync, true}, {body_format, binary}],
   Resp = httpc:request(get, {eu_types:to_list(Url), Headers}, HttpOpts, HttpClOpts),
   minimize_resp(Resp).

minimize_resp(Resp) ->
   case Resp of
      {ok, {{_NewVrsn, 200, _}, _Headers, RespBody}} ->
         {ok, 200, RespBody};
      {ok, {{_NewVrsn, HttpCode, _}, _Headers, RespBody}} ->
         {error, HttpCode, RespBody};
      Any -> Any
   end.

url_encode(Data) ->
   url_encode(Data, "").

url_encode([], Acc) ->
   Acc;

url_encode([{Key, Value} | R], "") ->
   url_encode(R, edoc_lib:escape_uri(Key) ++ "=" ++ edoc_lib:escape_uri(Value));
url_encode([{Key, Value} | R], Acc) ->
   url_encode(R, Acc ++ "&" ++ edoc_lib:escape_uri(Key) ++ "=" ++ edoc_lib:escape_uri(Value)).