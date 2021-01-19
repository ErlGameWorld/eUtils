-module(utTestMap).

-compile([export_all, nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).

t1(N) ->
   Map = #{etime => {make_ref(), test}, stime => {make_ref(), test}, other => [1212, 132, 3242, 768]},
   tt1(N, Map).

tt1(0, Map) ->
   Map;
tt1(N, Map) ->
   Key = etime,
   case Map of
      #{Key := {Ref, Msg}} ->
         _ = {Ref, Msg};
      _ ->
         not_found
   end,
   tt1(N - 1, Map).

t2(N) ->
   Map = #{etime => {make_ref(), test}, stime => {make_ref(), test}, other => [1212, 132, 3242, 768]},
   tt2(N, Map).

tt2(0, Map) ->
   Map;
tt2(N, Map) ->
   case maps:find(etime1, Map) of
      {ok, A} ->
         A;
      _ ->
         not_found
   end,
   tt2(N - 1, Map).

t3(N) ->
   Map = #{etime => {make_ref(), test}, stime => {make_ref(), test}, other => [1212, 132, 3242, 768]},
   tt3(N, Map).

tt3(0, Map) ->
   Map;
tt3(N, Map) ->
   case maps:get(etime, Map, not_found) of
      {_Ref, _Msg} = A ->
         A;
      _ ->
         not_found
   end,
   tt3(N - 1, Map).
