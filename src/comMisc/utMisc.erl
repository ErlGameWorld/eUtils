-module(utMisc).

-compile([export_all, nowarn_export_all]).

%% 日志记录函数
fileLog(T, F, A, Mod, Line) ->
   {ok, Fl} = file:open("logs/error_log.txt", [write, append]),
   Format = list_to_binary("#" ++ T ++ " ~s[~w:~w] " ++ F ++ "\r\n~n"),
   {{Y, M, D}, {H, I, S}} = erlang:localtime(),
   Date = list_to_binary([integer_to_list(Y), "-", integer_to_list(M), "-", integer_to_list(D), " ", integer_to_list(H), ":", integer_to_list(I), ":", integer_to_list(S)]),
   io:format(Fl, unicode:characters_to_list(Format), [Date, Mod, Line] ++ A),
   file:close(Fl).

compile_base_data(Table, ModName, IDPoses) ->
   ModNameString = com_util:term_to_string(ModName),
   HeadString =
      "-module(" ++ ModNameString ++ ").
		-compile(export_all).
		",
   BaseDataList = db_base:select_all(Table, "*", []),
   ContentString =
      lists:foldl(fun(BaseData0, PreString) ->
         FunChange =
            fun(Field) ->
               if is_integer(Field) -> Field;
                  true ->
                     case com_util:bitstring_to_term(Field) of
                        undefined ->
                           Field;
                        Term ->
                           Term
                     end
               end
            end,
         BaseData = [FunChange(Item) || Item <- BaseData0],
         Base = list_to_tuple([Table | BaseData]),
         BaseString = com_util:term_to_string(Base),
         IDs = [element(Pos, Base) || Pos <- IDPoses],
         IDList0 = lists:foldl(fun(ID, PreString2) ->
            IDList =
               if erlang:is_integer(ID) ->
                  integer_to_list(ID);
                  true ->
                     ID
               end,
            PreString2 ++ "," ++ IDList
                               end, [], IDs),
         [_ | IDList] = IDList0,
         PreString ++
            "get(" ++
            IDList ++
            ") ->" ++
            BaseString ++
            ";
            "
                  end
         , "", BaseDataList),

   _List0 = [",_" || _Pos <- IDPoses],
   [_ | _List] = lists:flatten(_List0),
   ErrorString = "get(" ++ _List ++ ") -> undefined.
	",
   FinalString = HeadString ++ ContentString ++ ErrorString,
   %% ?PRINT("string=~s~n",[FinalString]),
   try
      {Mod, Code} = dynamic_compile:from_string(FinalString),
      code:load_binary(Mod, ModNameString ++ ".erl", Code)
   catch
      Type:Error -> io:format("Error compiling (~p): ~p~n", [Type, Error])
   end,
   ok.

%% 显示record
%% 用法 : r2p(#ets_online{...}, record_info(fields, ets_online))
r2p(A, B) -> record_to_proplist(A, B).
record_to_proplist(Record, Fields) ->
   record_to_proplist(Record, Fields, record__).

record_to_proplist(Record, Fields, TypeKey)
   when tuple_size(Record) - 1 =:= length(Fields) ->
   lists:zip([TypeKey | Fields], tuple_to_list(Record)).

%% 角度和cos的转换，cos(60') = 0.5
angle_to_float(Angle) ->
   math:cos(math:pi() * Angle / 180).

%%十进制转到N进制 ex: v10toVn(999999,36,[])
v10toVn(I0, Base, R0) ->
   D = I0 rem Base,
   I1 = I0 div Base,
   R1 = if D >= 10 ->
      [D - 10 + $A | R0];
           true ->
              [D + $0 | R0]
        end,
   if I1 =:= 0 ->
      R1;
      true ->
         v10toVn(I1, Base, R1)
   end.

-define(SOLUT_X, 30).            %% 默认手机分表率X
-define(SOLUT_Y, 20).            %% 默认手机分表率Y

%%--------------------------同屏处理函数---------------------------
%% @spec 相同区域算法,玩家在屏幕的正中央，因此只需要算出该玩家边框的4个坐标点
get_screen(X, Y, SolutX, SolutY) ->
   {HalfScreenWidth, HalfScreenHeight} =
      if
         SolutX =/= 0 andalso SolutY =/= 0 ->
            {util:ceil(SolutX / 2), util:ceil(SolutY / 2)};
         true ->
            {util:ceil(?SOLUT_X / 2), util:ceil(?SOLUT_Y / 2)}
      end,
   {X - HalfScreenWidth, Y - HalfScreenHeight, X + HalfScreenWidth, Y + HalfScreenHeight}.

%% 判断X,Y是否在所在的框里面。
is_same_screen(X, Y, {X1, Y1, X2, Y2}) ->
   X1 =< X andalso X2 >= X andalso Y1 =< Y andalso Y2 >= Y.

%%判断两个坐标是否在同一个屏幕里面
is_same_screen([X1, Y1, X2, Y2], [SolutX, SolutY]) ->
   {ScreenWidth, ScreenHeight} =
      if
         SolutX =/= 0 andalso SolutY =/= 0 ->
            {SolutX, SolutY};
         true ->
            {?SOLUT_X, ?SOLUT_Y}
      end,
%% 	io:format("=====~p:~p~n",[ScreenWidth,ScreenHeight]) ,
   SX1 = X1 div ScreenWidth,
   SY1 = Y1 div ScreenHeight,
   SX2 = X2 div ScreenWidth,
   SY2 = Y2 div ScreenHeight,

   SX1 == SX2 andalso SY1 == SY2.

-define(SLICEWIDTH, 15).
-define(SLICEHEIGHT, 9).

%% 获取九宫格(?SLICEWIDTH*?SLICEHEIGHT)的边界坐标
%% 九宫格的边界各自为屏幕长宽的1/2
get_matrix(X, Y) ->
   X1 = X div ?SLICEWIDTH * ?SLICEWIDTH,
   Y1 = Y div ?SLICEHEIGHT * ?SLICEHEIGHT,
   {X1 - ?SLICEWIDTH, Y1 - ?SLICEHEIGHT, X1 + ?SLICEWIDTH * 2, Y1 + ?SLICEHEIGHT * 2}.
get_matrix(X, Y, SolutX, SolutY) ->
   {SliceWidth, SliceHeight} = get_slice_area(SolutX, SolutY),
   X1 = X div SliceWidth * SliceWidth,
   Y1 = Y div SliceHeight * SliceHeight,
   {X1 - SliceWidth, Y1 - SliceHeight, X1 + SliceWidth * 2, Y1 + SliceHeight * 2}.

%% 获取九宫格小格子的长宽
get_slice_area(SolutX, SolutY) ->
   case SolutX =:= 0 andalso SolutY =:= 0 of
      true ->
         {?SLICEWIDTH, ?SLICEWIDTH};
      false ->
         {round(SolutX / 2), round(SolutY / 2)}
   end.

%% 获取九宫格小格子的长宽
get_slice(X, Y) ->
   {SliceWidth, SliceHeight} = get_slice_area(0, 0),
   X1 = SliceWidth div 2,
   Y1 = SliceHeight div 2,
   {X - X1, Y - Y1, X + X1, Y + Y1}.

%% 获取九宫格小格子的长宽
get_slice(X, Y, SolutX, SolutY) ->
   {SliceWidth, SliceHeight} = get_slice_area(SolutX, SolutY),
   X1 = SliceWidth div 2,
   Y1 = SliceHeight div 2,
   {X - X1, Y - Y1, X + X1, Y + Y1}.

%% 判断X,Y是否在所在的九宫格边界内。
is_in_matrix(X, Y, {X1, Y1, X2, Y2}) ->
   if
      X1 =< X andalso X2 >= X andalso Y1 =< Y andalso Y2 >= Y ->
         true;
      true ->
         false
   end.

%是否在同一九宫格子小格子里面
is_same_slice(X1, Y1, X2, Y2) ->
   SX1 = X1 div ?SLICEWIDTH,
   SY1 = Y1 div ?SLICEHEIGHT,
   SX2 = X2 div ?SLICEWIDTH,
   SY2 = Y2 div ?SLICEHEIGHT,
   if
      SX1 == SX2 andalso SY1 == SY2 ->
         true;
      true ->
         false
   end.
is_same_slice(X1, Y1, X2, Y2, SolutX, SolutY) ->
   {SliceWidth, SliceHeight} = get_slice_area(SolutX, SolutY),
   SX1 = X1 div SliceWidth,
   SY1 = Y1 div SliceHeight,
   SX2 = X2 div SliceWidth,
   SY2 = Y2 div SliceHeight,
   if
      SX1 == SX2 andalso SY1 == SY2 ->
         true;
      true ->
         false
   end.
get_xy_slice(X1, Y1) ->
   SX1 = X1 div ?SLICEWIDTH,
   SY1 = Y1 div ?SLICEHEIGHT,
   {SX1, SY1}.

is_in_range(X1, Y1, X2, Y2, Range) ->
   X = abs(X1 - X2),
   Y = abs(Y1 - Y2),
   X =< Range andalso Y =< Range.

%%@spec 计算两点间的直线距离
distance({X1, Y1}, {X2, Y2}) ->
   round(math:sqrt((X2 - X1) * (X2 - X1) + (Y2 - Y1) * (Y2 - Y1))).

traverseEts(TableName, MatchCnt) ->
   ets:safe_fixtable(TableName, true),
   try
      case ets:match_object(TableName, '$1', MatchCnt) of
         '$end_of_table' ->
            ok;
         {ElemList, NextKey} ->
            match_do_fun(ElemList),
            continue_next(NextKey)
      end
   after
      ets:safe_fixtable(TableName, false)
   end.

continue_next(NextKey) ->
   case ets:match_object(NextKey) of
      '$end_of_table' ->
         ok1111;
      {ElemList, NewNextKey} ->
         match_do_fun(ElemList),
         continue_next(NewNextKey)
   end.

match_do_fun(ElemList) ->
   [io:format("IMY*************one ~p~n", [OneElem])  || OneElem <- ElemList],
   ok.

test() ->
   case ets:info(test, id) of
      undefined ->
         ets:new(test, [named_table, ordered_set, public]);
      _ ->
         ignore
   end,
   [ets:insert(test, {{One, aaa}, bbb}) || One <- lists:seq(1, 15)],
   traverseEts(test, 6).




