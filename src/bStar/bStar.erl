-module(bStar).         %% B星寻路逻辑
-include("utComMisc.hrl").

-compile(export_all).
-export([
   bstar_search/5
   , test/0
   , test/1      %% B星寻路
]).

%% 格子状态
-define(cell_state_none, none).
-define(cell_state_balk, balk).
-define(cell_state_close, close).
-define(cell_state_open, open).

%% 左右分支
-define(branch_dir_none, 0).
-define(branch_dir_left, 1).
-define(branch_dir_right, 2).
%% -define(branch_dir_count, 3).

-define(MAX_DIRECTION, 4). %% 卷曲度数量

-record(pos, {x, y}).

-record(cell, {
   x = 0       %% 	格子在X轴上的坐标,即第几列
   , y = 0     %% 	格子在Y轴上的坐标,即第几行
   , s = ?cell_state_none     %% 状态
   , g = 0      %% 到原点的坐标
   %% ,h = 0      %% 到终点的坐标
   , px = -1    %% 父节点坐标
   , py = -1    %% 父节点坐标
   , branch = ?branch_dir_none  %% 分支
   , dir = -1   %% 方向
   , reel = 0 %% 卷曲度
   , angle = 0
}).

-record(map_config, {
   weight = 100
   , height = 100
}).

-record(bstar_map, {
   origin = #cell{}
   , target = #cell{}
   , map_config
   , open_list = []
   , cell_dict = dict:new()
}).

%% B星搜索
%% @spec() -> false | [#cell{}]
bstar_search(MapConfig, StartX, StartY, EndX, EndY) ->
   FirstCell = #cell{x = StartX, y = StartY},
   TarGet = #cell{x = EndX, y = EndY},
   BStarMap2 = #bstar_map{origin = FirstCell, target = TarGet, map_config = MapConfig, open_list = [FirstCell]},
   start_search(BStarMap2).

start_search(#bstar_map{open_list = []}) ->
   false;
start_search(#bstar_map{open_list = [CurrentCell | Rest], target = Target} = BStarMap) ->
   if
   %% 向着目标前进
      CurrentCell#cell.branch =:= ?branch_dir_none ->
         case find_next(CurrentCell, BStarMap) of
            {Dir, NextCell, BStarMap2} ->
               case NextCell#cell.x =:= Target#cell.x andalso NextCell#cell.y == Target#cell.y of
                  true -> %% 找到目标
                     NextCell2 = NextCell#cell{px = CurrentCell#cell.x, py = CurrentCell#cell.y, g = CurrentCell#cell.g + 1},
                     NewBStarMap = save_cell(BStarMap2, NextCell2),
                     buildPath(NewBStarMap);
                  _ ->
                     %% 自由节点
                     case search_free(CurrentCell, NextCell, Dir, BStarMap2) of
                        {true, OpenList, BStarMap3} ->
                           start_search(BStarMap3#bstar_map{open_list = Rest ++ OpenList});
                        {false, BStarMap3} ->
                           start_search(BStarMap3#bstar_map{open_list = Rest})
                     end
               end;
            _ ->
               false
         end;
      true ->
         %% 攀爬节点处理
         %% 非自由节点，沿着障碍爬
         Dir = get_branchDir(CurrentCell#cell.branch, CurrentCell#cell.dir),
         Dir2 = Dir + ?MAX_DIRECTION - 1,
         Dir3 = ?IIF(Dir2 >= ?MAX_DIRECTION, Dir2 - ?MAX_DIRECTION, Dir2),
         case get_branch(CurrentCell, CurrentCell#cell.branch, Dir3, BStarMap) of
            {true, OpenCell, BStarMap2} ->
               BStarMap3 = close_cell(BStarMap2, CurrentCell),
               start_search(BStarMap3#bstar_map{open_list = Rest ++ [OpenCell]});
            {find, BStarMap2} ->
               buildPath(BStarMap2);
            {false, BStarMap2} ->
               BStarMap3 = close_cell(BStarMap2, CurrentCell),
               start_search(BStarMap3#bstar_map{open_list = Rest})
         end
   end.

%% 自由节点,前方无阻碍
search_free(CurrentCell, #cell{s = ?cell_state_none} = NextCell, _Dir, BStarMap) ->
   NextCell2 = NextCell#cell{px = CurrentCell#cell.x, py = CurrentCell#cell.y, g = CurrentCell#cell.g + 1, s = ?cell_state_open},
   BStarMap2 = save_cell(BStarMap, NextCell2),
   BStarMap3 = close_cell(BStarMap2, CurrentCell),
   {true, [NextCell2], BStarMap3};
%% 自由节点,前方格子关闭
search_free(CurrentCell, #cell{s = ?cell_state_close}, _Dir, BStarMap) ->
   BStarMap2 = close_cell(BStarMap, CurrentCell),
   {false, BStarMap2};
%% 自由节点,前方格子打开
search_free(CurrentCell, #cell{s = ?cell_state_open}, _Dir, BStarMap) ->
   BStarMap2 = close_cell(BStarMap, CurrentCell),
   {false, BStarMap2};
%% 自由节点,前方格子是阻碍点
search_free(CurrentCell, NextCell, Dir, BStarMap) ->
   {OpenedList, NewBStarMap} =
      case get_left_right(CurrentCell, NextCell, ?branch_dir_left, Dir, BStarMap) of
         {false, TempBStarMap} ->
            {[], TempBStarMap};
         {true, Left, TempBStarMap} ->
            {[Left], TempBStarMap}
      end,
   {OpenedList2, NewBStarMap2} =
      case get_left_right(CurrentCell, NextCell, ?branch_dir_right, Dir, NewBStarMap) of
         {false, TempBStarMap2} ->
            {OpenedList, TempBStarMap2};
         {true, Right, TempBStarMap2} ->
            {[Right | OpenedList], TempBStarMap2}
      end,
   NewBStarMap3 = close_cell(NewBStarMap2, CurrentCell),
   {true, OpenedList2, NewBStarMap3}.


%% 取得左分叉点
get_left_right(CurrentCell, NextCell, BranchDir, Dir, #bstar_map{origin = Origin} = BStarMap) ->
   TestDir = get_branchDir(BranchDir, Dir),
   case get_branchAround(CurrentCell, BranchDir, TestDir + 1, BStarMap) of
      false ->
         {false, BStarMap};
      {Cell, TestDir2, Count} ->
         Cell2 = Cell#cell{branch = BranchDir, dir = branchAround(BranchDir, TestDir2)},
         case get_left_right2(CurrentCell, Cell2, BranchDir) of
            false ->
               {false, BStarMap};
            Cell3 ->
               Angle = g_GetDirAngle(Origin#cell.x, Origin#cell.y, Cell2#cell.x, Cell2#cell.y, NextCell#cell.x, NextCell#cell.y),
               Angle2 = case BranchDir of
                           ?branch_dir_left ->
                              ?IIF(Angle > 32, 64 - Angle, 0 - Angle);
                           _ ->
                              ?IIF(Angle > 32, Angle - 64, Angle)
                        end,
               Cell4 = Cell3#cell{reel = Count + 1, s = ?cell_state_open, angle = Angle2},
               BStarMap2 = save_cell(BStarMap, Cell4),
               {true, Cell4, BStarMap2}
         end
   end.

%% 取得分支格子
get_left_right2(CurrentCell, #cell{s = ?cell_state_none} = NextCell, _BranchDir) ->
   NextCell#cell{
      px = CurrentCell#cell.x
      , py = CurrentCell#cell.y
      , g = CurrentCell#cell.g + 1
   };
get_left_right2(CurrentCell, #cell{s = ?cell_state_close} = NextCell, BranchDir) ->
   BranchDir2 = contrary_branch_dir(BranchDir),
   if
      NextCell#cell.branch =:= BranchDir2 andalso NextCell#cell.x == CurrentCell#cell.px andalso NextCell#cell.y =:= CurrentCell#cell.py ->
         false;
      NextCell#cell.g > CurrentCell#cell.g + 1 ->
         NextCell#cell{
            px = CurrentCell#cell.x
            , py = CurrentCell#cell.y
            , g = CurrentCell#cell.g + 1
         };
      true ->
         NextCell
   end;
get_left_right2(CurrentCell, #cell{s = ?cell_state_open} = NextCell, _BranchDir) ->
   case NextCell#cell.g > CurrentCell#cell.g + 1 of
      true ->
         NextCell#cell{
            px = CurrentCell#cell.x
            , py = CurrentCell#cell.y
            , g = CurrentCell#cell.g + 1
         };
      _ ->
         NextCell
   end;
get_left_right2(_CurrentCell, NextCell, _BranchDir) ->
   NextCell.

%% 攀爬
get_branch(CurrentCell, BranchDir, Dir, BStarMap) ->
   get_branch([0, 1, 2, 3], CurrentCell, BranchDir, Dir, BStarMap).
get_branch([], _CurrentCell, _BranchDir, _Dir, BStarMap) ->
   {false, BStarMap};
get_branch([Count | Rest], CurrentCell, BranchDir, Dir, #bstar_map{target = Target, map_config = #map_config{weight = Weight, height = Height}} = BStarMap) ->
   Dir2 = ?IIF(Dir >= ?MAX_DIRECTION, Dir - ?MAX_DIRECTION, Dir),
   NextDir = branchAround(BranchDir, Dir2),
   Around = around(NextDir),
   NextX = CurrentCell#cell.x + Around#pos.x,
   NextY = CurrentCell#cell.y + Around#pos.y,
   case NextY < 0 orelse NextY >= Height orelse NextX < 0 orelse NextX >= Weight of
      true ->
         {false, BStarMap};
      _ ->
         NextCell = get_cell(BStarMap, NextX, NextY),
         if
            NextCell#cell.s /= ?cell_state_balk -> %% 不是阻碍物
               case NextCell#cell.x =:= Target#cell.x andalso NextCell#cell.y =:= Target#cell.y of %% 到目标地
                  true ->
                     NextCell2 = NextCell#cell{px = CurrentCell#cell.x, py = CurrentCell#cell.y, g = CurrentCell#cell.g + 1},
                     BStarMap2 = save_cell(BStarMap, NextCell2),
                     {find, BStarMap2};
                  false ->
                     get_branch2(CurrentCell, NextCell, BranchDir, NextDir, Count, BStarMap)
               end;
            true -> %% 阻碍物
               get_branch(Rest, CurrentCell, BranchDir, Dir + 1, BStarMap)
         end
   end.

%% 无阻碍攀爬
get_branch2(CurrentCell, #cell{s = ?cell_state_none} = NextCell, BranchDir, NextDir, Count, BStarMap) ->
   NextCell2 = NextCell#cell{
      px = CurrentCell#cell.x
      , py = CurrentCell#cell.y
      , g = CurrentCell#cell.g + 1
   },
   NextCell3 = get_branch_i(CurrentCell, NextCell2, BranchDir, NextDir, Count, BStarMap),
   {true, NextCell3, save_cell(BStarMap, NextCell3)};
get_branch2(CurrentCell, #cell{s = ?cell_state_close} = NextCell, BranchDir, NextDir, Count, BStarMap) ->
   BranchDir2 = contrary_branch_dir(BranchDir),
   if
      NextCell#cell.branch =:= BranchDir2 andalso NextCell#cell.x =:= CurrentCell#cell.px andalso NextCell#cell.y =:= CurrentCell#cell.py andalso NextCell#cell.dir /= NextDir ->
         {false, BStarMap};
      NextCell#cell.branch =:= BranchDir andalso NextCell#cell.dir == NextDir ->
         {false, BStarMap};
      NextCell#cell.branch =:= ?branch_dir_none andalso NextCell#cell.g > CurrentCell#cell.g ->
         {false, BStarMap};
      NextCell#cell.g > CurrentCell#cell.g + 1 ->
         NextCell2 = NextCell#cell{px = CurrentCell#cell.x, py = CurrentCell#cell.y, g = CurrentCell#cell.g + 1},
         NextCell3 = get_branch_i(CurrentCell, NextCell2, BranchDir, NextDir, Count, BStarMap),
         {true, NextCell3, save_cell(BStarMap, NextCell3)};
      true ->
         NextCell3 = get_branch_i(CurrentCell, NextCell, BranchDir, NextDir, Count, BStarMap),
         {true, NextCell3, save_cell(BStarMap, NextCell3)}
   end;
get_branch2(CurrentCell, #cell{s = ?cell_state_open} = NextCell, BranchDir, NextDir, Count, BStarMap) ->
   if
      NextCell#cell.branch == BranchDir andalso NextCell#cell.dir == NextDir ->
         {false, BStarMap};
      NextCell#cell.g > CurrentCell#cell.g + 1 ->
         NextCell2 = NextCell#cell{
            px = CurrentCell#cell.x
            , py = CurrentCell#cell.y
            , g = CurrentCell#cell.g + 1
         },
         NextCell3 = get_branch_i(CurrentCell, NextCell2, BranchDir, NextDir, Count, BStarMap),
         {true, NextCell3, save_cell(BStarMap, NextCell3)};
      true ->
         NextCell3 = get_branch_i(CurrentCell, NextCell, BranchDir, NextDir, Count, BStarMap),
         {true, NextCell3, save_cell(BStarMap, NextCell3)}
   end;
get_branch2(_CurrentCell, _NextCell, _BranchDir, _NextDir, _Count, BStarMap) ->
   {false, BStarMap}.

%%% ************************************************** 内部调用函数 **************************************************
%% 计算下一个格子属于自由节点还是攀爬节点
get_branch_i(CurrentCell, NextCell, BranchDir, NextDir, Count, #bstar_map{origin = Origin} = _BStarMap) ->
   Reel = CurrentCell#cell.reel + Count - 1,
   Reel2 = ?IIF(Reel < 0, 0, Reel),
   Angle = g_GetDirAngle(Origin#cell.x, Origin#cell.y, NextCell#cell.x, NextCell#cell.y, CurrentCell#cell.x, CurrentCell#cell.y),
   Angle2 = case BranchDir of
               ?branch_dir_left -> ?IIF(Angle > 32, 64 - Angle, 0 - Angle);
               _ -> ?IIF(Angle > 32, Angle - 64, Angle)
            end,
   Angle3 = CurrentCell#cell.angle + Angle2,
   {Angle4, Reel3} = ?IIF(Angle3 >= 64, {Angle3 - 64, Reel2 - 4}, {Angle3, Reel2}),
   NextCell2 = NextCell#cell{reel = Reel3, s = ?cell_state_open, angle = Angle4},
   ?IIF(NextCell2#cell.reel > 0, NextCell2#cell{branch = BranchDir, dir = NextDir}, NextCell2#cell{branch = ?branch_dir_none, dir = -1, angle = 0}).

%% 找下一个自由点
find_next(CurrentCell, BStarMap) ->
   find_next([0, 1, 2, 3], CurrentCell, BStarMap).
find_next([], _CurrentCell, _BStarMap) ->
   ?PRINT("find_next:err:~w", [_CurrentCell]),
   ok;
find_next([Dir | Rest], CurrentCell, #bstar_map{target = Target, map_config = #map_config{weight = Weight, height = Height}} = BStarMap) ->
   Around = around(Dir),
   NextX = CurrentCell#cell.x + Around#pos.x,
   NextY = CurrentCell#cell.y + Around#pos.y,
   case NextY < 0 orelse NextY >= Height orelse NextX < 0 orelse NextX >= Weight of
      true ->
         NewBStarMap = save_cell(BStarMap, CurrentCell#cell{s = ?cell_state_close}),
         find_next(Rest, CurrentCell, NewBStarMap);
      _ ->
         DirDiff = g_GetDirDiff(CurrentCell#cell.x, CurrentCell#cell.y, Target#cell.x, Target#cell.y, NextX, NextY),
         case DirDiff =< 8 of
            true ->
               NextCell = get_cell(BStarMap, NextX, NextY),
               {Dir, NextCell, BStarMap};
            _ ->
               find_next(Rest, CurrentCell, BStarMap)
         end
   end.

%% 取得格子
get_cell(#bstar_map{cell_dict = Dict, map_config = MapConfig}, X, Y) ->
   case dict:find({X, Y}, Dict) of
      {ok, O} -> O;
      _ ->
         CellState = ?IIF(lib_map:is_walkable_by_index(MapConfig, X, Y), ?cell_state_none, ?cell_state_balk),
         #cell{x = X, y = Y, s = CellState}
   end.

%% 保存格子
save_cell(#bstar_map{cell_dict = Dict} = BStarMap, #cell{x = X, y = Y} = Cell) ->
   NewDict = dict:store({X, Y}, Cell, Dict),
   BStarMap#bstar_map{cell_dict = NewDict}.

%% 关闭格子
close_cell(BStarMap, Cell) ->
   Cell2 = get_cell(BStarMap, Cell#cell.x, Cell#cell.y),
   save_cell(BStarMap, Cell2#cell{s = ?cell_state_close}).

%% 取得周围方向
around(0) -> #pos{x = 0, y = 1};
around(1) -> #pos{x = -1, y = 0};
around(2) -> #pos{x = 0, y = -1};
around(3) -> #pos{x = 1, y = 0}.

%% 取得原点到终点和当前点的夹角
g_GetDirDiff(OriginX, OriginY, TargetX, TargetY, X, Y) ->
   TargetDir = g_GetDirIndex(OriginX, OriginY, TargetX, TargetY),
   TestDir = g_GetDirIndex(OriginX, OriginY, X, Y),
   DirDiff = TargetDir - TestDir,
   DirDiff2 = ?IIF(DirDiff < 0, DirDiff + 64, DirDiff),
   ?IIF(DirDiff2 > 32, 64 - DirDiff2, DirDiff2).

%% 取得方向
g_GetDirAngle(OriginX, OriginY, TargetX, TargetY, X, Y) ->
   TargetDir = g_GetDirIndex(OriginX, OriginY, TargetX, TargetY),
   TestDir = g_GetDirIndex(OriginX, OriginY, X, Y),
   DirAngle = TargetDir - TestDir,
   case DirAngle < 0 of
      true -> DirAngle + 64;
      _ -> DirAngle
   end.

%% 取得两点间的角度（360度转换为64度）
g_GetDirIndex(X1, Y1, X2, Y2) when X1 =:= X2 andalso Y1 =:= Y2 -> -1;
g_GetDirIndex(X1, Y1, X2, Y2) when X1 =:= X2 andalso Y2 > Y1 -> 0;
g_GetDirIndex(X1, Y1, X2, Y2) when X1 =:= X2 andalso Y2 < Y1 -> 32;
g_GetDirIndex(X1, Y1, X2, Y2) when Y1 =:= Y2 andalso X2 > X1 -> 48;
g_GetDirIndex(X1, Y1, X2, Y2) when Y1 =:= Y2 andalso X2 < X1 -> 16;
g_GetDirIndex(X1, Y1, X2, Y2) ->
   X = X2 - X1,
   Y = Y2 - Y1,
   Distance = math:sqrt(X * X + Y * Y),
   Sin = Y / Distance,
   Radian = math:asin(Sin),
   Angle = 180 / (math:pi() / Radian),
   Angle2 = if
               X > 0 andalso Y > 0 ->
                  Angle;
               X > 0 andalso Y < 0 ->
                  360 + Angle;
               X < 0 andalso Y < 0 ->
                  180 + Angle;
               true ->
                  180 + Angle
            end,
   (48 + round(Angle2 * 64 / 360)) rem 64.

branchAround(0, 0) -> 0;
branchAround(0, 1) -> 1;
branchAround(0, 2) -> 2;
branchAround(0, 3) -> 3;
branchAround(1, 0) -> 0;
branchAround(1, 1) -> 3;
branchAround(1, 2) -> 2;
branchAround(1, 3) -> 1;
branchAround(2, 0) -> 0;
branchAround(2, 1) -> 1;
branchAround(2, 2) -> 2;
branchAround(2, 3) -> 3.

%% 生成路径
buildPath(#bstar_map{origin = Origin, target = Target} = BStarMap) ->
   Current = get_cell(BStarMap, Target#cell.x, Target#cell.y),
   buildPath(Current, Origin, [], BStarMap).
buildPath(Current, Origin, Res, BStarMap) ->
   if
      Current#cell.x =:= Origin#cell.x andalso Current#cell.y =:= Origin#cell.y ->
         Res;
      true ->
         NewCurrent = get_cell(BStarMap, Current#cell.px, Current#cell.py),
         buildPath(NewCurrent, Origin, [Current | Res], BStarMap)
   end.

%% 相返分支
contrary_branch_dir(?branch_dir_left) ->
   ?branch_dir_right;
contrary_branch_dir(_) ->
   ?branch_dir_left.

%% 取得分支Dir
get_branchDir(Branch, CurDir) ->
   get_branchDir([0, 1, 2, 3], Branch, CurDir).
get_branchDir([], _, _CurDir) ->
   throw({badarg, get_branchAround});
get_branchDir([Dir | Rest], Branch, CurDir) ->
   case branchAround(Branch, Dir) == CurDir of
      true -> Dir;
      _ -> get_branchDir(Rest, Branch, CurDir)
   end.

%% 取得分支Around
get_branchAround(CurrentCell, Branch, TestDir, BStarMap) ->
   get_branchAround([0, 1, 2, 3], CurrentCell, Branch, TestDir, BStarMap).
get_branchAround([], _CurrentCell, _Branch, _TestDir, _BStarMap) ->
   false;
get_branchAround([Count | Rest], CurrentCell, Branch, TestDir, #bstar_map{map_config = #map_config{weight = Weight, height = Height}} = BStarMap) ->
   TestDir2 = ?IIF(TestDir =:= ?MAX_DIRECTION, 0, TestDir),
   BranchAround = branchAround(Branch, TestDir2),
   Around = around(BranchAround),
   NextX = CurrentCell#cell.x + Around#pos.x,
   NextY = CurrentCell#cell.y + Around#pos.y,
   if
      NextX < 0 orelse NextX >= Weight orelse NextY < 0 orelse NextY >= Height ->
         false;
      true ->
         NextCell = get_cell(BStarMap, NextX, NextY),
         if
            NextCell#cell.s /= ?cell_state_balk ->
               {NextCell, TestDir2, Count};
            true ->
               get_branchAround(Rest, CurrentCell, Branch, TestDir2 + 1, BStarMap)
         end
   end.

%% ******************************* 测试 **************************************
test() ->
   MapConfig = lib_map:get_map_config(1000001),
   bstar_search(MapConfig, 28, 44, 132, 44).
test(Max) ->
   MapConfig = lib_map:get_map_config(1000001),
   test(MapConfig, Max, 28, 44, 132, 44).
test(MapConfig, Max, X1, Y1, X2, Y2) ->
   List = lists:seq(1, Max),
   statistics(wall_clock),
   [bstar_search(MapConfig, X1, Y1, X2, Y2) || _ <- List],
   {_, Time} = statistics(wall_clock),
   io:format("t=~p ", [Time]).

is_walkable_by_index(MapConfig, X, Y) ->
   true.