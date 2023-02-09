-module(utDBLoad).

-export([loadData/1]).

%%-----------------------------------------------------------------
%% load_data/1
%% @doc 起服的时候 加载数据
%% ```
%%
%% '''
%% @end
%% -----------------------------------------------------------------
-spec loadData(non_neg_integer()) -> no_return().
%% -----------------------------------------------------------------
% #方式4 仅仅使用 id<max and limit size;
% #每次查询前获取上一页最小id作为下一页的最大id使用
% ##20万-40万:82959503-82620566  60万-80万:82334851   260万-280万:80106996-79887685 660万-680万:76010656-75810657 1660万-1680万:53482458-53240959 3660万-3680万:32532145-32332146
% #首页查询
% select * from order_table order by id desc limit 200000;
% #非首页查询
% #平均耗时：1.539s
% select * from order_table where id <=82543981 order by id desc limit 200000;
loadData(Limit) ->
   ValueList = db:reads_mysql(dbTableName, [], [{key_id, desc}], [Limit]),
   NextId = dealValueList(ValueList, Limit, 0, 0),
   loadLeft(NextId, Limit).

loadLeft(0, _Limit) ->
   ok;
loadLeft(NextId, Limit) ->
   ValueList = db:reads_mysql(dbTableName, [{key_id, "<", NextId}], [{key_id, desc}], [Limit]),
   NewNextId = dealValueList(ValueList, Limit, 0, 0),
   loadLeft(NewNextId, Limit).

dealValueList([], Limit, SumCnt, NextId) ->
   case SumCnt >= Limit of
      true ->
         NextId;
      _ ->
         0
   end;
dealValueList([OneInfo | AllyList], Limit, SumCnt, _NextId) ->
   NewNextId = parseData(OneInfo),
   dealValueList(AllyList, Limit, SumCnt + 1, NewNextId).

parseData(OneInfo) ->
   _NewNextId = element(1, OneInfo).
