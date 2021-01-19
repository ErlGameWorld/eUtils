%% 三目元算符
-define(IIF(Cond, Then, That), case Cond of true -> Then; _ -> That end).

%%汉字unicode编码范围 0x4e00 - 0x9fa5
-define(UNICODE_CHINESE_BEGIN, (4 * 16 * 16 * 16 + 14 * 16 * 16)).
-define(UNICODE_CHINESE_END, (9 * 16 * 16 * 16 + 15 * 16 * 16 + 10 * 16 + 5)).

-define(PRINT(Format, Args),
   io:format(Format, Args)).

%% format_record(record名, record数据) -> [{#record.field, record_field_value}]
-define(recordToKvList(RecordName, RecordData),
   fun() ->
      Fields = record_info(fields, RecordName),
      [_ | Data] = tuple_to_list(RecordData),
      {RecordName, lists:zip(Fields, Data)}
   end()).


-define(pdMemInfo, '$pdMemInfo').
%% 运行时内存初始化宏
-define(MII(), utVMInfo:memInfoInit(?MODULE, ?LINE)).
%% 运行时内存默认打印函数
-define(MIP(), utVMInfo:memInfoPrint(?MODULE, ?LINE, 100)).
%% 运行时内存带阀值参数打印函数
-define(MIP(Threshold), utVMInfo:memInfoPrint(?MODULE, ?LINE, Threshold)).