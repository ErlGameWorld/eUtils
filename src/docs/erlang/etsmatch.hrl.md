-type matchExpression() :: [ matchFunction(), ... ].

-type matchFunction() :: { matchHead(), matchConditions(), matchBody()}.

-type matchHead() :: matchVariable() | '_' | { matchHeadPart(), ...}.

-type matchHeadPart() :: term() | matchVariable() | '_'.

-type matchVariable() :: '$<number>'.

-type matchConditions() :: [ matchCondition(), ...] | [].

-type matchCondition() :: { guardFunction() } | { guardFunction(), conditionExpression(), ... }.
-type boolFunction() :: is_atom | is_float | is_integer | is_list | is_number | is_pid | is_port | is_reference | is_tuple | is_map | map_is_key | is_binary | is_function | is_record | 'and' | 'or' | 'not' | 'xor' | 'andalso' | 'orelse'.
-type conditionExpression() :: exprMatchVariable() | { guardFunction() } | { guardFunction(), conditionExpression(), ... } | termConstruct().
-type exprMatchVariable() :: matchVariable() (bound in the MatchHead) | '$_' | '$$'
-type termConstruct():: {{}} | {{ conditionExpression(), ... }} | [] | [conditionExpression(), ...] | #{} | #{term() => conditionExpression(), ...} | nonCompositeTerm() | constant().
-type nonCompositeTerm() :: term(). %%   (not list or tuple or map)
-type constant() :: {const, term()}.
-type guardFunction() :: boolFunction() | abs | element | hd | length | map_get | map_size | node | round | size | bit_size | tl | trunc | '+' | '-' | '*' | 'div' | 'rem' | 'band' | 'bor' | 'bxor' | 'bnot' | 'bsl' | 'bsr' | '>' | '>=' | '<' | '=<' | '=:=' | '==' | '=/=' | '/=' | self.
-type matchBody() :: [ conditionExpression(), ... ].

ets表并发写优化
    1.核心思想还是避免数据写操作是并发操作 这种可以将ets 设置为bag 相当于有主key 是并发的 然后次key的拥有者只能自己写自己的数据
    2. 使用ets:select_replace 实现无锁替换操作
        update_with_fun(Table, Key, Fun) ->
            [Old] = ets:lookup(Table, Key),
            New = Fun(Old),
            Success = (1 =:= ets:select_replace(Table, [{Old, [], [{const, New}]}])),
            case Success of
            true -> New;
            false -> update_with_fun(Table, Key, Fun)
        end.

match
    用法：获取 ets 存储数据结构的某几个数据集合
match_object
    用法：获取 ets 存储的符合条件的整个结构，只能支持直接值匹配
    用法 1：迭代的遍历整个 ets 列表 match_object/3
    用法 2：查找指定数据的 ets 列表 match_object/2
select
    用法： 一般用于条件判定来筛选符合条件的数据和指定格式 (>=, =/=,<,=< 等)
    一般和 ets:fun2ms 结合使用
    效率较前两个较为慢一些，特别是 ets 中 key 的查找，效率就更慢了，能用 match_object 的就不要用 select

