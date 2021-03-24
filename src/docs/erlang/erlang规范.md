Erlang 编码标准指引
====================================
Table of Contents:
* [约定 &amp; 规则](#约定--规则)
   * [源码布局](#源码布局)
      * [用空格代替制表符(tab)](#用空格代替制表符(tab))
      * [使用你的空格键](#使用你的空格键)
      * [行尾不要留空格](#行尾不要留空格)
      * [每行100列](#每行100列)
      * [保持现有风格](#保持现有风格)
      * [避免多层嵌套](#避免多层嵌套)
      * [更多, 小函数比 case 表达式好用](#更多-小函数比-case-表达式好用)
      * [函数按逻辑功能分组](#函数按逻辑功能分组)
      * [集中你的 types](#集中你的-types)
      * [不要上帝模块](#不要上帝模块)
      * [Honor DRY](#抽象重复代码)
      * [避免动态调用](#避免动态调用)
  * [语法](#语法)
      * [避免使用 if 表达式](#避免使用-if-表达式)
      * [避免嵌套 try...catches](#避免嵌套try...catches)
   * [命名](#命名)
      * [在命名概念时保持一致](#在命名概念时保持一致)
      * [Don't use _Ignored variables](#不要使用匿名变量)
      * [避免用布尔类型作为函数参数](#避免用布尔类型原子作为函数参数)
      * [原子(atoms)请用小写](#原子(atoms)请用小写)
      * [函数名](#函数名)
      * [变量名](#变量名)
   * [宏](#宏)
      * [宏的应用场景](#宏的应用场景)
      * [宏名要大写](#宏名要大写)
   * [记录(Records)](#记录(Records))
      * [记录命名](#记录命名)
      * [在 specs 里避免出现记录(record)](#在specs里避免出现记录)
      * [给记录添加类型Types](#给记录添加类型Types)
   * [其它](#其它)
      * [给函数添加-spec函数规范定义](#给函数添加-spec函数规范定义)
      * [模块中不要用import](#模块中不要用import)
      * [Don't Use Case Catch](#Don't Use Case Catch)
* [好的建议和方法](#好的建议和方法)
  * [优先使用高级函数而不是手写的递归方法](#优先使用高级函数而不是手写的递归方法)
  * [驼峰式命名,下划线命名](#驼峰式命名，下划线命名)
  * [更短 (但仍保持有意义的) 的变量名称](#更短(但仍保持有意义的)的变量名称 )
  * [注释等级](#注释等级)
  * [保持函数精简](#保持函数精简)
  * [避免不必要调用length/1](#避免不必要调用length/1 )

### 约定--规则  
### 源码布局
  
#### 用空格代替制表符(tab)
> 用空格代替制表符(tab),使用两个空格符作为缩进.
*Examples*: [indent](src/indent.erl)
```erlang
%% @doc 不一致
bad() ->
  try
      ThisBlock = is:indented(with, two, spaces),
    that:is_good(ThisBlock) %% 这一部分的代码缩进用两个空格,没啥毛病
  catch
      _:_ ->
          this_block:is_indented(with, four, spaces) %% 但是这一部分的却用了4个空格,看起来不统一,很糟糕
  end.

%% @doc 一致,但是使用4个空格
better() ->
    receive
        {this, block} -> is:indented(with, four, spaces);
        _That -> is:not_good() %% 这一部分的代码缩进用四个空格,不太好
    after 100 ->
        but:at_least(it, is, consistent) %% 但起码全部是使用一致的风格
    end.

%% @doc 不错
good() ->
  case indentation:block() of
    {2, spaces} -> me:gusta();
    {_, _} -> not_sure:if_gusta()
  end.
```

*原因*: 这并不意味着允许代码中存在多层嵌套的结构.如果代码足够干净,2个空格就足够了,代码看起来更加简洁,同时在同一行中也能容纳更多的字符.

***
#### 使用你的空格键
> 使用空格来分割开运算符和逗号.

*Examples*: [spaces](src/spaces.erl)
```erlang
% @doc 没有空格
bad(_My,_Space,_Bar)->[is,'not',working].

% @doc 带空格!!
good(_Hey, _Now, _It) -> ["works " ++ "again, " | [hooray]].
```

*原因*: 同上,主要是为了代码易于读写,等等. 在这里顺便提醒一下erlang宏展开的时候会自动在两边增加分隔符
*Examples*: -define(plus,+).
t(A,B) -> A?plus+B.
结果会是这样的：
t(A,B) -> A + + B.
而不是这样的：
t(A,B) -> A ++ B.

***
#### 行尾不要留空格
> 检查你的没一行代码的最后,不要有空格.

*Examples*: [trailing_whitespace](src/trailing_whitespace.erl)

```erlang
bad() -> "这行尾部有空格".       

good() -> "这行没有".       
```

*原因*: 这是提交噪音. 可以看看[长篇论据](https://programmers.stackexchange.com/questions/121555/why-is-trailing-whitespace-a-big-deal).

#### 每行100列
> 每行最多100个字符.

*Examples*: [col_width](src/col_width.erl)

```erlang
%$ @doc 太宽
bad([#rec{field1 = FF1, field2 = FF2, 
field3 = FF3}, #rec{field1 = BF1, field2 = BF2, field3 = BF3} | Rest], Arg2) ->
  other_module:bad(FF1, FF2, FF3, BF1, BF2, BF3, bad(Rest, Arg2)).

%% @doc 不错 (< 100 字符)
good([Foo, Bar | Rest], Arg2) ->
  #rec{field1 = FF1, field2 = FF2, field3 = FF3} = Foo,
  #rec{field1 = BF1, field2 = BF2, field3 = BF3} = Bar,
  other_module:good(FF1, FF2, FF3, BF1, BF2, BF3, good(Rest, Arg2)).
```

*原因*：太长的行在处理的时候是相当痛苦的: 要么在编辑的时候不停水平滚动, 要么就是忍受自动断行造成布局错乱.
100个字符的限制不仅仅让每一行保持简短, 另外也能让你可以毫无压力地在标准的手提电脑屏幕上并排同时打开两个文件, 或者三个 1080p 显示器上.

***
#### 保持现有风格
> 当你维护别人的模块时, 请坚持按前人的编码风格样式维护. 如果项目有整体的风格样式, 那么在编写新的模块是也要坚持按项目的整体风格进行.

*Examples*: [existing_style](src/existing_style.erl)

```erlang
bad() ->
  % 之前的代码
  List = [ {elem1, 1}
         , {elem2, 2}
  % 新代码 (不按之前的格式来编码)
         , {elem3, 3}, {elem4, 4},
           {elem5, 5}
         ],
  other_module:call(List).
good() ->
  % 之前的代码
  List = [ {elem1, 1}
         , {elem2, 2}
  % 新代码 (按之前的格式来编码)
         , {elem3, 3}
         , {elem4, 4}
         , {elem5, 5}
         ],
  other_module:call(List).
```

*原因*: 在维护别人的代码的时候,如果你不喜欢他的编码规范,这仅仅是你个人不喜欢而已,但是如果你不按他之前写的编码样式继续编写,
那这个模块就有两种编码样式了,这样你本人看起来这些代码很丑陋,别人看你的代码也觉得很丑陋,这样会让代码更加不容易维护.

***

#### 避免多层嵌套
> 尽量不要出现超过三个层级嵌套的代码样式

*Examples*: [nesting](src/nesting.erl)

```erlang
bad() ->
  case this:function() of
    has ->
      try too:much() of
        nested ->
          receive
            structures ->
              it:should_be(refactored);
            into ->
              several:other(functions)
          end
      catch
        _:_ ->
          dont:you("think?")
      end;
    _ ->
      i:do()
  end.

good() ->
  case this:function() of
    calls ->
      other:functions();
    that ->
      try do:the(internal, parts) of
        what ->
          was:done(in)
      catch
        _:the ->
          previous:example()
      end
  end.

%% 译者注: 上面部分代码的意思:通过将嵌套部分的代码封装成一些新的函数,可以减少嵌套的结构.
```

*原因*: 嵌套级别表示函数中的逻辑比较复杂,过多地将需要执行和完成的决策放在单个函数中. 这不仅阻碍了可读性,而且阻碍了可维护性,如果嵌套过多梳理逻辑
分支代码也很容易看错拆分成相应的函数可读性更好，逻辑也会更清晰，也便于调试以及编写单元测试的进行,

***
#### 更多-小函数比-case-表达式好用
> 使用模式匹配的函数子句代替 case 表达式. 特别是当 case 在:
> - 函数的开头(下面代码第一个bad函数)
> - case分支比较多的时候

*Examples*: [smaller_functions](src/smaller_functions.erl)

```erlang
%% @doc 这个函数仅仅使用的是 case 表达式
bad(Arg) ->
  case Arg of
    this_one -> should:be(a, function, clause); %% 这一句应该用一个函数子句代替
    and_this_one -> should:be(another, function, clause) %% 这一句应该用另一个函数子句代替
  end.

%% @doc 使用模式匹配
good(this_one) -> is:a(function, clause); %% 这是一个函数子句
good(and_this_one) -> is:another(function, clause). %% 这是另一个函数子句


%% @doc case 表达式在函数内部
bad() ->
  InitialArg = some:initial_arg(),
  InternalResult =
    case InitialArg of
      this_one -> should:be(a, function, clause);
      and_this_one -> should:be(another, function, clause)
    end,
  some:modification(InternalResult).

%% @doc 使用多个函数字句代替内部 case 表达式
good() ->
  InitialArg = some:initial_arg(),
  InternalResult = good(InitialArg),
  some:modification(InternalResult).
```

*原因:* 一般而言,函数体中的一个case代表某种决定,同时函数应尽可能的简单. 如果决策结果的每个分支作为一个函数子句而不是一个case子句来实现,
同时函数子句的函数名也可以让代码容易读懂. 换言之, 这个 case 在此扮演的是 '匿名函数', 除非它们在高阶函数的上下文中被使用,而只是模糊的含义.

***
#### 函数按逻辑功能分组
> 始终保持区分导出函数和未导出的函数, 并将导出的放在前面, 除非还有其他方法更加有助于可读性和代码发现的.

*Examples*: [grouping_functions](src/grouping_functions)

`bad.erl`:

```erlang
%%% @doc 私有和公用函数随意摆放
-module(bad).

-export([public1/0, public2/0]).

public1() -> private3(atom1).

private1() -> atom2.

public2() -> private2(private1()).

private2(Atom) -> private3(Atom).

private3(Atom) -> Atom.
```

`better.erl`:

```erlang
%%% @doc 按函数相关程度区分组
-module(better).

-export([public1/0, public2/0]).

public1() ->
  case application:get_env(atom_for_public_1) of
    {ok, X} -> public1(X);
    _ -> throw(cant_do)
  end.
%% @doc 这是一个仅仅与上面函数相关的私有函数
public1(X) -> private3(X).

public2() -> private2(private1()).

private1() -> atom2.

private2(Atom) -> private3(Atom).

private3(Atom) -> Atom.
```

`good.erl`:

```erlang
-module(good).

-export([public1/0, public2/0]).

public1() ->
  case application:get_env(atom_for_public_1) of
    {ok, X} -> private3(X);
    _ -> throw(cant_do)
  end.

public2() -> private2(private1()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

private1() -> atom2.

private2(Atom) -> private3(Atom).

private3(Atom) -> Atom.
```

*原因*: 好的代码结构易于读/理解/修改，很多时候在写erlang代码的时候写着写着发现需要添加一些额外的分支匹配函数，
有时候这种分支匹配函数就为了某种情况下使用，有可能就用一次，这时候我的习惯就是把这个分支匹配函数写在要用这个函数的函数前面

***
#### 集中你的 types
> 将 types 都放在文件开头的地方

*Examples*: [type_placement](src/type_placement.erl)

```erlang
-type good_type() :: 1..3.

-spec good() -> good_type().
good() -> 2.


-type bad_type() :: 1..3.
-spec bad() -> bad_type().
bad() -> 2.
```

*原因*: Types 定义的数据结构极有可能被用于多个函数,所以他们的定义不能只与其中一个有关. 
另外将他们在代码中放在一起并像文档一样展示他们就像edoc 也是将 types 放在每个文档的开头一样.

***
#### 不要上帝模块
> 不要让你的系统使用上帝模块 (模块中包含了很多函数 和/或 函数与函数之间处理的事情并不相关)

*Examples*: [god](src/god.erl)

```erlang
%%% @doc all of your db operations belong to us!
-module(god).

-export([create_user/1, create_user/2, create_user/3]).
-export([update_user/2, update_user/3]).
-export([delete_user/1]).
-export([create_post/1, create_post/2, create_post/3]).
-export([update_post/2, update_post/3]).
-export([delete_post/1]).
-export([create_comment/2, create_comment/3]).
-export([update_comment/3, update_comment/4]).
-export([delete_comment/2]).

create_user(Name) -> create_user(Name, undefined).

create_user(Name, Email) -> create_user(Name, Email, undefined).

create_user(Name, Email, Phone) ->
  some_db:insert(users, [{name, Name}, {email, Email}, {phone, Phone}]).

update_user(Name, Changes) ->
  some_db:update(users, [{name, Name}], Changes).

update_user(Name, Key, Value) ->
  update_user(Name, [{Key, Value}]).

delete_user(Name) ->
  some_db:delete(users, [{name, Name}]).

create_post(Text) -> create_post(Text, undefined).

create_post(Text, Title) -> create_post(Text, Title, undefined).

create_post(Text, Title, Image) ->
  some_db:insert(posts, [{text, Text}, {title, Title}, {image, Image}]).

update_post(Text, Changes) ->
  some_db:update(posts, [{text, Text}], Changes).

update_post(Text, Key, Value) ->
  update_post(Text, [{Key, Value}]).

delete_post(Text) ->
  some_db:delete(posts, [{text, Text}]).

create_comment(PostId, Text) -> create_comment(PostId, Text, undefined).

create_comment(PostId, Text, Image) ->
  some_db:insert(comments, [{post_id, PostId}, {text, Text}, {image, Image}]).

update_comment(PostId, CommentId, Changes) ->
  some_db:update(comments, [{post_id, PostId}, {id, CommentId}], Changes).

update_comment(PostId, CommentId, Key, Value) ->
  update_comment(PostId, CommentId, [{Key, Value}]).

delete_comment(PostId, CommentId) ->
  some_db:delete(comments, [{post_id, PostId}, {id, CommentId}]).
```

*原因*: 上帝模块, 类似上帝对象, 了解过多或者负责过多的模块. 上帝模块通常是因为不断的增加功能函数演变出来的. 
A beautiful, to-the-point module with one job, one responsibility done well, gains a function. Then another, which does the same thing but with different parameters. 
总有一天, 你会写出一个包含500多个函数并且高达6000多行代码的模块 .因此,让模块(和功能)只做一件事情就可以很容易地探索和理解代码,从而维护它.
这个的意思就是按功能拆分模块，不同功能让放到不同模块实现，A模块做A功能相关的事情，B模块做B模块相关的事情，不要把不相关的功能放到一个模块去，特别是作为较底层的模块


***
#### 抽象重复代码
> 不要在多个地方使用相同的代码，请用函数或者变量去代替。  
1 把重复的代码抽象成函数
2 把同个作用域同个函数(参数也一样)的结果用变量保存，替换后面再次调到该函数的地方
*Examples*: [dry](src/dry.erl)

```erlang
%% @doc this is a very very trivial example, DRY has a much wider scope but it's
%%      provided just as an example
bad() ->
  case somthing:from(other, place) of
    {show, _} ->
      display:nicely(somthing:from(other, place));
    nothing ->
      display:nothing()
  end.

good() ->
  case somthing:from(other, place) of
    {show, _} = ThingToShow ->
      display:nicely(ThingToShow);
    dont_show_me ->
      display:nothing()
  end.
```

*原因*: 这是一条特别的规约，因为这样子审查人员就可以拒绝接受那些好几个地方都包含相同代码的提交（PRs）了，或者接受那些在某个地方已完成的可复用新实现。 

***
#### 避免动态调用
> If there is no specific need for it, don't use dynamic function calling.

*Examples*: [dyn_calls](src/dyn_calls.erl)

```erlang
bad(Arg) ->
  Mods = [module_1, module_2, module_3],
  Fun = my_function,
  lists:foreach(
    fun(Mod) ->
      Mod:Fun(Arg)
    end, Mods).

good(Arg) ->
  mdoule_1:my_function(Arg),
  module_2:my_function(Arg),
  module_3:my_function(Arg).
```

*原因*: Dynamic calls can't be checked by [``xref``](http://erlang.org/doc/apps/tools/xref_chapter.html), 
one of the most useful tools in the Erlang world. ``xref`` is a cross reference checking/observing tool.
Xref是一个交叉引用工具，可用于查找函数，模块，应用程序和发行版之间的依赖关系。它通过分析定义的函数和函数调用来实现


*原因*: 不要写面条式代码很难阅读, 理解和修改. The function callgraph for your program should strive to be a directed acyclic graph.

### 语法
Erlang语法很可怕, 我说得对吗? 所以你也可以充分利用它, 对吗? _对_?

***
#### 避免使用 if 表达式
> Don't use `if`.

*Examples*: [no_if](src/no_if.erl)

```erlang
bad(Connection) ->
  {Transport, Version} = other_place:get_http_params(),
  if
    Transport =/= cowboy_spdy, Version =:= 'HTTP/1.1' ->
      [{<<"connection">>, utils:atom_to_connection(Connection)}];
    true ->
      []
  end.


better(Connection) ->
  {Transport, Version} = other_place:get_http_params(),
  case {Transport, Version} of
    {cowboy_spdy, 'HTTP/1.1'} ->
      [{<<"connection">>, utils:atom_to_connection(Connection)}];
    {_, _} ->
      []
  end.
 
good(Connection) ->
  {Transport, Version} = other_place:get_http_params(),
  connection_headers(Transport, Version, Connection).
  
connection_headers(cowboy_spdy, 'HTTP/1.1', Connection) ->
    [{<<"connection">>, utils:atom_to_connection(Connection)}];
connection_headers(_, _, _) ->
    [].
```

*原因*: 在某些情况下，`if`会在代码中引入静态布尔逻辑，从而降低代码的灵活性。在其他情况下，
`case`或在其子句中具有模式匹配的函数调用是更具说明性。 对于新手（已经学会在其他语言中使用`if`），
Erlang的“if”可能难以理解或容易被滥用。

*更多相关的讨论看下面*:
- [From OOP world](http://antiifcampaign.com/)
- [In this repo](issues/14)
- [In erlang-questions](http://erlang.org/pipermail/erlang-questions/2014-September/080827.html)

***
#### 避免嵌套try...catches
> Don't nest `try…catch` clauses

*Examples*: [nested_try_catch](src/nested_try_catch.erl)

```erlang
bad() ->
  try
    maybe:throw(exception1),
    try
      maybe:throw(exception2),
      "We are safe!"
    catch
      _:exception2 ->
        "Oh, no! Exception #2"
    end
  catch
    _:exception1 -> "Bummer! Exception #1"
  end.

good1() ->
  try
    maybe:throw(exception1),
    maybe:throw(exception2),
    "We are safe!"
  catch
    _:exception1 ->
      "Bummer! Exception #1";
    _:exception2 ->
      "Oh, no! Exception #2"
  end.

good2() ->
  try
    maybe:throw(exception1),
    a_function:that_deals(with, exception2),
    "We are safe!"
  catch
    _:exception1 ->
      "Bummer! Exception #1"
  end.
```
*原因*: 嵌套`try ... catch`块会破坏它们的整个目的，即将处理错误的代码与处理预期执行路径的代码隔离开来。
### 命名

***
#### 在命名概念时保持一致
> 对于相同的概念，在任何地方都使用相同的变量名 (即使在不同的模块当中).

*Examples*: [consistency](src/consistency.erl)

```erlang
bad(UserId) -> internal_bad(UserId).

internal_bad(User_Id) -> internal_bad2(User_Id).

internal_bad2(Usr) -> db:get_by_id(Usr).


good(UserId) -> internal_good(UserId).

internal_good(UserId) -> internal_good2(UserId).

internal_good2(UserId) -> db:get_by_id(UserId).
```

*原因*: 当要找出所有用到``OrgID`` 的代码 (例如 我们想把变量从 ``string`` 转为 ``binary``), 
我们只要搜索名为 ``OrgID``的变量，而不需要查找所有有可能关于 ``OrgID``的命名变量.
对于这个还是要注意 相同的概念 这个限定 比如我们经常用到的等级 玩家等级 装备等级 技能等级 公会等级  虽然都是等级，但是最好前缀..level
类似还有一些同类型枚举定义加个前缀，字典原子名定义可以加个 pd_ 前缀

***
#### 不要使用匿名变量
> 以_开头的变量仍然是变量，并且是匹配和绑定的，_开头的变量只是在不使用它的时候避免编译器产生警告信息。如果将_添加到变量的名称，请不要使用它。
同时即使下划线开头的变量，在后面代码中不使用，但是应该还是需要把下线线后面的变量名写成好，一是为了可读性，二 当修改需要使用该比变量的时候直接去掉下划线
三 即使是_划线开始 ，但是这个变量名还是被绑定了的 同个函数内不能和其他下划线开头的变量一样
*Examples*: [ignored_vars](src/ignored_vars.erl)

```erlang
bad(_Number) -> 2 * _Number.

good(Number) -> 2 * Number.
```

*原因*: They are **not** supposed to be used.

***
#### 避免用布尔类型原子作为函数参数
> Don't use boolean parameters (i.e. `true` and `false`) to control clause selection.

*Examples*: [boolean_params](src/boolean_params.erl)

```erlang
bad(EdgeLength) -> bad_draw_square(EdgeLength, true).

bad_draw_square(EdgeLength, true) ->
  square:fill(square:draw(EdgeLength));
bad_draw_square(EdgeLength, false) ->
  square:draw(EdgeLength).

good(EdgeLength) -> good_draw_square(EdgeLength, full).

good_draw_square(EdgeLength, full) ->
  square:fill(square:draw(EdgeLength));
good_draw_square(EdgeLength, empty) ->
  square:draw(EdgeLength).
```

*原因*: 主要目的在于，使用其他原子做匹配时意图清晰，不要求读者检查功能定义以了解其功能。

***
#### 原子(atoms)请用小写
> 原子命名只能使用小写字母. 当一个原子含有多个单词时












































,单词之间用 `_` 隔开. 特殊情况可以允许用大写 (例如  `'GET'`, `'POST'`, 等等)
 但是尽量还是控制在一定使用量.

*Examples*: [atoms](src/atoms.erl)

```erlang
bad() -> ['BAD', alsoBad, bad_AS_well].

good() -> [good, also_good, 'good@its.mail'].
```

*原因*: 坚持一个约定使得更容易在代码周围没有“重复”原子。 此外，不使用大写字母或特殊字符减少了对原子周围的需求。
***
#### 函数名
> 函数名称只能使用小写字符或数字。 函数名中的单词必须用`_`分隔。

*Examples*: [function_names](src/function_names.erl)

```erlang
badFunction() -> {not_allowed, camel_case}.

'BAD_FUNCTION'() -> {not_allowed, upper_case}.

good_function() -> ok.

base64_encode() -> ok.
```

*原因*: 函数名称是原子，它们应遵循适用于它们的相同规则。

***
#### 变量名
> 使用驼峰式命名变量. 单词之间不要用下划线分割.

*Examples*: [variable_names](src/variable_names.erl)

```erlang
bad(Variablename, Another_Variable_Name) ->
  [Variablename, Another_Variable_Name].

good(Variable, VariableName) ->
  [Variable, VariableName].
```

*原因*:遵循一个约定可以更容易地在代码周围没有“重复”变量。 Camel-case使变量名称在视觉上与原子更加明显，并且符合OTP标准。

大部分从其他语言转过来可能都习惯了驼峰命名法，可能对函数名 原子也喜欢用，
但是看很多erlang的开源项目，包括OTP自身的代码命名风格的话都是遵循上面这些规则

### 宏

***
#### 宏的应用场景
> 除了包含以下使用方式的情况外，不要使用宏
> * 预定义部分: ``?MODULE``, ``?MODULE_STRING`` and ``?LINE``
> * 魔术数字: ``?DEFAULT_TIMEOUT``

*Examples*: [macros](src/macros.erl)
```erlang
-module(macros).

-define(OTHER_MODULE, other_module).
-define(LOG_ERROR(Error),
        error_logger:error_msg(
          "~p:~p >> Error: ~p~n\tStack: ~p",
          [?MODULE, ?LINE, Error, erlang:get_stacktrace()])).

-define(HTTP_CREATED, 201).

-export([bad/0, good/0]).

bad() ->
  try
    ?OTHER_MODULE:some_function(that, may, fail, 201)
  catch
    _:Error ->
      ?LOG_ERROR(Error)
  end.

good() ->
  try
    other_module:some_function(that, may, fail, ?HTTP_CREATED)
  catch
    _:Error ->
      log_error(?LINE, Error)
  end.

log_error(Line, Error) ->
  error_logger:error_msg(
    "~p:~p >> Error: ~p~n\tStack: ~p",
    [?MODULE, Line, Error, erlang:get_stacktrace()]).
```

*原因*: 宏的使用不利于调试工作的进行. 如果你尝试用它们来避免重复的代码块，可以使用以下函数去实现。
具体看 [related blog post](https://medium.com/@erszcz/when-not-to-use-macros-in-erlang-1d3f10d377f#.xc9b4bsl9) by [@erszcz](https://github.com/erszcz).

***
#### 宏名要大写
> 宏名应以大写字母命名:

*Examples*: [macro_names](src/macro_names.erl)
```erlang
-module(macro_names).

-define(bad, 1).
-define(BADMACRONAME, 2).
-define(Bad_Macro_Name, 3).
-define(Bad_L33t_M@Cr0, 4).

-define(GOOD, 5).
-define(GOOD_MACRO_NAME, 6).
```

*原因*: 这样做可以区分开普通变量和宏,在使用`grep`等工具查找这个宏时不会出现重复宏名,让查找变得更加容易等好处


### 记录(Records)

***
#### 记录命名
> 记录(`record`)命名只能使用小写字母. 单词之间用 `_`分隔. 这个规则同样适用于`record`的字段名

*Examples*: [record_names](src/record_names.erl)

```erlang
-module(record_names).

-export([records/0]).

-record(badName, {}).
-record(bad_field_name, {badFieldName :: any()}).
-record('UPPERCASE', {'THIS_IS_BAD' :: any()}).

-record(good_name, {good_field_name :: any()}).

records() -> [#badName{}, #bad_field_name{}, #'UPPERCASE'{}, #good_name{}].
```

*原因*: `record`和其字段名都是原子(`atom`), 因此跟原子的命名规则是一样的.


#### 在specs里避免出现记录
> 在 specs 里应该尽可能用 `types` 代替 记录(`records`).

*Examples*: [record_spec](src/record_spec.erl)

```erlang
-module(record_spec).

-record(state, {field1:: any(), field2:: any()}).

-opaque state() :: #state{}.

-export_type([state/0]).

-export([bad/1, good/1]).

-spec bad(#state{}) -> {any(), #state{}}.
bad(State) -> {State#state.field1, State}.

-spec good(state()) -> {any(), state()}.
good(State) -> {State#state.field1, State}.
```

*原因*: 类型可以导出使用,同时也有助于文档化, 使用 ``opaque`` 可以对记录进行封装和抽象.

***
####  给记录添加类型Types
> 保持给记录(`record`)的每个字段添加类型定义的习惯

*Examples*: [record_types](src/record_types.erl)

```erlang
-module(record_types).

-export([records/0]).

-record(bad, {no_type}).

-record(good, {with_type :: string(), with_value_type = 1 :: non_neg_integer()}).

records() -> [#bad{}, #good{}].
```

*原因*: 记录(`record`)定义的是数据结构, 而其中最重要的部分之一就是记录组成部分的类型定义.

### 其它

***
#### 给函数添加-spec函数规范定义
*Examples*: [specs](src/specs.erl)

*原因*: 1 便于Dialyzer分析
        2 更容易知道函数的参数类型和返回以及用法

***
#### 模块中不要用import
> Do not use the `-import` directive

*Examples*: [import](src/import.erl)

*原因*:从其他模块导入函数会使代码更难以读取和调试，因为您无法直接区分本地函数和外部函数。
***

***
#### Don't Use Case Catch
> 不要用`case catch` 捕获匹配异常, 使用 `try ... of ... catch` 代替 `case catch`.

*Examples*: [case-catch](src/case_catch.erl)

*原因*: `case catch ...` 把正确的的结果与异常一起处理令人困惑。
        `try ... of ... catch` 把正确的的结果与异常分开处理。

## 好的建议和方法
当我们写代码时，应该考虑以下一些注意事项，但是不要引发PR拒绝，或者含糊到无法连贯执行。

***
### 优先使用高级函数而不是手写的递归方法
> 有时实现函数最好的方式是编写递归函数, 但是比较经常的写法是使用 fold函数 或者 列表推导式 会更加安全和可读性更高.

*Examples*: [alternatives to recursion](src/recursion.erl)

```erlang
-module(recursion).

-export([recurse/1, fold/1, map/1, comprehension/1]).

%%
%% 例子:
%% 不同的方法实现大写字符串
%%

%% 差的: 使用不必要的人工手写递归
recurse(S) ->
    lists:reverse(recurse(S, [])).

recurse([], Acc) ->
    Acc;
recurse([H | T], Acc) ->
    NewAcc = [string:to_upper(H) | Acc],
    recurse(T, NewAcc).

%% 好的: 使用fold函数实现同样的结果,更加安全，更少的代码行数
fold(S) ->
    Result = lists:foldl(fun fold_fun/2, [], S),
    lists:reverse(Result).

fold_fun(C, Acc) ->
    [string:to_upper(C) | Acc].

%% 更佳的: 使用map函数代替fold函数,更简单的实现方法，因为在这种情况下，fold函数大材小用了。
map(S) ->
    lists:map(fun string:to_upper/1, S).

%% 最好的: 在这种情况下，列表推导式最简单的实现方法（假设忽略string:to_upper也能直接对string使用的事实）
comprehension(S) ->
    [string:to_upper(C) || C <- S].
```

*原因*: 人工手写的递归容易出错, 并且代价昂贵。在有错误的情况下，一个错误的递归函数会失去它的基本功能，
如螺旋般地失去控制，导致整个erlang节点挂掉，抵消了erlang最主要的好处之一： 进程的死亡不会导致整个节点的崩溃。

另外，对于一个有经验的erlang开发者而言，folds 和 列表推导式比复杂的递归函数更容易理解。
显而易见的是:它们能为列表中的每个元素执行操作，递归也许同样能够实现，但是它经常需要仔细的检查，以验证控制流在实践中实际执行的路径。

***
### 驼峰式命名，下划线命名
> 符号命名：使用驼峰式命名变量，原子，函数和模块则使用下划线命名
> *Examples*: [camel_case](src/camel_case.erl)
```erlang
-module(camel_case).

-export([bad/0, good/0]).
%% 差的
bad() ->
  Variable_Name = moduleName:functionName(atomConstant),
  another_ModuleName:another_Function_Name(Variable_Name).
%% 好的
good() ->
  VariableName = module_name:function_name(atom_constant),
  another_module_name:another_function_name(VariableName).
```

*小节结论*:本节对下面一个问题很有帮助。

***
### 更短(但仍保持有意义的)的变量名称 

> 只要易于阅读和理解，保持变量名称简短。

*Examples*: [var_names](src/var_names.erl)
```erlang
-module(var_names).

-export([bad/1, good/1]).
%% 差的
bad(OrganizationToken) ->
  OID = organization:get_id(OrganizationToken),
  OID.
%% 好的
good(OrgToken) ->
  OrgID = organization:get_id(OrgToken),
  OrgID.
```

*小节结论*: 它有助于减少每行的长度，这也是上面描述的。

***
### 注释等级

> 模块注释用 **%%%**, 函数注释用 **%%**, 代码注释用 **%**.

*Examples*: [comment_levels](src/comment_levels.erl)

```erlang
% 这样的注释坏到家了
%%% @doc 这样的注释不错
-module(comment_levels).

-export([bad/0, good/0]).

% @doc 这样的注释不好
%%% @doc 这的注释也不好
bad() ->
  R = 1 + 2, %%% 这样的注释不好(not good)
  R. %% 这样的注释依然不好(bad again)

%% @doc 这种注释我喜欢
good() ->
  % 这个注释得到国际注释协会的一致认可
  % 还有 Chuck Norris的认可
  R = 1 + 2,
  R. % This comment (megusta)  这个注释我喜欢（megusta 西班牙语：我喜欢）
```
*小节结论*: 清晰的陈述了注释是什么, 并且寻找特定的注释比如："%% @"等 是非常有用的。
    注释的位置应与被描述的代码相邻，可以放在代码的上方或右方，不可放在下方。 
    修改代码同时修改相应的注释，以保证注释与代码的一致性。不再有用的注释要删除。 
    注释应当准确、易懂，防止注释有二义性。错误的注释不但无益反而有害。 
    当代码比较长，特别是有多重嵌套时，应当在一些段落的结束处加注释，便于阅读。

***
### 保持函数精简
> 只做一件事，尝试着用少量表达式来写函数. 除了集成测试外，每个函数理想的表达式数量是不超过**12**个.

*Examples*: [small_funs](src/small_funs.erl)

*小结*: 从3个方面：
- 简洁的函数有助于是可读性和组装性。可读性又有助于维护。这一点强调的足够多了，你的代码越简洁，就越容易修复和更改。
- 简洁的函数目的清晰明了，因此您只需要了解执行操作的其中一小部分的子集，这使得验证它是否正确地工作变得非常简单。

- 强有力的论据：
  + 一个函数只干一件事情，如果函数太冗长你可能更适合改为以多个函数实现
  + 很明显，简单的，简洁的函数更容易理解
  + 重用性，保持函数的精简有利于后续使用（特别是erlang）
  + 屏幕尺寸：出于如何原因如果通过ssh连接服务器或者，你希望能够看到整个函数


*提示*:

本指导, 联合 **[避免多层嵌套](#avoid-deep-nesting)** and  
**[在case表达式中使用更多更小的函数](#more-smaller-functions-over-case-expressions)** 
两个指导, 可以很好的利用来构建函数，如下所示：

```erlang
some_fun(#state{name=foo} = State) ->
  do_foo_thing(),
  continue_some_fun(State);
some_fun(#state{name=bar} = State) ->
  do_bar_thing(),
  continue_some_fun(State).

continue_some_fun(State) ->
  ...,
  ok.
```
记住这些:

- 像这样在函数结尾调用函数是没有代价的
- 这种模式高效、紧凑、清晰
- 这样重置缩进，因此代码不会游离于屏幕右边边缘地带
最重要的:
- 测试起来简单，因为函数描绘了测试节点.
- 提供更多的跟踪切入面，因此我们能够找到哪里的代码计算运行导致脱轨，而嵌套case写法在运行时是不可跟踪的。


***
### 避免不必要调用length/1 
> 许多用`length/1`作为`case`条件都可以被模式匹配替代掉，尤其在检查列表是否至少有一个元素时很管用。
（要遍历列表，时间长度不定）
*Examples*: [pattern matching](src/pattern_matching.erl)
```erlang
-module(pattern_matching).

-export([bad/1, good/1]).

bad(L) ->
  case length(L) of
    0 -> error;
    _ -> ok
  end.

good([]) ->
  error;
good(_L) ->
  ok.
```
*小结*:模式匹配是`Erlang`的核心内容之一，并且它的性能和可读性都很好。模式匹配也更加灵活，因此它使得代码逻辑变得更加简单。

防坑指南----------------------------------------------------------------------------------------
'--' 运算与 '++'运算 
 > [1,2,3,4] -- [1] -- [2]. 
   [2,3,4]
这是从后面算起的，先算 [1] -- [2] ，得到 [1] 后被 [1,2,3,4] --，最后得到 [2,3,4]
 '++'运算也是一样的，也是从后面开始算起。
>  [1,2,3,4] -- [1] ++ [2,3,4].
   []

++只是lists:append/2的一个别名：如果要用一定要确定 ShortList++LongList

erlang:list_to_binary()
如果参数是多层嵌套结构，就会被扁平化掉，使用 binary_to_list 不能转成原来的数据，也就是不可逆的。
6> list_to_binary([1,2,[3,4],5]) .
<<1,2,3,4,5>>
如果想可逆，可以使用 erlang:term_to_binary
7> binary_to_term(term_to_binary([1,2,[3,4],5])).
[1,2,[3,4],5]

ets:tablist/2在数据比较大的时候尽量少用

erlang 不同数据类型比较
number < atom < reference < fun < port < pid < tuple < list < bit string

Erlang中整数值没有上限值,最大值只是受限于硬件(内存有多大) 但是erlang的浮点数是有上限的遵循 IEEE754

在 guard 模式下，原本会抛出异常的代码就不会报错了，而是结束当前计算并返回false。
在 guard 模式下，erlang是有两种表达方法的： 
1.  ,  和  ;  
2. andalso   和   orelse

然而，这两种表述是有区别的：

首先，假如条件语句是这样的  X >= N; N >= 0，
          当前半句出现异常时候，后面半句还是会执行，而且结果可能回是true;
然后，假如条件语句是这样的   X >= N orelse N >= 0，
          当前半句出现异常时候，后面半句是会被跳过的，返回的结果就是异常
而其实两种都有各自的优缺点，所以，很多情况下都是把他们两种混合起来使用，达到业务需求
最后补充下，when 之后是不允许使用自定义function的，会产生副作用，所以只能是跟整数比较或者是内部的函数，如：is_integer/1,is_atom/1.
顺带讲一下 and andalso or orelse 
and 和 or 两边参与运算的必须是true或者是false才行 返回值也必然是true或者false 但是andalso orelse跟这个是短路求值 是有点差别的 比如下面
(A > 1 orelse io:format("this is run ~n")).
(A > 1 andalso io:format("this is run ~n").
(true andalso io:format("this is run ~n") andalso io:format("this is run2 ~n")).  
(true andalso io:format("this is run ~n") andalso io:format("this is run2 ~n")).  
true andalso false andalso fdfd . 
true andalso false andalso io:format("this is run ~n") orelse io:format("this is run2 ~n").
true andalso false andalso io:format("this is run ~n") andalso io:format("this is run2 ~n").
true andalso false orelse io:format("this is run ~n") andalso io:format("this is run2 ~n"). 




其他的更多的
[那些经历过的Erlang小坑1-10](https://www.cnblogs.com/zhongwencool/p/3712909.html)
[那些经历过的Erlang小坑11-20](http://www.bubuko.com/infodetail-249770.html)
[那些经历过的Erlang小坑21-30](https://www.cnblogs.com/zhongwencool/p/erlang_tip_21_30.html)



