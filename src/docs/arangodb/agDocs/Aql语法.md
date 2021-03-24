 # for语法
    一般语法
        FOR variableName IN expression
    图遍历
        FOR vertexVariableName, edgeVariableName, pathVariableName IN traversalExpression
    视图 特殊关键字
        FOR variableName IN viewName SEARCH searchExpression     
        需要注意 视图不能用作遍历中的边集合
        FOR v IN 1..3 ANY startVertex viewName /* 错误的用法 */
    选项 对于集合和视图，该FOR构造支持可选的OPTIONS 后缀以修改行为。通用语法为：
        FOR variableName IN expression OPTIONS {option: value, ...}
            
    示例
        表达式返回的每个数组元素仅被访问一次。在所有情况下，表达式都必须返回一个数组。空数组也是允许的。当前数组元素可用于由variableName指定的变量中的进一步处理。
        
        FOR u IN users
          RETURN u
        这将遍历数组用户中的所有元素（注意：在这种情况下，此数组由名为“ users”的集合中的所有文档组成），并使当前数组元素在变量u中可用。在此示例中，不对u进行修改，而只是使用RETURN关键字将其推入结果中。
        
        注意：如此处所示，在基于集合的数组上进行迭代时，除非使用SORT 语句定义了明确的排序顺序，否则文档的顺序是不确定的。
        
        引入的变量在关闭放置FOR范围之前一直可用FOR。
        
        另一个示例使用静态声明的值数组进行迭代：
        
        FOR year IN [ 2011, 2012, 2013 ]
          RETURN { "year" : year, "isLeapYear" : year % 4 == 0 && (year % 100 != 0 || year % 400 == 0) }
        FOR也允许嵌套多个语句。当FOR语句嵌套时，FOR 将创建由各个语句返回的数组元素的叉积。
        
        FOR u IN users
          FOR l IN locations
            RETURN { "user" : u, "location" : l }   
            
        索引提示
        对于集合，通过此内联选项机制提供了索引提示。提示可以两种不同的格式指定。
        第一个格式选项是最简单的，只有一个索引名称。对于许多情况，这应该足够了。只要有可能在该FOR循环中使用索引的选择，优化器就会首先检查是否可以使用指定的索引。
        如果是这样，它将使用它，而不管它通常是否使用其他索引。如果它不能使用该索引，则它将退回到其正常逻辑以选择另一个索引。
        如果forceIndexHint: true指定了可选参数，那么它将不会回退，而是会产生错误。
        OPTIONS {indexHint: 'byName'[, forceIndexHint: <boolean>]}
        第二个是按优先顺序排列的索引名称数组。以这种方式指定时，优化器的行为将与上述相同，但是将按照给定的顺序检查每个指定索引的可行性，并退回其正常逻辑或仅在未指定指定条件时失败指标是可行的。
        OPTIONS {indexHint: ['byName', 'byColor'][, forceIndexHint: <boolean>]}         
 
# return语法
     该RETURN语句可用于产生查询结果。必须RETURN在数据选择查询的每个块的末尾指定一条语句，否则查询结果将是不确定的。RETURN在数据修改查询的主级别使用 是可选的。
     一般语法为RETURN：
        RETURN expression or RETURN DISTINCT(如果查询的顶层没有FOR 循环，则不允许在查询的顶层。)
        另外 arangodb3.3开始 return distinct 将不会改变其应用结果的顺序
        
     注意：RETURN将关闭当前作用域并消除其中的所有局部变量。在处理子查询时，记住这一点很重要。   
 
# filter语法
    一般语法 
        FILTER expression
        expression必须是评估为false或true的条件。如果条件结果为false，则将跳过当前元素，因此将不对其进行进一步处理，也不将其作为结果的一部分。
        如果条件为true，则不跳过当前元素，可以对其进行进一步处理。有关条件中可以使用的比较运算符，逻辑运算符等的列表，请参见运算符。\
        另外注意filter的位置 他会影响查询的结果
 
 # search语法
    该SEARCH关键字开始语言结构类型为ArangoSearch的过滤器视图。从概念上讲，视图只是另一个文档数据源，类似于数组或文档/边缘集合，您可以在其上使用AQL中的FOR操作进行迭代：
    可选SEARCH操作提供以下功能：
        1. 根据AQL布尔表达式和函数过滤文档
        2. 匹配位于由快速索引支持的不同集合中的文档
        3. 根据每个文档与搜索条件的匹配程度对结果集进行排序
        
    一般语法
        FOR doc IN viewName
          SEARCH expression OPTIONS {…}
          ...
        SEARCH与相比FILTER，该语句被视为FOR操作的一部分 ，而不是单独的语句。它不能随意放置在查询中，也不能多次放置在FOR循环体内。
        FOR ... IN必须紧随其后的是View的名称，而不是集合的名称。该SEARCH操作必须遵循接下来，其他操作之前SEARCH，如FILTER，
        COLLECT等没有在这个位置上允许的。SEARCH在表达式之后，可以进行后续操作， 但包括SORT根据ArangoSearch View计算的排名值对搜索结果进行排序。
        expression必须是ArangoSearch表达式。在搜索和排序阶段，ArangoSearch的全部功能都通过特殊的ArangoSearch功能加以利用和公开。最重要的是，支持常见的AQL运算符：
        
        AND， &&
        OR， ||
        NOT， !
        ==
        <=
        >=
        <
        >
        !=
        IN （数组或范围），也 NOT IN
        ArangoSearch不会考虑字母的字母顺序，即针对视图的SEARCH操作中的范围查询将不会遵循已定义的Analyzer语言环境或服务器语言（启动选项--default-language）的语言规则！另请参阅已知问题。
        
        FOR doc IN viewName
          SEARCH ANALYZER(doc.text == "quick" OR doc.text == "brown", "text_en")
        RETURN doc
        支持数组比较运算符（在v3.6.0中引入）：
        
        LET tokens = TOKENS("some input", "text_en")                 // ["some", "input"]
        FOR doc IN myView SEARCH tokens  ALL IN doc.title RETURN doc // dynamic conjunction
        FOR doc IN myView SEARCH tokens  ANY IN doc.title RETURN doc // dynamic disjunction
        FOR doc IN myView SEARCH tokens NONE IN doc.title RETURN doc // dynamic negation
        FOR doc IN myView SEARCH tokens  ALL >  doc.title RETURN doc // dynamic conjunction with comparison
        FOR doc IN myView SEARCH tokens  ANY <= doc.title RETURN doc // dynamic disjunction with comparison
        请注意，不支持内联表达式和其他一些功能 SEARCH。如果表达式无效，服务器将引发查询错误。
        
        所述OPTIONS关键字和一个对象可以任选地按照搜索表达式来设置搜索选项。  
        
        处理非索引字段固定链接
        未配置为由视图索引的文档属性被SEARCH视为不存在。这会影响仅从视图发出的文档的测试。
        如果需要，可以使用特殊的includeAllFields View属性为源文档的所有（子）字段建立索引。
        
# sort语法
     SORT expression direction(ASC or DESC)(默认升序ASC)

# limit语法
    LIMIT count
    LIMIT offset, count
      
# let语法
    该LET语句可用于为变量分配任意值。然后将该变量引入LET放置该语句的作用域中。
    通用语法为：
    LET variableName = expression    
    变量在AQL中是不可变的，这意味着它们不能重新分配：   
    
# collect语法   
    COLLECT关键字可用来组由一个或多个基团的标准阵列。
    该COLLECT语句将消除当前范围内的所有局部变量。之后，COLLECT只有由COLLECT自身引入的变量才可用。
    操作有几种语法变体COLLECT：    
        COLLECT variableName = expression
        COLLECT variableName = expression INTO groupsVariable
        COLLECT variableName = expression INTO groupsVariable = projectionExpression
        COLLECT variableName = expression INTO groupsVariable KEEP keepVariable
        COLLECT variableName = expression WITH COUNT INTO countVariable
        COLLECT variableName = expression AGGREGATE variableName = aggregateExpression
        COLLECT variableName = expression AGGREGATE variableName = aggregateExpression INTO groupsVariable
        COLLECT AGGREGATE variableName = aggregateExpression
        COLLECT AGGREGATE variableName = aggregateExpression INTO groupsVariable
        COLLECT WITH COUNT INTO countVariable  
        所有变体都可以选择以OPTIONS { … }子句结尾。   
        
# remove语法
    REMOVE keyExpression IN collection options  
    REMOVE keyExpression IN collection options RETURN OLD     

# udpate语法
    更新操作的两种语法是：
    UPDATE document IN collection options
    UPDATE keyExpression WITH document IN collection options
    
    UPDATE document IN collection options RETURN OLD
    UPDATE document IN collection options RETURN NEW
    UPDATE keyExpression WITH document IN collection options RETURN OLD
    UPDATE keyExpression WITH document IN collection options RETURN NEW、

# replace语法
    REPLACE document IN collection options
    REPLACE keyExpression WITH document IN collection options
    
    REPLACE document IN collection options RETURN OLD
    REPLACE document IN collection options RETURN NEW
    REPLACE keyExpression WITH document IN collection options RETURN OLD
    REPLACE keyExpression WITH document IN collection options RETURN NEW
    
    以下查询使用NEW伪值返回替换的文档（不包含某些系统属性）：
    FOR u IN users
      REPLACE u WITH { value: "test" } IN users
      LET replaced = NEW 
      RETURN UNSET(replaced, '_key', '_id', '_rev')
      
# insert语法
    INSERT document INTO collection [ OPTIONS options ]    
    INSERT document INTO collection RETURN NEW     
    IN关键字可以代替INTO并具有相同的含义。 
    
# upsert语法
    UPSERT searchExpression INSERT insertExpression UPDATE updateExpression IN collection options
    UPSERT searchExpression INSERT insertExpression REPLACE updateExpression IN collection options
    当使用UPDATEupsert操作的变体时，找到的文档将被部分更新，这意味着仅updateExpression中指定的属性将被更新或添加。使用REPLACEupsert 的变体时，现有文档将被updateExpression的上下文替换。
    更新文档将使用服务器生成的值来修改文档的修订号。系统属性_id，_key和_rev无法更新，_from和_to可以更新。
    
# with语法
    WITH managers
    FOR v, e, p IN OUTBOUND 'users/1' usersHaveManagers
      RETURN { v, e, p }      
                 
           
 插入文档
	语法是INSERT document INTO collectionName。该文档是一个对象，您可以从JavaScript或JSON中了解它，它由属性键和值对组成。
	属性键周围的引号在AQL中是可选的。键总是字符序列（字符串），而属性值可以有不同的类型：
	空值
	布尔值（true，false）
	数字（整数和浮点数）
	串
	排列
	宾语

	该LET关键字定义一个带有名称数据的变量和一个对象数组作为值，因此LET variableName = valueExpression表达式是一个文字数组定义[ {...}, {...}, ... ]。

	FOR variableName IN expression用于迭代数据数组的每个元素 。在每个循环中，将一个元素分配给变量d。然后在INSERT语句中使用此变量而不是文字对象定义。基本上是做什么的：

	INSERT {
		"name": "Robert",
		"surname": "Baratheon",
		"alive": false,
		"traits": ["A","H","C"]
	} INTO Characters

	INSERT {
		"name": "Jaime",
		"surname": "Lannister",
		"alive": true,
		"age": 36,
		"traits": ["A","F","B"]
	} INTO Characters

	...
	注意：AQL不允许INSERT在单个查询中针对同一集合的多个操作。然而， 允许它作为FOR循环体，插入多个文档，就像我们对上面的查询所做的那样。

	
	FOR c IN Characters_1 RETURN c
	循环的语法是FOR variableName IN collectionName。对于集合中的每个文档，将为c分配一个文档，然后根据循环体返回该文档。查询返回我们先前存储的所有字符。
	该文档包含我们存储的四个属性，以及数据库系统添加的另外三个属性。每个文档都需要一个唯一的文档_key，用于在集合中标识它。它_id是计算属性，集合名称，
	正斜杠/和文档键的串联。它唯一标识数据库中的文档。_rev是系统管理的修订版ID。

	用户可以在创建文档时提供文档键，也可以自动分配唯一值。它以后不能改变。以下划线开头的所有三个系统属性_都是只读的。

	我们可以使用文档密钥或文档ID在AQL函数的帮助下检索特定文档DOCUMENT()

更新文档
	UPDATE documentKey WITH object IN collectionName  l列出的属性更新指定的文档（存在则添加它们）
	要更新整个文档 整个用replace

	可以用循环 更新或者替换属性
	FOR c IN Character 
		UPDATE c with {swason: 1} IN Character
	
删除文件
	要从集合中删除文档 执行 REMOVE
	REMOVE "201213" IN Character

	FOR C IN Characters
		REMOVE c IN Characters
		
匹配条件
	为了查找满足条件的文档 FILTER AQL
	FOR c IN Characters 
		FILTER c.age >= 12
		RETURN c.name
	
	多种条件
	FOR c IN Characters
		FILTER c.age 《 13
		FILTER c.age != null
		RETURN {name: c.name, age: c.age} 可以用AND运算符
		同时也有OR运算符  
	
限制结果计数
	FOR c IN Characters_1
	LIMIT	5
	RETURN c.name
	FOR c IN Characters 
	LIMIT 2,5
	RETURN c.name
		
	
	
	




	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	