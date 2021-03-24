# 函数名不区分大小写

# ArangoSearch函数

# 数值函数
    AQL提供了一些用于计算的数值函数。支持以下功能：
    
## ABS（）
    ABS(value) → unsignedValue
    
    返回值的绝对部分。
    
    值（数字）：任何数字，正数或负数
    返回unsignedValue（数字）：不带+或-号的数字
    ABS(-5) // 5
    ABS(+5) // 5
    ABS(3.5) // 3.5
## ACOS（）
    ACOS(value) → num
    
    返回value的反余弦值。
    
    值（数字）：输入值
    返回NUM（数|空）：的反余弦值，或空如果值是在有效范围之外-1和1（含）
    ACOS(-1) // 3.141592653589793
    ACOS(0) // 1.5707963267948966
    ACOS(1) // 0
    ACOS(2) // null
## ASIN（）
    ASIN(value) → num
    
    返回value的反正弦值。
    
    值（数字）：输入值
    返回NUM（数|空）：的反正弦值或空值如果值是在有效范围之外-1和1（含）
    ASIN(1) // 1.5707963267948966
    ASIN(0) // 0
    ASIN(-1) // -1.5707963267948966
    ASIN(2) // null
## ATAN（）
    ATAN(value) → num
    
    返回value的反正切值。
    
    值（数字）：输入值
    返回num（数字）：值的反正切
    ATAN(-1) // -0.7853981633974483
    ATAN(0) // 0
    ATAN(10) // 1.4711276743037347
## ATAN2（）
    ATAN2(y, x) → num
    
    返回y和x的商的反正切。
    
    ATAN2(0, 0) // 0
    ATAN2(1, 0) // 1.5707963267948966
    ATAN2(1, 1) // 0.7853981633974483
    ATAN2(-10, 20) // -0.4636476090008061
## AVERAGE（）
    AVERAGE(numArray) → mean
    
    返回array中值的平均值（算术平均值）。
    
    numArray（数组）：数字数组，将忽略空值
    返回均值（number | null）：numArray的平均值。如果数组为空或仅包含null值，则将返回null。
    AVERAGE( [5, 2, 9, 2] ) // 4.5
    AVERAGE( [ -3, -5, 2 ] ) // -2
    AVERAGE( [ 999, 80, 4, 4, 4, 3, 3, 3 ] ) // 137.5
## AVG（）
    这是AVERAGE（）的别名。
    
## CEIL（）
    CEIL(value) → roundedValue
    
    返回最接近但不小于value的整数。
    
    要向下舍入，请参见FLOOR（）。
    要舍入到最接近的整数值，请参见ROUND（）。
    
    值（数字）：任意数字
    返回roundedValue（数字）：将值四舍五入到上限
    CEIL(2.49) // 3
    CEIL(2.50) // 3
    CEIL(-2.50) // -2
    CEIL(-2.51) // -2
## COS（）
    COS(value) → num
    
    返回值的余弦值。
    
    值（数字）：输入值
    返回num（数字）：值的余弦值
    COS(1) // 0.5403023058681398 
    COS(0) // 1
    COS(-3.141592653589783) // -1
    COS(RADIANS(45)) // 0.7071067811865476
## DEGREES（）
    DEGREES(rad) → num
    
    返回从弧度转换为角度的角度。
    
    rad（数字）：输入值
    返回num（数字）：以度为单位的角度
    DEGREES(0.7853981633974483) // 45
    DEGREES(0) // 0
    DEGREES(3.141592653589793) // 180
## EXP（）
    EXP(value) → num
    
    将欧拉常数（2.71828…）提高到value的幂。
    
    值（数字）：输入值
    返回num（数字）：欧拉常数提高到价值的幂
    EXP(1) // 2.718281828459045
    EXP(10) // 22026.46579480671
    EXP(0) // 1
## EXP2（）
    EXP2(value) → num
    
    返回2提高到价值的力量。
    
    值（数字）：输入值
    返回num（数字）：2提高到价值的幂
    EXP2(16) // 65536
    EXP2(1) // 2
    EXP2(0) // 1
# FLOOR（）
    FLOOR(value) → roundedValue
    
    返回最接近但不大于value的整数。
    
    要向上舍入，请参见CEIL（）。
    要舍入到最接近的整数值，请参见ROUND（）。
    
    值（数字）：任意数字
    返回roundedValue（数字）：向下舍入的值
    FLOOR(2.49) // 2
    FLOOR(2.50) // 2
    FLOOR(-2.50) // -3
    FLOOR(-2.51) // -3
## LOG（）
    LOG(value) → num
    
    返回value的自然对数。基数是欧拉常数（2.71828…）。
    
    值（数字）：输入值
    返回num（number | null）：value的自然对数；如果value等于或小于0 ，则返回null
    LOG(2.718281828459045) // 1
    LOG(10) // 2.302585092994046
    LOG(0) // null
## LOG2（）
    LOG2(value) → num
    
    返回value以2为底的对数。
    
    值（数字）：输入值
    返回num（number | null）：value的以2为底的对数；如果value等于或小于0 ，则返回null
    LOG2(1024) // 10
    LOG2(8) // 3
    LOG2(0) // null
## LOG10（）
    LOG10(value) → num
    
    返回value以10为底的对数。
    
    值（数字）：输入值
    返回num（数字）：value的以10为底的对数；如果value等于或小于0 ，则返回null
    LOG10(10000) // 10
    LOG10(10) // 1
    LOG10(0) // null
## MAX（）
    MAX(anyArray) → max
    
    返回anyArray的最大元素。数组不限于数字。另请参阅类型和值顺序。
    
    anyArray（数组）：数字数组，将忽略空值
    返回max（any | null）：具有最大值的元素。如果数组为空或仅包含空值，则该函数将返回null。
    MAX( [5, 9, -2, null, 1] ) // 9
    MAX( [ null, null ] ) // null
    MEDIAN（）
    MEDIAN(numArray) → median
    
    返回array中值的中间值。
    
    对数组进行排序，然后返回中间的元素。如果数组的元素长度均匀，则通过计算平均值（算术平均值）对两个最中心的元素进行插值。
    
    numArray（数组）：数字数组，将忽略空值
    返回中位数（number | null）：numArray的中位数。如果数组为空或仅包含空值，则该函数将返回null。
    MEDIAN( [ 1, 2, 3] ) // 2
    MEDIAN( [ 1, 2, 3, 4 ] ) // 2.5
    MEDIAN( [ 4, 2, 3, 1 ] ) // 2.5
    MEDIAN( [ 999, 80, 4, 4, 4, 3, 3, 3 ] ) // 4
## MIN（）
    MIN(anyArray) → min
    
    返回anyArray的最小元素。数组不限于数字。另请参阅类型和值顺序。
    
    anyArray（数组）：数字数组，将忽略空值
    返回min（any | null）：最小值的元素。如果数组为空或仅包含空值，则该函数将返回null。
    MIN( [5, 9, -2, null, 1] ) // -2
    MIN( [ null, null ] ) // null
## PERCENTILE（）
    PERCENTILE(numArray, n, method) → percentile
    
    返回numArray中值的n个百分点。
    
    numArray（数组）：数字数组，将忽略空值
    n（数字）：必须介于0（排除）和100（包括）之间
    方法（字符串，可选）：“等级”（默认）或“插值”
    返回百分（数|空）：在Ñ百分位数，或空如果数组为空或仅空值包含在它或不能计算百分
    PERCENTILE( [1, 2, 3, 4], 50 ) // 2
    PERCENTILE( [1, 2, 3, 4], 50, "rank" ) // 2
    PERCENTILE( [1, 2, 3, 4], 50, "interpolation" ) // 2.5
## PI（）
    PI() → pi
    
    返回pi。
    
    返回pi（数字）：pi的前几个有效数字（3.141592653589793）
    PI() // 3.141592653589793
## POW（）
    POW(base, exp) → num
    
    将基数返回指数exp。
    
    基本（数字）：基本值
    exp（数字）：指数值
    返回num（数字）：幂值
    POW( 2, 4 ) // 16
    POW( 5, -1 ) // 0.2
    POW( 5, 0 ) // 1
## RADIANS（）
    RADIANS(deg) → num
    
    将角度转换为弧度。
    
    deg（数字）：输入值
    返回num（数字）：以弧度为单位的角度
    RADIANS(180) // 3.141592653589793
    RADIANS(90) // 1.5707963267948966
    RADIANS(0) // 0
## RAND（）
    RAND() → randomNumber
    
    返回介于0和1之间的伪随机数。
    
    返回randomNumber（数字）：大于0且小于1的数字
    RAND() // 0.3503170117504508
    RAND() // 0.6138226173882478
    复杂的例子：
    
    LET coinFlips = (
        FOR i IN 1..100000
        RETURN RAND() > 0.5 ? "heads" : "tails"
    )
    RETURN MERGE(
        FOR flip IN coinFlips
            COLLECT f = flip WITH COUNT INTO count
            RETURN { [f]: count }
    )
    结果：
    
    [
      {
        "heads": 49902,
        "tails": 50098
      }
    ]
## RANGE（）
    RANGE(start, stop, step) → numArray
    
    返回指定范围内的数字数组，可以选择以1以外的增量返回。除非提供了step参数，否则start和stop参数将被截断为整数。
    
    另请参见范围运算符，以了解整数范围和步长为1的范围。
    
    start（数字）：起始范围（包括）的值
    停止（数字）：以（包括）结束范围的值
    step（数字，可选）：每步增加多少，默认值为1.0
    返回numArray（数组）：范围内所有数字作为数组
    RANGE(1, 4) // [ 1, 2, 3, 4 ]
    RANGE(1, 4, 2) // [ 1, 3 ]
    RANGE(1, 4, 3) // [ 1, 4 ]
    RANGE(1.5, 2.5) // [ 1, 2 ]
    RANGE(1.5, 2.5, 1) // [ 1.5, 2.5 ]
    RANGE(1.5, 2.5, 0.5) // [ 1.5, 2, 2.5 ]
    RANGE(-0.75, 1.1, 0.5) // [ -0.75, -0.25, 0.25, 0.75 ]
## ROUND（）
    ROUND(value) → roundedValue
    
    返回最接近value的整数。
    
    值（数字）：任意数字
    返回roundedValue（数字）：将值四舍五入到最接近的整数
    ROUND(2.49) // 2
    ROUND(2.50) // 3
    ROUND(-2.50) // -2
    ROUND(-2.51) // -3
    舍入到零，也称为TRUNC（）在C / C ++，可与组合来实现三元操作， CEIL（） 和FLOOR（） ：
    
    value >= 0 ? FLOOR(value) : CEIL(value)
## SIN（）
    SIN(value) → num
    
    返回值的正弦值。
    
    值（数字）：输入值
    返回num（数字）：值的正弦值
    SIN(3.141592653589783 / 2) // 1
    SIN(0) // 0
    SIN(-3.141592653589783 / 2) // -1
    SIN(RADIANS(270)) // -1
## SQRT（）
    SQRT(value) → squareRoot
    
    返回值的平方根。
    
    值（数字）：一个数字
    返回squareRoot（数字）：值的平方根
    SQRT(9) // 3
    SQRT(2) // 1.4142135623730951
    其他根可以使用POW（）计算，例如POW(value, 1/n)：
    
    // 4th root of 8*8*8*8 = 4096
    POW(4096, 1/4) // 8
    
    // cube root of 3*3*3 = 27
    POW(27, 1/3) // 3
    
    // square root of 3*3 = 9
    POW(9, 1/2) // 3
## STDDEV_POPULATION（）
    STDDEV_POPULATION(numArray) → num
    
    返回array中值的总体标准差。
    
    numArray（数组）：数字数组，将忽略空值
    返回num（number | null）：numArray的总体标准偏差。如果数组为空，或者数组中仅包含空值， 则将返回null。
    STDDEV_POPULATION( [ 1, 3, 6, 5, 2 ] ) // 1.854723699099141
    STDDEV_SAMPLE（）
    STDDEV_SAMPLE(numArray) → num
    
    返回array中值的样本标准偏差。
    
    numArray（数组）：数字数组，将忽略空值
    返回num（number | null）：numArray的样本标准差。如果数组为空，或者数组中仅包含空值， 则将返回null。
    STDDEV_SAMPLE( [ 1, 3, 6, 5, 2 ] ) // 2.0736441353327724
## STDDEV（）
    这是STDDEV_POPULATION（）的别名。
    
## SUM（）
    SUM(numArray) → sum
    
    返回array中的值之和。
    
    numArray（数组）：数字数组，将忽略空值
    返回sum（number）：numArray中所有值的总和。如果数组为空或仅包含空值，则将返回0。
    SUM( [1, 2, 3, 4] ) // 10
    SUM( [null, -5, 6] ) // 1
    SUM( [ ] ) // 0
## TAN（）
    TAN(value) → num
    
    返回值的正切值。
    
    值（数字）：输入值
    返回num（数字）：值的正切值
    TAN(10) // 0.6483608274590866
    TAN(5) // -3.380515006246586
    TAN(0) // 0
    VARIANCE_POPULATION（）
    VARIANCE_POPULATION(numArray) → num
    
    返回array中值的总体方差。
    
    numArray（数组）：数字数组，将忽略空值
    返回num（number | null）：numArray的总体方差。如果数组为空，或者数组中仅包含空值， 则将返回null。
    VARIANCE_POPULATION( [ 1, 3, 6, 5, 2 ] ) // 3.4400000000000004
    VARIANCE_SAMPLE（）
    VARIANCE_SAMPLE(array) → num
    
    返回array中值的样本方差。
    
    numArray（数组）：数字数组，将忽略空值
    返回num（number | null）：numArray的样本方差。如果数组为空，或者数组中仅包含空值， 则将返回null。
    VARIANCE_SAMPLE( [ 1, 3, 6, 5, 2 ] ) // 4.300000000000001
## VARIANCE（）
    这是VARIANCE_POPULATION（）的别名。

# 字符串函数
## CHAR_LENGTH（）永久链接
    CHAR_LENGTH(value) → length
    返回的字符数值（未字节长度）。  
    输入值	长度
    串	Unicode字符数
    数	代表数字的Unicode字符数
    数组	结果字符串化中的Unicode字符数
    宾语	结果字符串化中的Unicode字符数
    真正	4
    假	5
    空值	0
    
## CONCAT（）
    CONCAT(value1, value2, ... valueN) → str
    
    将作为value1传递的值连接到valueN。
    
    值（任意，可重复）：任意类型的元素（至少1个）
    返回str（字符串）：元素的串联。空值将被忽略。
    CONCAT("foo", "bar", "baz") // "foobarbaz"
    CONCAT(1, 2, 3) // "123"
    CONCAT("foo", [5, 6], {bar: "baz"}) // "foo[5,6]{\"bar\":\"baz\"}"
    CONCAT(anyArray) → str
    
    如果将单个数组传递给CONCAT（），则将其成员连接在一起。
    
    anyArray（array）：具有任意类型元素的数组
    返回str（字符串）：数组元素的串联。空值将被忽略。
    CONCAT( [ "foo", "bar", "baz" ] ) // "foobarbaz"
    CONCAT( [1, 2, 3] ) // "123"    

## CONCAT_SEPARATOR（）永久链接
    CONCAT_SEPARATOR(separator, value1, value2, ... valueN) → joinedString
    
    使用 分隔符字符串将作为参数value1传递的字符串连接到valueN。
    
    分隔符（字符串）：任意分隔符字符串
    值（字符串|数组，可重复）：字符串或字符串数​​组作为多个参数（至少1个）
    返回joindString（字符串）：元素的连接字符串，使用 分隔符作为分隔符字符串。空值将被忽略。数组值参数将自动展开，并且它们的各个成员将被串联。嵌套数组也将被扩展，但是如果它们具有多个元素，则其元素之间用逗号分隔。
    CONCAT_SEPARATOR(", ", "foo", "bar", "baz")
    // "foo, bar, baz"
    
    CONCAT_SEPARATOR(", ", [ "foo", "bar", "baz" ])
    // "foo, bar, baz"
    
    CONCAT_SEPARATOR(", ", [ "foo", [ "b", "a", "r" ], "baz" ])
    // [ "foo, b,a,r, baz" ]
    
    CONCAT_SEPARATOR("-", [1, 2, 3, null], [4, null, 5])
    // "1-2-3-4-5"

## CONTAINS（）
    CONTAINS(text, search, returnIndex) → match
    
    检查字符串是否搜索包含在字符串文本。CONTAINS执行的字符串匹配区分大小写。
    
    文本（字符串）：大海捞针
    搜索（字符串）：针
    returnIndex（布尔型，可选）：如果设置为true，则返回匹配项的字符位置，而不是布尔值。默认值为false。
    返回匹配（布尔|号码）：默认情况下，真正的如果返回搜索 包含在文本和假否则。与returnIndex设置为真，第一次出现的位置搜索中的文本被返回（在偏移0开始），或-1，如果搜索未在包含文本。
    CONTAINS("foobarbaz", "bar") // true
    CONTAINS("foobarbaz", "horse") // false
    CONTAINS("foobarbaz", "ba", true) // 3
    CONTAINS("foobarbaz", "horse", true) // -1
    要确定值是否包含在数组中或在数组的哪个位置，请参见 POSITION（）数组函数。

## COUNT（）
    这是LENGTH（）的别名。
    
## CRC32（）
    CRC32(text) → hash
    
    计算文本的CRC32校验和，并以十六进制字符串表示形式返回。使用的多项式为0x1EDC6F41。使用的初始值为0xFFFFFFFF，最终的异或值也是0xFFFFFFFF。
    
    文字（字串）：字串
    返回哈希（字符串）：CRC32校验和为十六进制字符串
    CRC32("foobar") // "D5F5C7F"
    
## ENCODE_URI_COMPONENT（）
    ENCODE_URI_COMPONENT(value) → encodedURIComponentString
    
    返回value的已编码uri分量。
    
    值（字符串）：字符串
    返回已编码的URIComponentString（字符串）：值的已编码的uri组件    
    
## FIND_FIRST（）
    FIND_FIRST(text, search, start, end) → position
    
    返回字符串文本中第一次出现的字符串搜索的位置。位置从0开始。
    
    文本（字符串）：大海捞针
    搜索（字符串）：针
    启动（数，可选）：限制搜索到的文本的一个子集，在开始启动
    end（数字，可选）：将搜索限制为文本的子集，以end结尾
    返回位置（数字）：匹配的字符位置。如果text中 不包含搜索，则返回-1。如果搜索为空，则返回开始。
    FIND_FIRST("foobarbaz", "ba") // 3
    FIND_FIRST("foobarbaz", "ba", 4) // 6
    FIND_FIRST("foobarbaz", "ba", 0, 3) // -1
    
## FIND_LAST（）
    FIND_LAST(text, search, start, end) → position
    
    返回字符串文本中最后一次出现字符串搜索的位置。位置从0开始。
    
    文本（字符串）：大海捞针
    搜索（字符串）：针
    启动（数，可选）：限制搜索到的文本的一个子集，在开始启动
    end（数字，可选）：将搜索限制为文本的子集，以end结尾
    返回位置（数字）：匹配的字符位置。如果text中 不包含搜索，则返回-1。如果搜索为空，则返回字符串长度，或结束 + 1。
    FIND_LAST("foobarbaz", "ba") // 6
    FIND_LAST("foobarbaz", "ba", 7) // -1
    FIND_LAST("foobarbaz", "ba", 0, 4) // 3
    
## FNV64（）
    FNV64(text) → hash 
    
    计算文本的FNV-1A 64位哈希，并以十六进制字符串表示形式返回。
    
    文字（字串）：字串
    返回哈希（字符串）：FNV-1A哈希作为十六进制字符串
    FNV64("foobar") // "85944171F73967E8"
    
## JSON_PARSE（）
    JSON_PARSE(text) → value
    
    返回由JSON编码的输入字符串描述的AQL值。
    
    text（字符串）：要解析为JSON的字符串
    返回值（混合）：与给定JSON文本对应的值。对于不是有效JSON字符串的输入值，该函数将返回null。
    JSON_PARSE("123") // 123
    JSON_PARSE("[ true, false, 2 ]") // [ true, false, 2 ]
    JSON_PARSE("\\\"abc\\\"") // "abc"
    JSON_PARSE("{\\\"a\\\": 1}") // { a : 1 }
    JSON_PARSE("abc") // null
    
## JSON_STRINGIFY（）
    JSON_STRINGIFY(value) → text
    
    返回输入值的JSON字符串表示形式。
    
    值（混合）：要转换为JSON字符串的值
    返回text（string）：表示value的JSON字符串。对于无法转换为JSON的输入值，该函数将返回null。
    JSON_STRINGIFY("1") // "1"
    JSON_STRINGIFY("abc") // "\"abc\""
    JSON_STRINGIFY("[1, 2, 3]") // "[1,2,3]"
    
## LEFT（）
    LEFT(value, n) → substring
    
    返回字符串值的最左边的n个字符。
    
    要返回最右边的字符，请参见RIGHT（）。
    要从字符串的任意位置移开一部分，请参见SUBSTRING（）。
    
    值（字符串）：字符串
    n（数字）：要返回的字符数
    返回子字符串（字符串）：从字符串的左侧开始，最多为n个value字符
    LEFT("foobar", 3) // "foo"
    LEFT("foobar", 10) // "foobar"
    
## LENGTH（）
    LENGTH(str) → length
    
    确定字符串的字符长度。
    
    str（字符串）：字符串。如果传递了一个数字，它将首先转换为字符串。
    返回长度（数字）：str的字符长度（不是字节长度）
    LENGTH("foobar") // 6
    LENGTH("电脑坏了") // 4
    LENGTH（）还可以确定数组中元素的数量，对象/文档的属性键的数量以及集合中文档的数量。
    
## LEVENSHTEIN_DISTANCE（）
    LEVENSHTEIN_DISTANCE(value1, value2) → levenshteinDistance
    
    计算 两个字符串之间的Levenshtein距离。
    
    value1（字符串）：一个字符串
    value2（字符串）：字符串
    返回levenshteinDistance（数字）：计算输入字符串value1和value2之间的Levenshtein距离
    LEVENSHTEIN_DISTANCE("foobar", "bar") // 3
    LEVENSHTEIN_DISTANCE(" ", "") // 1
    LEVENSHTEIN_DISTANCE("The quick brown fox jumps over the lazy dog", "The quick black dog jumps over the brown fox") // 13
    LEVENSHTEIN_DISTANCE("der mötör trötet", "der trötet") // 6
    
## LIKE（）
    LIKE(text, search, caseInsensitive) → bool
    
    使用通配符匹配，检查模式搜索是否包含在字符串文本中。
    
    文本（字符串）：要搜索的字符串
    搜索（字符串）：一种搜索模式，可以包含通配符 %（意味着任何字符序列，包括无字符）和_（任何单个字符）。从字面来看%，_必须使用两个反斜杠（在arangosh中为四个反斜杠）进行转义。 搜索不能是变量或文档属性。实际值必须已经存在于查询解析时。
    caseInsensitive（布尔型，可选）：如果设置为true，则匹配将不区分大小写。默认值为false。
    返回bool类型：（布尔）真如果该模式包含在文本和虚假，否则
    LIKE("cart", "ca_t")   // true
    LIKE("carrot", "ca_t") // false
    LIKE("carrot", "ca%t") // true
    
    LIKE("foo bar baz", "bar")   // false
    LIKE("foo bar baz", "%bar%") // true
    LIKE("bar", "%bar%")         // true
    
    LIKE("FoO bAr BaZ", "fOo%bAz")       // false
    LIKE("FoO bAr BaZ", "fOo%bAz", true) // true
    
## LOWER（）
    LOWER(value) → lowerCaseString
    
    将值中的大写字母转换为小写字母。其他所有字符均保持不变。
    
    值（字符串）：字符串
    返回lowerCaseString（字符串）值与大写字符转换为小写字符
## LTRIM（）
    LTRIM(value, chars) → strippedString
    
    返回仅从开头去除空格的字符串值。
    
    要仅从末尾剥离，请参见RTRIM（）。
    要剥离两侧，请参见TRIM（）。
    
    值（字符串）：字符串
    chars（字符串，可选）：覆盖应从字符串中删除的字符。它默认为\r\n \t（即0x0d，0x0a， 0x20和0x09）。
    返回strippedString（字符串）：左侧不带字符的值
    LTRIM("foo bar") // "foo bar"
    LTRIM("  foo bar  ") // "foo bar  "
    LTRIM("--==[foo-bar]==--", "-=[]") // "foo-bar]==--"

## MD5（）
    MD5(text) → hash
    
    计算文本的MD5校验和，并以十六进制字符串表示形式返回。
    
    文字（字串）：字串
    返回哈希（字符串）：MD5校验和为十六进制字符串
    MD5("foobar") // "3858f62230ac3c915f300c664312c63f"
    
## RANDOM_TOKEN（）
    RANDOM_TOKEN(length) → randomString
    
    生成具有指定长度的伪随机令牌字符串。令牌生成算法应视为不透明。
    
    length（数字）：令牌所需的字符串长度。它必须大于或等于0，并且最大为65536。长度为0会返回一个空字符串。
    返回randomString（字符串）：生成的令牌，由小写字母，大写字母和数字组成
    RANDOM_TOKEN(8) // "zGl09z42"
    RANDOM_TOKEN(8) // "m9w50Ft9"
## REGEX_MATCHES（）
    REGEX_MATCHES(text, regex, caseInsensitive) → stringArray
    
    使用正则表达式返回给定字符串文本中的匹配项。
    
    文本（字符串）：要搜索的字符串
    regex（字符串）：用于匹配文本的正则表达式
    返回stringArray（array）：包含匹配项的字符串数组
    正则表达式可以由文字字符以及以下字符和序列组成：
    
    .–点匹配除行终止符之外的任何单个字符。包括行终止符，使用[\s\S]替代模拟.与DOTALL标志。
    \d –匹配一个数字，等于 [0-9]
    \s –匹配单个空格字符
    \S –匹配一个非空白字符
    \t –匹配制表符
    \r –匹配回车
    \n –匹配换行符
    [xyz]–字符集。匹配任何封闭的字符（此处为x，y或z）
    [^xyz]–否定的字符集。匹配除封闭字符以外的任何其他字符（在这种情况下，即x，y或z以外的任何其他字符）
    [x-z]–字符范围。匹配指定范围内的[0-9A-F]任何字符，例如，匹配0123456789ABCDEF中的任何字符
    [^x-z]–字符范围取反。匹配范围内指定字符以外的任何其他字符
    (xyz) –定义并匹配模式组
    (x|y)–匹配x或y
    ^–匹配字符串的开头（例如^xyz）
    $–匹配字符串的结尾（例如xyz$）
    需要注意的是人物.，*，?，[，]，(，)，{，}，^，和$在正则表达式特殊含义，可能需要使用反斜杠进行转义，这需要转义本身（\\）。文字反斜杠需要使用另一个转义的反斜杠进行转义\\\\。在arangosh中，反斜杠的数量需要加倍。
    
    可以使用以下量词来重复字符和序列：
    
    x*–匹配x的零个或多个出现
    x+–匹配x的一个或多个出现
    x?–匹配x的一次或零次出现
    x{y}–精确匹配x的y个出现
    x{y,z}–匹配x的y和z出现
    x{y,}–匹配至少y次出现的x
    请注意，它xyz+匹配xyzzzzz，但是如果要匹配xyzxyz，则需要通过将子表达式包装在括号中并将量词放在其后面来定义模式组(xyz)+。
    
    如果regex中的正则表达式无效，则会引发警告，并且该函数将返回null。
    
    REGEX_MATCHES("My-us3r_n4m3", "^[a-z0-9_-]{3,16}$", true) // ["My-us3r_n4m3"]
    REGEX_MATCHES("#4d82h4", "^#?([a-f0-9]{6}|[a-f0-9]{3})$", true) // null
    REGEX_MATCHES("john@doe.com", "^([a-z0-9_\.-]+)@([\da-z-]+)\.([a-z\.]{2,6})$", false) // ["john@doe.com", "john", "doe", "com"]
## REGEX_SPLIT（）
    REGEX_SPLIT(text, splitExpression, caseInsensitive, limit) → stringArray
    
    使用分隔符将给定的字符串文本拆分为字符串列表。
    
    文本（字符串）：要分割的字符串
    splitExpression（字符串）：用于拆分文本的正则表达式
    限制（数字，可选）：限制结果中分割值的数量。如果未给出限制，则返回的拆分数不受限制。
    返回stringArray（array）：字符串数组
    正则表达式可以由文字字符以及以下字符和序列组成：
    
    .–点匹配除行终止符之外的任何单个字符。包括行终止符，使用[\s\S]替代模拟.与DOTALL标志。
    \d –匹配一个数字，等于 [0-9]
    \s –匹配单个空格字符
    \S –匹配一个非空白字符
    \t –匹配制表符
    \r –匹配回车
    \n –匹配换行符
    [xyz]–字符集。匹配任何封闭的字符（此处为x，y或z）
    [^xyz]–否定的字符集。匹配除封闭字符以外的任何其他字符（在这种情况下，即x，y或z以外的任何其他字符）
    [x-z]–字符范围。匹配指定范围内的[0-9A-F]任何字符，例如，匹配0123456789ABCDEF中的任何字符
    [^x-z]–字符范围取反。匹配范围内指定字符以外的任何其他字符
    (xyz) –定义并匹配模式组
    (x|y)–匹配x或y
    ^–匹配字符串的开头（例如^xyz）
    $–匹配字符串的结尾（例如xyz$）
    需要注意的是人物.，*，?，[，]，(，)，{，}，^，和$在正则表达式特殊含义，可能需要使用反斜杠进行转义，这需要转义本身（\\）。文字反斜杠需要使用另一个转义的反斜杠进行转义\\\\。在arangosh中，反斜杠的数量需要加倍。
    
    可以使用以下量词来重复字符和序列：
    
    x*–匹配x的零个或多个出现
    x+–匹配x的一个或多个出现
    x?–匹配x的一次或零次出现
    x{y}–精确匹配x的y个出现
    x{y,z}–匹配x的y和z出现
    x{y,}–匹配至少y次出现的x
    请注意，它xyz+匹配xyzzzzz，但是如果要匹配xyzxyz，则需要通过将子表达式包装在括号中并将量词放在其后面来定义模式组(xyz)+。
    
    如果splitExpression中的正则表达式无效，则会引发警告，并且该函数将返回null。
    
    REGEX_SPLIT("This is a line.\n This is yet another line\r\n This again is a line.\r Mac line ", "\.?(\n|\r|\r\n)", true, 4) // ["This is a line", "\n", " This is yet another lin", "\r"]
    REGEX_SPLIT("hypertext language, programming", "[\s, ]+") // ["hypertext", "language", "programming"]
    REGEX_SPLIT("ca,bc,a,bca,bca,bc", "a,b", true, 5) // ["c", "c,", "c", "c", "c"]
## REGEX_TEST（）
    REGEX_TEST(text, search, caseInsensitive) → bool
    
    使用正则表达式匹配检查模式搜索是否包含在字符串文本中。
    
    文本（字符串）：要搜索的字符串
    搜索（字符串）：正则表达式搜索模式
    返回bool类型：（布尔）真如果该模式包含在文本和虚假，否则
    caseInsensitive（布尔型，可选）：如果设置为true，则匹配将不区分大小写。默认值为false。
    正则表达式可以由文字字符以及以下字符和序列组成：
    
    .–点匹配除行终止符之外的任何单个字符。包括行终止符，使用[\s\S]替代模拟.与DOTALL标志。
    \d –匹配一个数字，等于 [0-9]
    \s –匹配单个空格字符
    \S –匹配一个非空白字符
    \t –匹配制表符
    \r –匹配回车
    \n –匹配换行符
    [xyz]–字符集。匹配任何封闭的字符（此处为x，y或z）
    [^xyz]–否定的字符集。匹配除封闭字符以外的任何其他字符（在这种情况下，即x，y或z以外的任何其他字符）
    [x-z]–字符范围。匹配指定范围内的[0-9A-F]任何字符，例如，匹配0123456789ABCDEF中的任何字符
    [^x-z]–字符范围取反。匹配范围内指定字符以外的任何其他字符
    (xyz) –定义并匹配模式组
    (x|y)–匹配x或y
    ^–匹配字符串的开头（例如^xyz）
    $–匹配字符串的结尾（例如xyz$）
    需要注意的是人物.，*，?，[，]，(，)，{，}，^，和$在正则表达式特殊含义，可能需要使用反斜杠进行转义，这需要转义本身（\\）。文字反斜杠需要使用另一个转义的反斜杠进行转义\\\\。在arangosh中，反斜杠的数量需要加倍。
    
    可以使用以下量词来重复字符和序列：
    
    x*–匹配x的零个或多个出现
    x+–匹配x的一个或多个出现
    x?–匹配x的一次或零次出现
    x{y}–精确匹配x的y个出现
    x{y,z}–匹配x的y和z出现
    x{y,}–匹配至少y次出现的x
    请注意，它xyz+匹配xyzzzzz，但是如果要匹配xyzxyz，则需要通过将子表达式包装在括号中并将量词放在其后面来定义模式组(xyz)+。
    
    如果搜索中的正则表达式无效，则会引发警告，并且该函数将返回null。
    
    REGEX_TEST("the quick brown fox", "the.*fox") // true
    REGEX_TEST("the quick brown fox", "^(a|the)\s+(quick|slow).*f.x$") // true
    REGEX_TEST("the\nquick\nbrown\nfox", "^the(\n[a-w]+)+\nfox$") // true
## REGEX_REPLACE（）
    REGEX_REPLACE(text, search, replacement, caseInsensitive) → string
    
    使用正则表达式匹配，将模式搜索替换为字符串 文本中的字符串替换。
    
    文本（字符串）：要搜索的字符串
    搜索（字符串）：正则表达式搜索模式
    替换（字符串）：用替换搜索模式的字符串
    返回字符串（字符串）：具有搜索正则表达式模式的字符串文本，无论该模式存在于文本中的何处，均被替换字符串替换
    caseInsensitive（布尔型，可选）：如果设置为true，则匹配将不区分大小写。默认值为false。
    有关字符和序列规则的更多详细信息，请参见 REGEX_TEST（）。
    
    如果搜索中的正则表达式无效，则会引发警告，并且该函数将返回null。
    
    REGEX_REPLACE("the quick brown fox", "the.*fox", "jumped over") // jumped over
    REGEX_REPLACE("the quick brown fox", "o", "i") // the quick briwn fix
## REVERSE（）
    REVERSE(value) → reversedString
    
    返回字符串值的反方向。
    
    值（字符串）：字符串
    返回reversedString（string）：一个新字符串，其中字符的顺序相反
    REVERSE("foobar") // "raboof"
    REVERSE("电脑坏了") // "了坏脑电"
## RIGHT（）
    RIGHT(value, length) → substring
    
    返回字符串值的最右边字符的长度。
    
    要返回最左边的字符，请参见LEFT（）。
    要从字符串的任意位置移开一部分，请参见SUBSTRING（）。
    
    值（字符串）：字符串
    长度（数字）：返回多少个字符
    返回子字符串（字符串）：最长为value的字符，从字符串的右侧开始
    RIGHT("foobar", 3) // "bar"
    RIGHT("foobar", 10) // "foobar"
## RTRIM（）
    RTRIM(value, chars) → strippedString
    
    返回仅从末尾去除空格的字符串值。
    
    要仅从头开始剥离，请参见LTRIM（）。
    要剥离两侧，请参见TRIM（）。
    
    值（字符串）：字符串
    chars（字符串，可选）：覆盖应从字符串中删除的字符。它默认为\r\n \t（即0x0d，0x0a， 0x20和0x09）。
    返回strippedString（string）：右侧不带字符的值
    RTRIM("foo bar") // "foo bar"
    RTRIM("  foo bar  ") // "  foo bar"
    RTRIM("--==[foo-bar]==--", "-=[]") // "--==[foo-bar"
## SHA1（）
    SHA1(text) → hash
    
    计算文本的SHA1校验和，并以十六进制字符串表示形式返回。
    
    文字（字串）：字串
    返回哈希（字符串）：SHA1校验和为十六进制字符串
    SHA1("foobar") // "8843d7f92416211de9ebb963ff4ce28125932878"
## SHA512（）
    SHA512(text) → hash
    
    计算文本的SHA512校验和，并以十六进制字符串表示形式返回。
    
    文字（字串）：字串
    返回哈希（字符串）：SHA512校验和作为十六进制字符串
    SHA512("foobar") // "0a50261ebd1a390fed2bf326f2673c145582a6342d523204973d0219337f81616a8069b012587cf5635f6925f1b56c360230c19b273500ee013e030601bf2425"
## SPLIT（）
    SPLIT(value, separator, limit) → strArray
    
    使用分隔符将给定的字符串值拆分为字符串列表。
    
    值（字符串）：字符串
    分隔符（字符串）：字符串或字符串列表。如果分隔符是一个空字符串，则值将被分成一个字符列表。如果未 指定分隔符，则值将作为数组返回。
    限制（数字，可选）：限制结果中分割值的数量。如果未给出限制，则返回的拆分数不受限制。
    返回strArray（array）：字符串数组
    SPLIT( "foo-bar-baz", "-" ) // [ "foo", "bar", "baz" ]
    SPLIT( "foo-bar-baz", "-", 1 ) // [ "foo" ]
    SPLIT( "foo, bar & baz", [ ", ", " & " ] ) // [ "foo", "bar", "baz" ]
## SOUNDEX（）
    SOUNDEX(value) → soundexString
    
    返回值的soundex指纹。
    
    值（字符串）：字符串
    返回soundexString（字符串）：值的soundex指纹
    SOUNDEX( "example" ) // "E251"
    SOUNDEX( "ekzampul")  // "E251"
    SOUNDEX( "soundex" ) // "S532"
    SOUNDEX( "sounteks" ) // "S532"
## SUBSTITUTE（）
    SUBSTITUTE(value, search, replace, limit) → substitutedString
    
    将搜索值替换为字符串值。
    
    值（字符串）：字符串
    搜索（字符串|数组）：如果搜索是字符串，则所有出现的 搜索都将替换为value。如果search是字符串数组，则每次在search中包含的值都将替换为replace中的相应数组元素。如果replace的列表项少于search，则未映射的搜索项的出现将由空字符串替换。
    replace（字符串|数组，可选）：替换字符串，或用于替换搜索中相应元素的字符串数组。元素可以少于搜索，或者可以删除以删除匹配项。如果search是一个数组，而replace是一个字符串，则所有匹配项都将替换为replace。
    限制（数量，可选）：将替换数量限制为该值
    返回replaceString（string）：一个新字符串，其中匹配项已替换（或移除）
    SUBSTITUTE( "the quick brown foxx", "quick", "lazy" )
    // "the lazy brown foxx"
    
    SUBSTITUTE( "the quick brown foxx", [ "quick", "foxx" ], [ "slow", "dog" ] )
    // "the slow brown dog"
    
    SUBSTITUTE( "the quick brown foxx", [ "the", "foxx" ], [ "that", "dog" ], 1 )
    //  "that quick brown foxx"
    
    SUBSTITUTE( "the quick brown foxx", [ "the", "quick", "foxx" ], [ "A", "VOID!" ] )
    // "A VOID! brown "
    
    SUBSTITUTE( "the quick brown foxx", [ "quick", "foxx" ], "xx" )
    // "the xx brown xx"
    SUBSTITUTE(value, mapping, limit) → substitutedString
    
    或者，可以在组合值中指定搜索和替换。
    
    值（字符串）：字符串
    映射（对象）：查找图，其中搜索字符串为键，替换字符串为值。空字符串和null（作为值）将删除匹配项。请注意，由此不能保证搜索字符串的顺序；意思是，如果搜索结果重叠，则前者可能会赢一次，而第二则可能会赢。如果需要确保序列的优先级，请选择基于数组的调用方法。
    限制（数量，可选）：将替换数量限制为该值
    返回replaceString（string）：一个新字符串，其中匹配项已替换（或移除）
    SUBSTITUTE("the quick brown foxx", {
      "quick": "small",
      "brown": "slow",
      "foxx": "ant"
    })
    // "the small slow ant"
    
    SUBSTITUTE("the quick brown foxx", { 
      "quick": "",
      "brown": null,
      "foxx": "ant"
    })
    // "the   ant"
    
    SUBSTITUTE("the quick brown foxx", {
      "quick": "small",
      "brown": "slow",
      "foxx": "ant"
    }, 2)
    // "the small slow foxx"
## SUBSTRING（）
    SUBSTRING(value, offset, length) → substring
    
    返回value的子字符串。
    
    要返回最右边的字符，请参见RIGHT（）。
    要返回最左边的字符，请参见LEFT（）。
    
    值（字符串）：字符串
    偏移量（数字）：从偏移量开始，偏移量从位置0开始
    长度（数字，可选）：最多为长度个字符，省略以获取从偏移量到字符串末尾的子字符串
    返回子字符串（字符串）：值的子字符串
## TOKENS（）
    请参阅ArangoSearch函数。
    
## TO_BASE64（）
    TO_BASE64(value) → toBase64String
    
    返回value的base64表示形式。
    
    值（字符串）：字符串
    返回toBase64String（字符串）：值的base64表示形式
## TO_HEX（）
    TO_HEX(value) → toHexString
    
    返回value的十六进制表示形式。
    
    值（字符串）：字符串
    返回toHexString（字符串）：值的十六进制表示
## TRIM（）
    TRIM(value, type) → strippedString
    
    返回从开头和/或结尾去除空格的字符串值。
    
    可选的type参数指定从字符串的哪一部分除去空白。但是，首选LTRIM（） 和RTRIM（）。
    
    值（字符串）：字符串
    类型（数字，可选）：从
    0 –字符串的开头和结尾（默认）
    1 –仅字符串开头
    2 –仅字符串结尾
    TRIM(value, chars) → strippedString
    
    返回从开头和结尾去除空格的字符串值。
    
    值（字符串）：字符串
    chars（字符串，可选）：覆盖应从字符串中删除的字符。它默认为\r\n \t（即0x0d，0x0a， 0x20和0x09）。
    返回strippedString（字符串）：两端均不带字符的值
    TRIM("foo bar") // "foo bar"
    TRIM("  foo bar  ") // "foo bar"
    TRIM("--==[foo-bar]==--", "-=[]") // "foo-bar"
    TRIM("  foobar\t \r\n ") // "foobar"
    TRIM(";foo;bar;baz, ", ",; ") // "foo;bar;baz"
## UPPER（）
    UPPER(value) → upperCaseString
    
    将值中的小写字母转换为大写字母。其他所有字符均保持不变。
    
    值（字符串）：字符串
    返回upperCaseString（字符串）值与小写字符转换为大写字符
## UUID（）
    UUID() → UUIDString
    
    返回一个通用的唯一标识符值。
    
    返回UUIDString（字符串）：通用唯一标识符

# 类型检查和转换

## TO_BOOL（）
    TO_BOOL(value) → bool
    
    接受任何类型的输入值，并将其转换为适当的布尔值。
    
    值（任意）：任意类型的输入
    返回布尔值（布尔值）：
    null转换为false
    数字将转换为true，但0除外，该数字将转换为false
    字符串转化成真正的，如果他们都是非空的，并假以其他方式
    数组始终转换为true（即使为空）
    对象/文档始终转换为true
    也可以使用双重否定转换为布尔值：
    
    !!1 // true
    !!0 // false
    !!-0.0 // false
    not not 1 // true
    !!"non-empty string" // true
    !!"" // false
    TO_BOOL() 但是，“首选”是首选，因为它可以使意图更清晰。
    
## TO_NUMBER()
    TO_NUMBER(value) → number
    
    取任何类型的输入值并将其转换为数字值。
    
    值（任意）：任意类型的输入
    返回数字（数字）：
    null和false转换为值0
    true转换为1
    数字保持其原始价值
    如果字符串包含数字的有效表示形式，则将字符串转换为它们的数字等效形式。字符串开头和结尾的空格是允许的。不包含任何有效数字表示形式的字符串值将转换为数字0。
    将空数组转换为0，将具有一个成员的数组转换TO_NUMBER()为其唯一成员的结果。具有两个或更多成员的数组将转换为数字0。
    对象/文档将转换为数字0。
    
    一元加号也会强制转换为数字，但这TO_NUMBER()是首选方式：
    
    +'5' // 5
    +[8] // 8
    +[8,9] // 0
    +{} // 0
    一元减号的工作原理与此类似，但数值也取反：
    
    -'5' // -5
    -[8] // -8
    -[8,9] // 0
    -{} // 0
    
## TO_STRING()
    TO_STRING(value) → str
    
    接受任何类型的输入值，并将其转换为字符串值。
    
    值（任意）：任意类型的输入
    返回str（字符串）：
    null转换为空字符串""
    false转换为字符串“ false”，true转换为字符串“ true”
    数字将转换为其字符串表示形式。这也可以是科学记数法（例如“ 2e-7”）
    数组和对象/文档将转换为字符串表示形式，这意味着JSON编码的字符串没有额外的空格
    TO_STRING(null) // ""
    TO_STRING(true) // "true"
    TO_STRING(false) // "false"
    TO_STRING(123) // "123"
    TO_STRING(+1.23) // "1.23"
    TO_STRING(-1.23) // "-1.23"
    TO_STRING(0.0000002) // "2e-7"
    TO_STRING( [1, 2, 3] ) // "[1,2,3]"
    TO_STRING( { foo: "bar", baz: null } ) // "{\"foo\":\"bar\",\"baz\":null}"
    
## TO_ARRAY()
    TO_ARRAY(value) → array
    
    接受任何类型的输入值并将其转换为数组值。
    
    值（任意）：任意类型的输入
    返回数组（array）：
    null转换为空数组
    布尔值，数字和字符串将转换为包含原始值作为其单个元素的数组
    数组保留其原始值
    对象/文档将转换为包含其属性 值作为数组元素的数组，就像VALUES（）一样
    TO_ARRAY(null) // []
    TO_ARRAY(false) // [false]
    TO_ARRAY(true) // [true]
    TO_ARRAY(5) // [5]
    TO_ARRAY("foo") // ["foo"]
    TO_ARRAY([1, 2, "foo"]) // [1, 2, "foo"]
    TO_ARRAY({foo: 1, bar: 2, baz: [3, 4, 5]}) // [1, 2, [3, 4, 5]]  
   
## TO_LIST()
    TO_LIST(value) → array
    这是TO_ARRAY（）的别名。 
    
# 类型检查功能
    AQL还提供了在运行时检查值的数据类型的功能。以下类型检查功能可用。这些函数中的每一个都接受任何数据类型的参数，如果值具有要检查的类型，则返回true，否则返回false。
    
## IS_NULL（）
    IS_NULL(value) → bool
    
    检查value是否为null。与相同value == null。
    
    要测试属性是否存在，请参阅HAS（）。
    
    值（任意）：要测试的值
    返回布尔值（布尔值）：如果value是true，则返回true， 否则返回falsenull
## IS_BOOL（）
    IS_BOOL(value) → bool
    
    检查值是否为布尔值
    
    值（任意）：要测试的值
    返回布尔值（布尔值）：如果value为or ，则为true， 否则为falsetruefalse
## IS_NUMBER（）
    IS_NUMBER(value) → bool
    
    检查值是否为数字
    
    值（任意）：要测试的值
    返回bool类型（布尔）：真，如果值是一个数字， 假否则
## IS_STRING（）
    IS_STRING(value) → bool
    
    检查值是否为字符串
    
    值（任意）：要测试的值
    返回布尔值（布尔值）：如果value是字符串，则返回true， 否则返回false
## IS_ARRAY（）
    IS_ARRAY(value) → bool
    
    检查值是否为数组/列表
    
    值（任意）：要测试的值
    返回bool的（布尔值）：真，如果值是数组/列表中， 假否则
## IS_LIST（）
    IS_LIST(value) → bool
    
    这是IS_ARRAY（）的别名
    
## IS_OBJECT（）
    IS_OBJECT(value) → bool
    
    检查值是否为对象/文档
    
    值（任意）：要测试的值
    返回bool的（布尔值）：真，如果值是一个对象/文件， 假否则
## IS_DOCUMENT（）
    IS_DOCUMENT(value) → bool
    
    这是IS_OBJECT（）的别名
    
## IS_DATESTRING（）
    IS_DATESTRING(str) → bool
    
    检查value是否是可以在日期函数中使用的字符串。这包括部分日期，例如“ 2015”或“ 2015-10”，以及包含格式正确但无效的日期的字符串，例如“ 2015-02-31”。
    
    str（字符串）：要测试的日期字符串
    返回bool的（布尔值）：真，如果STR是正确格式的日期字符串， 假否则包括所有非字符串值，即使它们中的一些可以是日期函数可用（数字时间戳）
## IS_KEY（）
    IS_KEY(str) → bool
    
    检查value是否是可以用作文档密钥的字符串，即_key属性的值。请参阅文档密钥的命名约定。
    
    str（字符串）：要测试的文档密钥
    返回布尔值（布尔值）：str是否可以用作文档键
    
## TYPENAME（）
    TYPENAME(value) → typeName
    
    返回值的数据类型名称。
    
    值（任意）：任意类型的输入
    返回的typeName（字符串）的数据类型名称值 （"null"，"bool"，"number"，"string"，"array"或"object"）
    范例值	数据类型名称
    null	"null"
    true	"bool"
    false	"bool"
    123	"number"
    -4.56	"number"
    0	"number"
    "foobar"	"string"
    "123"	"string"
    ""	"string"
    [ 1, 2, 3 ]	"array"
    ["foo",true]	"array"
    [ ]	"array"
    {"foo":"bar"}	"object"
    {"foo": null}	"object"
    { }	"object"
    
     
    