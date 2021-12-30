#8 抽象格式
本节将 Erlang 程序的解析树的标准表示描述为 Erlang 术语。这种表示被称为抽象格式。处理这种解析树的函数是 compile:forms/1,2和以下模块中的函数：
    EP(3)
    erl_eval(3)
    erl_lint(3)
    erl_parse(3)
    erl_pp(3)
    io(3)

这些函数还用作解析转换的输入和输出，请参阅compile(3) 模块。

我们使用函数Rep来表示从 Erlang 源结构C到其抽象格式R的映射，并写成 R = Rep(C)。

本节中的单词ANNO表示注释，并表示源文件中发生构造的行号。有关详细信息，请参阅erl_anno(3)。同一结构中的多个ANNO实例可以表示不同的注释。

由于运算符本身并不是术语，因此当下面提到运算符时，运算符的表示将被视为具有与运算符相同的字符组成的打印名称的原子。

#8.1 模块声明和Forms
模块声明由一系列形式组成，这些形式要么是函数声明，要么是属性。

    如果 D 是由形式F_1 , ..., F_k组成的模块声明 ，则 Rep(D) = [Rep(F_1), ..., Rep(F_k)]。
    如果 F 是属性 -export([Fun_1/A_1, ..., Fun_k/A_k])，则 Rep(F) = {attribute,ANNO,export,[{Fun_1,A_1}, ..., {Fun_k, A_k}]}。
    如果 F 是属性 -import(Mod,[Fun_1/A_1, ..., Fun_k/A_k])，则 Rep(F) = {attribute,ANNO,import,{Mod,[{Fun_1,A_1}, .. ., {Fun_k,A_k}]}} .
    如果 F 是属性 -module(Mod)，则 Rep(F) = {attribute,ANNO,module,Mod}。
    如果 F 是属性-file(File,Line)，则 Rep(F) = {attribute,ANNO,file,{File,Line}}。
    如果 F 是函数声明Name Fc_1 ; ...; 名称 Fc_k，其中每个Fc_i是一个具有相同长度Arity模式序列的函数子句，然后 Rep(F) = {function,ANNO,Name,Arity,[Rep(Fc_1), ...,Rep(Fc_k)] } .
    如果 F 是函数规范-Spec Name Ft_1; ...; Ft_k，其中Spec是原子规范或原子回调，每个Ft_i是一个可能受约束的函数类型，具有相同长度的参数序列 Arity，然后 Rep(F) = {attribute,ANNO,Spec,{{Name, Arity},[Rep(Ft_1), ..., Rep(Ft_k)]}} .
    如果 F 是函数规范 -spec Mod:Name Ft_1; ...; Ft_k，其中每个Ft_i是一个可能受约束的函数类型，具有相同长度的参数序列Arity，然后 Rep(F) = {attribute,ANNO,spec,{{Mod,Name,Arity},[Rep(Ft_1), ... .., Rep(Ft_k)]}}。
    如果 F 是记录声明-record(Name,{V_1, ..., V_k})，其中每个V_i是一个记录字段，则 Rep(F) = {attribute,ANNO,record,{Name,[Rep(V_1 ), ..., Rep(V_k)]}}。对于Rep(V)，见下文。
    如果 F 是类型声明-Type Name(V_1, ..., V_k) :: T，其中Type是原子类型或原子 opaque，每个V_i是一个类型变量，而T是一个类型，那么 Rep( F) = {attribute,ANNO,Type,{Name,Rep(T),[Rep(V_1), ..., Rep(V_k)]}}。
    如果 F 是野生属性-A(T)，则 Rep(F) = {attribute,ANNO,A,T}。

## Rep(V) Record Fields
    记录声明中的每个字段都可以有一个可选的、显式的、默认的初始化表达式和一个可选的类型。
    如果 V 是A，则 Rep(V) = {record_field,ANNO,Rep(A)}。
    如果 V 是A = E，其中E是一个表达式，则 Rep(V) = {record_field,ANNO,Rep(A),Rep(E)}。
    如果 V 是A :: T，其中T是类型，则 Rep(V) = {typed_record_field,{record_field,ANNO,Rep(A)},Rep(T)}。
    如果 V 是A = E :: T，其中 E是表达式，T是类型，则 Rep(V) = {typed_record_field,{record_field,ANNO,Rep(A),Rep(E)},Rep(T) } .

## 解析错误和文件结尾的表示
    除了表单的表示外，表示模块声明的列表（由epp(3)和 erl_parse(3) 中的函数返回 ）可以包含以下内容：
    元组{error,E}和{warning,W}，表示语法错误的形式和警告。
    {eof,LOCATION}，表示在解析完整表单之前遇到的流结束。LOCATION一词代表一个位置，表示源文件中最后一行的编号，也可能是该行最后一列的编号。有关详细信息，请参阅erl_anno(3)。
    有关这些值的更多详细信息，请参阅erl_parse(3) 中的 form_info/0类型 。

# 8.2 原子字面量
    有五种原子文字，它们在模式、表达式和守卫中以相同的方式表示：
    如果 L 是原子文字，则 Rep(L) = {atom,ANNO,L}。
    如果 L 是字符文字，则 Rep(L) = {char,ANNO,L}。
    如果 L 是浮点文字，则 Rep(L) = {float,ANNO,L}。
    如果 L 是整数文字，则 Rep(L) = {integer,ANNO,L}。
    如果 L 是由字符C_1 , ..., C_k组成的字符串文字 ，则 Rep(L) = {string,ANNO,[C_1, ..., C_k]}。

    请注意，负整数和浮点文字不会这样出现；they are parsed as an application of the unary negation operator.

# 8.3 模式
    如果 Ps 是模式序列P_1, ..., P_k，则 Rep(Ps) = [Rep(P_1), ..., Rep(P_k)]。这样的序列作为函数或乐趣的参数列表出现。
    单个模式表示如下：
        如果 P 是原子文字L，则 Rep(P) = Rep(L)。
        如果 P 是位串模式 <<P_1:Size_1/TSL_1, ..., P_k:Size_k/TSL_k>>，其中每个 Size_i是一个可以计算为整数的表达式，每个TSL_i是一个类型指定列表，然后Rep(P) = {bin,ANNO,[{bin_element,ANNO,Rep(P_1),Rep(Size_1),Rep(TSL_1)}, ..., {bin_element,ANNO,Rep(P_k),Rep(Size_k) ,Rep(TSL_k)}]}。对于 Rep(TSL)，请参见下文。默认情况下表示省略的Size_i。默认情况下表示省略的TSL_i。
        If P is a compound pattern P_1 = P_2，则 Rep(P) = {match,ANNO,Rep(P_1),Rep(P_2)}。
        If P is a cons pattern [P_h | P_t]，然后 Rep(P) = {cons,ANNO,Rep(P_h),Rep(P_t)}。
        If P is a map pattern #{A_1, ..., A_k}，其中每个 A_i是一个关联P_i_1 := P_i_2，那么 Rep(P) = {map,ANNO,[Rep(A_1), ..., Rep (A_k)]}。对于Rep(A)，见下文。
        If P is a nil pattern [], 那么 Rep(P) = {nil,ANNO}。
        如果 P 是运算符模式P_1 Op P_2，其中Op是二元运算符（这是应用于文字字符串或字符列表的++的出现，或者可以在编译时计算为数字的表达式的出现)，然后 Rep(P) = {op,ANNO,Op,Rep(P_1),Rep(P_2)}。
        如果 P 是运算符模式Op P_0，其中Op是一元运算符（这是一个可以在编译时计算为数字的表达式的出现），则 Rep(P) = {op,ANNO,Op,Rep( P_0)}。
        If P is a parenthesized pattern ( P_0 ),则 Rep(P) = Rep(P_0)，即括号模式无法与其主体区分开。
        如果 P 是记录字段索引模式#Name.Field，其中Field是原子，则 Rep(P) = {record_index,ANNO,Name,Rep(Field)}。
        If P is a record pattern#Name{Field_1=P_1, ..., Field_k=P_k}，其中每个Field_i是一个原子或_，则 Rep(P) = {record,ANNO,Name,[{record_field,ANNO, Rep(Field_1),Rep(P_1)}, ..., {record_field,ANNO,Rep(Field_k),Rep(P_k)}]} .
        If P is a tuple pattern {P_1, ..., P_k}，则 Rep(P) = {tuple,ANNO,[Rep(P_1), ..., Rep(P_k)]}。
        If P is a universal pattern _，则 Rep(P) = {var,ANNO,'_'}。
        If P is a variable pattern V则 Rep(P) = {var,ANNO,A}，其中 A 是一个原子，其打印名称由与V相同的字符组成。

    请注意，每个模式都与某个表达式具有相同的源形式，并且以与相应表达式相同的方式表示。

# 8.4 Expressions
    主体 B 是表达式E_1, ..., E_k和 Rep(B) = [Rep(E_1), ..., Rep(E_k)]的非空序列。
    表达式 E 是以下之一：
        如果 E 是原子文字L，则 Rep(E) = Rep(L)。
        如果 E 是位串推导式 <<E_0 || Q_1, ..., Q_k>>，其中每个Q_i是一个限定符，那么 Rep(E) = {bc,ANNO,Rep(E_0),[Rep(Q_1), ..., Rep(Q_k)]}。对于Rep(Q)，见下文。
        如果 E 是位串构造函数 <<E_1:Size_1/TSL_1, ..., E_k:Size_k/TSL_k>>，其中每个Size_i是一个表达式，每个 TSL_i是一个类型指定列表，则 Rep(E) = {bin, ANNO,[{bin_element,ANNO,Rep(E_1),Rep(Size_1),Rep(TSL_1)}, ..., {bin_element,ANNO,Rep(E_k),Rep(Size_k),Rep(TSL_k)}]} . 对于 Rep(TSL)，请参见下文。默认情况下表示省略的Size_i。默认情况下表示省略的TSL_i。
        如果 E 是块表达式begin B end，其中B是主体，则 Rep(E) = {block,ANNO,Rep(B)}。
        如果 E 是Cc_1的 case 表达式case E_0 ；...; Cc_k end，其中E_0是一个表达式，每个Cc_i是一个 case 子句，那么 Rep(E) = {'case',ANNO,Rep(E_0),[Rep(Cc_1), ..., Rep(Cc_k)]} .
        如果 E 是 catch 表达式catch E_0，则 Rep(E) = {'catch',ANNO,Rep(E_0)}。
        If E is a cons skeleton[E_h | E_t]，然后 Rep(E) = {cons,ANNO,Rep(E_h),Rep(E_t)}。
        If E is a fun expression fun Name/Arity，则 Rep(E) = {'fun',ANNO,{function,Name,Arity}}。
        If E is a fun expression fun Module:Name/Arity，则 Rep(E) = {'fun',ANNO,{function,Rep(Module),Rep(Name),Rep(Arity)}}。
        If E is a fun expression fun Fc_1 ；...; Fc_k end，其中每个Fc_i是一个函数子句，然后 Rep(E) = {'fun',ANNO,{clauses,[Rep(Fc_1), ..., Rep(Fc_k)]}}。
        If E is a fun expression fun Name Fc_1 ; ...; Name Fc_k end，其中Name是一个变量，每个 Fc_i是一个函数子句，然后 Rep(E) = {named_fun,ANNO,Name,[Rep(Fc_1), ..., Rep(Fc_k)]}。
        If E is a function call E_0(E_1, ..., E_k)，则 Rep(E) = {call,ANNO,Rep(E_0),[Rep(E_1), ..., Rep(E_k)]}。
        If E is a function callE_m:E_0(E_1, ..., E_k)，则 Rep(E) = {call,ANNO,{remote,ANNO,Rep(E_m),Rep(E_0)},[Rep(E_1) ), ..., Rep(E_k)]}。
        If E is an if expression if Ic_1 ; ...; Ic_k end，其中每个Ic_i是一个 if 子句，然后 Rep(E) = {'if',ANNO,[Rep(Ic_1), ..., Rep(Ic_k)]}。
        If E is a list comprehension  [E_0 || Q_1, ..., Q_k]，其中每个Q_i是一个限定符，那么 Rep(E) = {lc,ANNO,Rep(E_0),[Rep(Q_1), ..., Rep(Q_k)]}。对于Rep(Q)，见下文。
        If E is a map creation #{A_1, ..., A_k}，其中每个A_i是一个关联E_i_1 => E_i_2，那么 Rep(E) = {map,ANNO,[Rep(A_1), ..., Rep (A_k)]}。对于Rep(A)，见下文。
        If E is a map updateE_0#{A_1, ..., A_k}，其中每个A_i是关联E_i_1 => E_i_2 或E_i_1 := E_i_2，则 Rep(E) = {map,ANNO,Rep(E_0), [Rep(A_1), ..., Rep(A_k)]} . 对于Rep(A)，见下文。
        If E is a match operator expression P = E_0，其中P是模式，则 Rep(E) = {match,ANNO,Rep(P),Rep(E_0)}。
        If E is nil, [] 则 Rep(E) = {nil,ANNO}。
        If E is an operator expression E_1 Op E_2，其中Op是匹配运算符=以外的二元运算符 ，则 Rep(E) = {op,ANNO,Op,Rep(E_1),Rep(E_2)}。
        If E is an operator expression Op E_0，其中Op是一元运算符，则 Rep(E) = {op,ANNO,Op,Rep(E_0)}。
        If E is a parenthesized expression ( E_0 )，则 Rep(E) = Rep(E_0)，即括号表达式无法与其主体区分开来。
        如果 E 是接收表达式，则receive Cc_1 ；...; Cc_k end，其中每个Cc_i是一个 case 子句，然后 Rep(E) = {'receive',ANNO,[Rep(Cc_1), ..., Rep(Cc_k)]}。
        If E is a receive expression receive Cc_1 ；...; Cc_k after E_0 -> B_t end，其中每个Cc_i是一个 case 子句，E_0是一个表达式，而B_t是一个主体，那么 Rep(E) = {'receive',ANNO,[Rep(Cc_1), ..., Rep(Cc_k)],Rep(E_0),Rep(B_t)}。
        If E is a record creation #Name{Field_1=E_1, ..., Field_k=E_k}，其中每个Field_i是一个原子或_，则 Rep(E) = {record,ANNO,Name,[{record_field,ANNO, Rep(Field_1),Rep(E_1)}, ..., {record_field,ANNO,Rep(Field_k),Rep(E_k)}]}。
        If E is a record field accessE_0#Name.Field，其中Field是原子，则 Rep(E) = {record_field,ANNO,Rep(E_0),Name,Rep(Field)}。
        If E is a record field index #Name.Field，其中Field是原子，则 Rep(E) = {record_index,ANNO,Name,Rep(Field)}。
        If E is a record update E_0#Name{Field_1=E_1, ..., Field_k=E_k}，其中每个Field_i是一个原子，那么 Rep(E) = {record,ANNO,Rep(E_0),Name,[{ record_field,ANNO,Rep(Field_1),Rep(E_1)}, ..., {record_field,ANNO,Rep(Field_k),Rep(E_k)}]} .
        If E is a tuple skeleton {E_1, ..., E_k}，则 Rep(E) = {tuple,ANNO,[Rep(E_1), ..., Rep(E_k)]}。
        If E is a try expression try B catch Tc_1 ; ...; Tc_k end，其中B是一个主体，每个Tc_i是一个 catch 子句，然后 Rep(E) = {'try',ANNO,Rep(B),[],[Rep(Tc_1), ..., Rep(Tc_k )],[]}。
        If E is a try expression  try B of Cc_1 ; ...; cc_k 捕捉 Tc_1 ; ...; Tc_n end，其中B是一个主体，每个Cc_i是一个 case 子句，每个Tc_j是一个 catch 子句，那么 Rep(E) = {'try',ANNO,Rep(B),[Rep(Cc_1), .. ., Rep(Cc_k)],[Rep(Tc_1), ..., Rep(Tc_n)],[]} .
        If E is a try expression try B after A end，其中B和A是主体，则 Rep(E) = {'try',ANNO,Rep(B),[],[],Rep(A)}。
        If E is a try expression try B of Cc_1 ; ...; Cc_k 在 A end 之后，其中B和A是一个主体，每个Cc_i是一个 case 子句，那么 Rep(E) = {'try',ANNO,Rep(B),[Rep(Cc_1), ..., Rep (Cc_k)],[],Rep(A)}。
        If E is a try expression  try B catch Tc_1 ; ...; A end 之后的 Tc_k，其中B和A是主体，每个Tc_i是一个 catch 子句，那么 Rep(E) = {'try',ANNO,Rep(B),[],[Rep(Tc_1), ... , Rep(Tc_k)],Rep(A)} .
        If E is a try expression  try B of Cc_1 ; ...; cc_k 捕捉 Tc_1 ; ...; A end 之后的 Tc_n，其中B和A是一个主体，每个Cc_i是一个 case 子句，每个Tc_j是一个 catch 子句，那么 Rep(E) = {'try',ANNO,Rep(B),[Rep(Cc_1 ), ..., Rep(Cc_k)],[Rep(Tc_1), ..., Rep(Tc_n)],Rep(A)} .
        如果 E 是变量V，则 Rep(E) = {var,ANNO, A}，其中A是一个原子，其打印名称由与V相同的字符组成。

## Qualifiers
    限定符 Q 是以下之一：
    如果 Q 是过滤器E，其中E是表达式，则 Rep(Q) = Rep(E)。
    如果 Q 是生成器P <- E，其中P是模式，E是表达式，则 Rep(Q) = {generate,ANNO,Rep(P),Rep(E)}。
    如果 Q 是位串生成器P <= E，其中P是模式，E是表达式，则 Rep(Q) = {b_generate,ANNO,Rep(P),Rep(E)}。

## Bitstring Element Type Specifiers
    位串元素的类型说明符列表 TSL 是类型说明符TS_1 - ... - TS_k和 Rep(TSL) = [Rep(TS_1), ..., Rep(TS_k)] 的序列。
    如果 TS 是类型说明符A，其中A是原子，则 Rep(TS) = A。
    如果 TS 是类型说明符A:Value，其中A是原子，Value是整数，则 Rep(TS) = {A,Value}。
## Map Associations
    关联 A 是以下之一：
    如果 A 是关联K => V，则 Rep(A) = {map_field_assoc,ANNO,Rep(K),Rep(V)}。
    如果 A 是关联K := V，则 Rep(A) = {map_field_exact,ANNO,Rep(K),Rep(V)}。

# 8.5  Clauses
    There are function clauses, if clauses, case clauses, and catch clauses.
    A clause C is one of the following:
        If C is a case clause P -> B, 其中P是模式，B是主体，则 Rep(C) = {clause,ANNO,[Rep(P)],[],Rep(B)}。
        If C is a case clause P when Gs -> B, 其中P是模式， Gs是保护序列，B是主体，则 Rep(C) = {clause,ANNO,[Rep(P)],Rep (Gs),Rep(B)}。
        If C is a catch clause P -> B, 其中P是模式，B是主体，则 Rep(C) = {clause,ANNO,[Rep({throw,P,_})],[],Rep (B)}，即带有显式异常类 throw且带有或不带有显式堆栈跟踪变量_的 catch 子句无法与没有显式异常类和显式堆栈跟踪变量的 catch 子句区分开来。
        If C is a catch clause X : P -> B, 其中X是原子文字或变量模式， P是模式，B是主体，则 Rep(C) = {clause,ANNO,[Rep({ X,P,_})],[],Rep(B)}，即带有显式异常类和显式堆栈跟踪变量_的 catch 子句不能与带有显式异常类且没有显式异常类的 catch 子句区分开来显式堆栈跟踪变量。
        If C is a catch clause X : P : S -> B, 其中X是原子文字或变量模式， P是模式，S是变量，B 是主体，则 Rep(C) = {clause ,ANNO,[Rep({X,P,S})],[],Rep(B)}。
        If C is a catch clause P when Gs -> B, 其中P是模式，Gs是保护序列，B是主体，则 Rep(C) = {clause,ANNO,[Rep({throw,P, _})],Rep(Gs),Rep(B)}，即带有显式异常类 throw和带有或不带有显式堆栈跟踪变量_的 catch 子句不能与没有显式异常类的 catch 子句区分开来，并且没有显式的堆栈跟踪变量。
        If C is a catch clause X : P when Gs -> B, 其中X是原子文字或变量模式， P是模式，Gs是保护序列，B是主体，则 Rep(C) = {子句,ANNO,[Rep({X,P,_})],Rep(Gs),Rep(B)}，即带有显式异常类和显式堆栈跟踪变量_的 catch 子句无法与带有显式异常类且没有显式堆栈跟踪变量的 catch 子句。
        If C is a catch clause X : P : S when Gs -> B, 其中X是原子文字或变量模式， P是模式，Gs是保护序列， S是变量，B是主体，然后 Rep(C) = {clause,ANNO,[Rep({X,P,S})],Rep(Gs),Rep(B)}。
        If C is a function clause ( Ps ) -> B,其中Ps是模式序列而B是主体，则 Rep(C) = {clause,ANNO,Rep(Ps),[],Rep(B)}。
        If C is a function clause ( Ps ) when Gs -> B, 其中Ps是模式序列， Gs是保护序列，B是主体，则 Rep(C) = {clause,ANNO,Rep(Ps),Rep (Gs),Rep(B)}。
        If C is an if clause Gs -> B, 其中Gs是保护序列，B是主体，则 Rep(C) = {clause,ANNO,[],Rep(Gs),Rep(B)}。

# 8.6  Guards
    保护序列Gs是保护序列G_1；...; G_k和 Rep(Gs) = [Rep(G_1), ..., Rep(G_k)]。如果保护序列为空，则 Rep(Gs) = []。
    守卫 G 是守卫测试Gt_1, ..., Gt_k和 Rep(G) = [Rep(Gt_1), ..., Rep(Gt_k)]的非空序列 。
    防护测试 Gt 是以下之一：
        If Gt is an atomic literal L,则 Rep(Gt) = Rep(L)。
        If Gt is a bitstring constructor <<Gt_1:Size_1/TSL_1, ..., Gt_k:Size_k/TSL_k>>, 其中每个Size_i是一个保护测试，每个 TSL_i是一个类型指定列表，那么 Rep(Gt) = {bin ,ANNO,[{bin_element,ANNO,Rep(Gt_1),Rep(Size_1),Rep(TSL_1)}, ..., {bin_element,ANNO,Rep(Gt_k),Rep(Size_k),Rep(TSL_k)}] } . 对于 Rep(TSL)，请参见上文。默认情况下表示省略的Size_i。默认情况下表示省略的TSL_i。
        If Gt is a cons skeleton [Gt_h | Gt_t], 然后 Rep(Gt) = {cons,ANNO,Rep(Gt_h),Rep(Gt_t)}。
        If Gt is a function call A(Gt_1, ..., Gt_k), 其中A是一个原子，那么 Rep(Gt) = {call,ANNO,Rep(A),[Rep(Gt_1), ..., Rep (Gt_k)]}。
        If Gt is a function call A_m:A(Gt_1, ..., Gt_k),其中A_m是原子erlang并且A是原子或运算符，则 Rep(Gt) = {call,ANNO,{remote,ANNO, Rep(A_m),Rep(A)},[Rep(Gt_1), ..., Rep(Gt_k)]} .
        If Gt is a map creation #{A_1, ..., A_k}, 其中每个A_i是一个关联Gt_i_1 => Gt_i_2，那么 Rep(Gt) = {map,ANNO,[Rep(A_1), ..., Rep (A_k)]}。对于Rep(A)，见上文。
        If Gt is a map update Gt_0#{A_1, ..., A_k}, 其中每个A_i是一个关联Gt_i_1 => Gt_i_2 或Gt_i_1 := Gt_i_2，则 Rep(Gt) = {map,ANNO,Rep(Gt_0), [Rep(A_1), ..., Rep(A_k)]} . 对于Rep(A)，见上文。
        If Gt is nil, [], 则 Rep(Gt) = {nil,ANNO}。
        If Gt is an operator guard test Gt_1 Op Gt_2,其中Op是除 match operator =之外的二元运算符，则 Rep(Gt) = {op,ANNO,Op,Rep(Gt_1),Rep(Gt_2)}。
        If Gt is an operator guard test Op Gt_0, 其中Op是一元运算符，则 Rep(Gt) = {op,ANNO,Op,Rep(Gt_0)}。
        If Gt is a parenthesized guard test ( Gt_0 ), 那么 Rep(Gt) = Rep(Gt_0)，即括号内的守卫测试无法与其主体区分开来。
        If Gt is a record creation #Name{Field_1=Gt_1, ..., Field_k=Gt_k},其中每个Field_i是一个原子或_，则 Rep(Gt) = {record,ANNO,Name,[{record_field,ANNO, Rep(Field_1),Rep(Gt_1)}, ..., {record_field,ANNO,Rep(Field_k),Rep(Gt_k)}]}。
        If Gt is a record field access Gt_0#Name.Field,其中Field是原子，则 Rep(Gt) = {record_field,ANNO,Rep(Gt_0),Name,Rep(Field)}。
        If Gt is a record field index #Name.Field, 其中Field是原子，则 Rep(Gt) = {record_index,ANNO,Name,Rep(Field)}。
        If Gt is a tuple skeleton {Gt_1, ..., Gt_k}, 则 Rep(Gt) = {tuple,ANNO,[Rep(Gt_1), ..., Rep(Gt_k)]}。
        If Gt is a variable pattern V, then Rep(Gt) = {var,ANNO,A}, 其中 A 是一个原子，其打印名称由与V相同的字符组成。

    请注意，每个保护测试都与某个表达式具有相同的源形式，并以与相应表达式相同的方式表示。

# 8.7 类型
    If T is an annotated type A :: T_0, 其中A是变量，则 Rep(T) = {ann_type,ANNO,[Rep(A),Rep(T_0)]}。
    If T is an atom, a character, or an integer literal L,则 Rep(T) = Rep(L)。
    If T is a bitstring type <<_:M,_:_*N>>,其中M和N是单例整数类型，则 Rep(T) = {type,ANNO,binary,[Rep(M),Rep( N)]}。
    If T is the empty list type [],则 Rep(T) = {type,ANNO,nil,[]}，即空列表类型 []无法与预定义类型nil()区分开来 。
    If T is a fun type fun(),那么 Rep(T) = {type,ANNO,'fun',[]}。
    If T is a fun type fun((...) -> T_0), 那么 Rep(T) = {type,ANNO,'fun',[{type,ANNO,any},Rep(T_0)]}。
    If T is a fun type fun(Ft),其中 Ft是一个函数类型，那么 Rep(T) = Rep(Ft)。对于Rep(Ft)，见下文。
    If T is an integer range type L .. H,其中L和H是单例整数类型，则 Rep(T) = {type,ANNO,range,[Rep(L),Rep(H)]}。
    If T is a map type map(),则 Rep(T) = {type,ANNO,map,any}。
    If T is a map type #{A_1, ..., A_k},其中每个 A_i是关联类型，则 Rep(T) = {type,ANNO,map,[Rep(A_1), ..., Rep( A_k)]}。对于Rep(A)，见下文。
    If T is an operator type T_1 Op T_2,其中Op是二元运算符（这是可以在编译时计算为整数的表达式的出现），则 Rep(T) = {op,ANNO,Op,Rep (T_1),Rep(T_2)} .
    If T is an operator type Op T_0, 其中Op是一元运算符（这是一个可以在编译时计算为整数的表达式的出现），则 Rep(T) = {op,ANNO,Op,Rep( T_0)}。
    If T is ( T_0 ), 则 Rep(T) = Rep(T_0)，即括号中的类型无法与其主体区分开来。
    If T is a predefined (or built-in) type N(T_1, ..., T_k), 则 Rep(T) = {type,ANNO,N,[Rep(T_1), ..., Rep(T_k) ]}。
    If T is a record type #Name{F_1, ..., F_k}, 其中每个F_i是记录字段类型，则 Rep(T) = {type,ANNO,record,[Rep(Name),Rep(F_1) , ..., Rep(F_k)]}。对于Rep(F)，见下文。
    If T is a remote type M:N(T_1, ..., T_k),则 Rep(T) = {remote_type,ANNO,[Rep(M),Rep(N),[Rep(T_1), ... , Rep(T_k)]]}。
    If T is a tuple type tuple(), 则 Rep(T) = {type,ANNO,tuple,any}。
    If T is a tuple type {T_1, ..., T_k},则 Rep(T) = {type,ANNO,tuple,[Rep(T_1), ..., Rep(T_k)]}。
    If T is a type union T_1 | ... | T_k,然后 Rep(T) = {type,ANNO,union,[Rep(T_1), ..., Rep(T_k)]}。
    If T is a type variable V, then Rep(T) = {var,ANNO,A}, 其中A是一个原子，其打印名称由与V相同的字符组成。类型变量是除下划线 ( _ )之外的任何变量。
    If T is a user-defined type N(T_1, ..., T_k), 则 Rep(T) = {user_type,ANNO,N,[Rep(T_1), ..., Rep(T_k)]}。

## Function Types
    函数类型 Ft 是以下之一：
        If Ft is a constrained function type Ft_1 when Fc, 其中Ft_1是函数类型而 Fc是函数约束，则 Rep(T) = {type,ANNO,bounded_fun,[Rep(Ft_1),Rep(Fc)]}。对于Rep(Fc)，见下文。
        If Ft is a function type (T_1, ..., T_n) -> T_0, 其中每个T_i都是一个类型，那么 Rep(Ft) = {type,ANNO,'fun',[{type,ANNO,product,[ Rep(T_1), ..., Rep(T_n)]},Rep(T_0)]} .

## Function Constraints
    函数约束 Fc 是约束C_1, ..., C_k和 Rep(Fc) = [Rep(C_1), ..., Rep(C_k)]的非空序列 。
        If C is a constraint V :: T,，其中V是类型变量，T是类型，则 Rep(C) = {type,ANNO,constraint,[{atom,ANNO,is_subtype},[Rep(V),代表(T)]]}。

## Association Types
    If A is an association type K => V, 其中K和V是类型，则 Rep(A) = {type,ANNO,map_field_assoc,[Rep(K),Rep(V)]}。
    If A is an association type K := V, 其中K和V是类型，则 Rep(A) = {type,ANNO,map_field_exact,[Rep(K),Rep(V)]}。

## Record Field Types
    If F is a record field type Name :: Type, 其中Type是类型，则 Rep(F) = {type,ANNO,field_type,[Rep(Name),Rep(Type)]}。

# 8.8  The Abstract Format after Preprocessing
    编译选项debug_info可以指定给编译器，以便将抽象代码存储在Beam 文件的abstract_code块中（用于调试）。
    从 Erlang/OTP R9C 开始，abstract_code块包含 {raw_abstract_v1,AbstractCode}，其中AbstractCode是本节中描述的抽象代码。
    在 R9C 之前的 OTP 版本中，经过更多处理的抽象代码存储在 Beam 文件中。元组的第一个元素是abstract_v1（在 OTP R7B 中）或 abstract_v2（在 OTP R8B 中）。