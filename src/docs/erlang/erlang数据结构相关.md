# erlang各种数据类型占用的内存大小
    有效编程的一个好的开始是知道不同数据类型和操作需要多少内存。
    Erlang数据类型和其他项目消耗多少内存与实现方式有关，但是下表显示了OTP 19.0中erts-8.0系统的一些数据。
    度量单位是存储字。 同时存在32位和64位实现。 因此，一个字分别是4字节或8字节。
    erlang:system_info(wordsize).
    
```
Data Type       	Memory Size

Small integer
                    1 word.
                    On 32-bit architectures: -134217729 < i < 134217728
                    (28 bits).
                    On 64-bit architectures: -576460752303423489 < i <
                    576460752303423488 (60 bits).

Large 
                    integer 3..N words.

Atom	
                    1 word.
                    An atom refers into an atom table, which also consumes
                    memory. The atom text is stored once for each unique
                    atom in this table. The atom table is not garbage-collected.

Float	
                    On 32-bit architectures: 4 words.
                    On 64-bit architectures: 3 words.

Binary	
                    3..6 words + data (can be shared).

List	
                    1 word + 1 word per element + the size of each element.

String (is the same as a list of integers)
                    1 word + 2 words per character.

Tuple
                	2 words + the size of each element.

Small Map
                	5 words + the size of all keys and values.

Large Map (> 32 keys)
                	N x F words + the size of all keys and values.
                    N is the number of keys in the Map.
                    F is a sparsity factor that can vary 
                    between 1.6 and 1.8 due to the probabilistic nature of
                    the internal HAMT data structure.

Pid	
                    1 word for a process identifier from the current local
                    node + 5 words for a process identifier from another
                    node.
                    A process identifier refers into a process table and a
                    node table, which also consumes memory.                    

Port	            
                    1 word for a port identifier from the current local node +
                    5 words for a port identifier from another node.
                    A port identifier refers into a port table and a node table,
                    which also consumes memory.

Reference
                    On 32-bit architectures: 5 words for a reference from
                    the current local node + 7 words for a reference from
                    another node.
                    On 64-bit architectures: 4 words for a reference from
                    the current local node + 6 words for a reference from
                    another node.
                    A reference refers into a node table, which also
                    consumes memory.
                   
Fun	
                    9..13 words + the size of environment.
                    A fun refers into a fun table, which also consumes
                    memory.

Ets table	        
                    Initially 768 words + the size of each element (6 words
                    + the size of Erlang data). The table grows when
                    necessary.  
  
Erlang process	
                    338 words when spawned, including a heap of 233 words.
```

# System Limits
    Erlang语言规范对进程数，原子长度等没有任何限制。 但是，出于性能和内存节省的原因，在Erlang语言和执行环境的实际实现中始终会受到限制。
```
Processes 
            The maximum number of simultaneously alive Erlang
            processes is by default 262,144. This limit can be
            configured at startup. For more information, see the
            +P command-line flag in the erl(1) manual page in
            ERTS.  

Known nodes
           A remote node Y must be known to node X if there
           exists any pids, ports, references, or funs (Erlang data
           types) from Y on X, or if X and Y are connected. The
           maximum number of remote nodes simultaneously/ever
           known to a node is limited by the maximum number of
           atoms available for node names. All data concerning
           remote nodes, except for the node name atom, are
           garbage-collected.  

Connected nodes
           The maximum number of simultaneously connected
           nodes is limited by either the maximum number of
           simultaneously known remote nodes, the maximum  
           number of (Erlang) ports available, or the maximum
           number of sockets available. 

Characters in an atom
          255.

Atoms
          By default, the maximum number of atoms is 1,048,576.
          This limit can be raised or lowered using the +t option.

Elements in a tuple
          The maximum number of elements in a tuple is
          16,777,215 (24-bit unsigned integer).

Size of binary  
          In the 32-bit implementation of Erlang, 536,870,911
          bytes is the largest binary that can be constructed
          or matched using the bit syntax. In the 64-
          bit implementation, the maximum size is
          2,305,843,009,213,693,951 bytes. If the limit
          is exceeded, bit syntax construction fails with a
          system_limit exception, while any attempt to
          match a binary that is too large fails. This limit is
          enforced starting in R11B-4.
          In earlier Erlang/OTP releases, operations on too large
          binaries in general either fail or give incorrect results.
          In future releases, other operations that create binaries
          (such as list_to_binary/1) will probably also
          enforce the same limit.

Total amount of data allocated by an Erlang node
           The Erlang runtime system can use the complete 32-bit
           (or 64-bit) address space, but the operating system often
           limits a single process to use less than that.

Length of a node name
            An Erlang node name has the form host@shortname
            or host@longname. The node name is used as an atom
            within the system, so the maximum size of 255 holds
            also for the node name.

Open ports
           The maximum number of simultaneously open Erlang
           ports is often by default 16,384. This limit can be
           configured at startup. For more information, see the
           +Q command-line flag in the erl(1) manual page in
           ERTS. 

Open files and sockets
          同时打开的文件和套接字的最大数量取决于可用的Erlang端口的最大数量，以及特定于操作系统的设置和限制。 

Number of arguments to a function or fun
          255

Unique References on a Runtime System Instance
            Each scheduler thread has its own set of references,
            and all other threads have a shared set of references.
            Each set of references consist of 2## - 1 unique
            references. That is, the total amount of unique
            references that can be produced on a runtime system
            instance is (NoSchedulers + 1) × (2## -
            1).
            If a scheduler thread create a new reference each nano
            second, references will at earliest be reused after more
            than 584 years. That is, for the foreseeable future they
            are unique enough.

Unique Integers on a Runtime System Instance
            There are two types of unique integers both created
            using the erlang:unique_integer() BIF:
            1. Unique integers created with the monotonic
            modifier consist of a set of 2## - 1 unique integers.
            2. Unique integers created without the monotonic
            modifier consist of a set of 2## - 1 unique integers
            per scheduler thread and a set of 2## - 1 unique
            integers shared by other threads. That is, the total
            amount of unique integers without the monotonic
            modifier is (NoSchedulers + 1) × (2## -
            1).
            If a unique integer is created each nano second, unique
            integers will at earliest be reused after more than 584
            years. That is, for the foreseeable future they are unique
            enough.
```  
  
# Erlang 常用数据结构实现
    erlang虚拟机中用Eterm表示所有的类型的数据，具体的实施方案通过占用Eterm的后几位作为类型标签，然后根据标签类型来解释剩余位的用途。这个标签是多层级的，最外层占用两位，有三种类型： 
        list，剩下62位是指向列表Cons的指针
        boxed对象，即复杂对象，剩余62位指向boxed对象的对象头。包括元组，大整数，外部Pid/Port等
        immediate立即数，即可以在一个字中表示的小型对象，包括小整数，本地Pid/Port，Atom，NIL等
        
        这三种类型是Erlang类型的大框架，前两者是可以看做是引用类型，立即数相当于是值类型，但无论对于哪种类型，Erlang Eterm本身只占用一个字，理解这一点是很重要的。
        对于二三级标签的细分和编码，一般我们无需知道这些具体的底层细节，以下是几种常用的数据结构实现方式。
    一. 常用类型
    1. atom  
        atom用立即数表示，在Eterm中保存的是atom在全局atom表中的索引，依赖于高效的哈希和索引表，Erlang的atom比较和匹配像整数一样高效。atom表是不回收的，并且默认最大值为1024*1024，超过这个限制Erlang虚拟机将会崩溃，可通过+t参数调整该上限。      
    2.Pid/Port
        在R9B之后，随着进程数量增加和其它因素，Pid只在32位中表示本地Pid(A=0)，将32位中除了4位Tag之外的28位，都可用于进程Pid表示，
        出于Pid表示的历史原因，仍然保留三段式的显示，本地Pid表示变成了<0, Pid低15位, Pid高13位>。对于外部Pid，采用boxed复合对象表示，
        在将本地Pid发往其它node时，Erlang会自动将为Pid加上本地节点信息，并打包为一个boxed对象，占用6个字。另外，Erlang需要维护Pid表，
        每个条目占8个字节，当进程数量过大时，Pid表将占用大量内存，Erlang默认可以使用18位有效位来表示Pid(262144)，可通过+P参数调节，
        最大值为27位(2^27-1)，此时Pid表占用内存为2G。
    3. ists
       列表以标签01标识，剩余62位指向列表的Cons单元，Cons是[Head|Tail]的组合，在内存中体现为两个相邻的Eterm，Head可以是任何类型的Eterm，
       。因此形如L2 = [Elem|L1]的操作，实际上构造了一个新的Cons，其中Head是Elem Eterm，Tail是L1 Eterm，然后将L2的Eterm指向了这个新的Cons，
       因此L2即代表了这个新的列表。对于[Elem|L2] = L1，实际上是提出了L1 Eterm指向的Cons，将Head部分赋给Elem，Tail部分赋给L2，
       注意Tail本身就是个List的Eterm，因此list是单向列表，并且构造和提取操作是很高效的。需要再次注意的是，Erlang所有类型的Eterm本身只占用一个字大小。
       这也是诸如list,tuple能够容纳任意类型的基础。
       
       Erlang中进程内对对象的重复引用只需占用一份对象内存(只是Eterm本身一个字的拷贝)，但是在对象跨进程时，对象会被展开，执行速深度拷贝：  
         
    4. tuple
       tuple属于boxed对象的一种，每个boxed对象都有一个对象头(header)，boxed Eterm即指向这个header，这个header里面包含具体的boxed对象类型，
       如tuple的header末6位为000000，前面的位数为tuple的size： 
       tuple实际上就是一个有头部的数组，其包含的Eterm在内存中紧凑排列，tuple的操作效率和数组是一致的。
       list，tuple中添加元素，实际上都是在拷贝Eterm本身，Erlang虚拟机会追踪这些引用，并负责垃圾回收。
    5. binary
       Erlang binary用于处理字节块，Erlang其它的数据结构(list,tuple,record)都是以Eterm为单位的，用于处理字节块会浪费大量内存
       ，如”abc”占用了7个字(加上ETerm本身)，binary为字节流提供一种操作高效，占用空间少的解决方案。
       
       之前我们介绍的数据结构都存放在Erlang进程堆上，进程内部可以使用对象引用，在对象跨进程传输时，会执行对象拷贝。
       为了避免大binary跨进程传输时的拷贝开销，Erlang针对binary作出了优化，将binary分为小binary和大binary。
       heap binary   
            小于64字节(定义于erl_binary.h ERL_ONHEAP_BIN_LIMIT宏)的小binary直接创建在进程堆上，称为heap binary，heap binary是一个boxed对象：  
       refc binary
            大于64字节的binary将创建在Erlang虚拟机全局堆上，称为refc binary(reference-counted binary)，可被所有Erlang进程共享，
            这样跨进程传输只需传输引用即可，虚拟机会对binary本身进行引用计数追踪，以便GC。refc binary需要两个部分来描述，
            位于全局堆的refc binary数据本身和位于进程堆的binary引用(称作proc binary)，这两种数据结构定义于global.h中。
            下图描述refc binary和proc binary的关系：
            所有的OffHeap(进程堆之外的数据)被组织为一个单向链表，进程控制块(erl_process.h struct process)中的off_heap字段维护链表头和所有OffHeap对象的总大小，
            当这个大小超过虚拟机阀值时，将导致一次强制GC。注意，refc binary只是OffHeap对象的一种，以后可扩展其它种类。
       sub binary
            sub binary是Erlang为了优化binary分割的(如split_binary/2)，由于Erlang变量不可变语义，拷贝分割的binary是效率比较底下的做法，Erlang通过sub binary来复用原有binary。
       bit string
            当我们通过如<<2:3,3:6>>的位语法构建binary时，将得到<<65,1:1>>这种非字节对齐的数据，即二进制流，
            在Erlang中被称为bitstring，Erlang的bitstring基于ErlSubBin结构实现，此时bitsize为最后一个字节的有效位数，
            size为有效字节数(不包括未填满的最后一个字节)，对虚拟机底层来说，sub bianry和bit string是同一种数据结构。
            
    ## 复合类型
     1. record
       这个类型无需过多介绍，它就是一个tuple，所谓record filed在预编译后实际上都是通过数值下标来索引，因此它访问field是O(1)复杂度的。        
     2. map
        该结构体之后就是依次存放的Value，因此maps的get操作，需要先遍历keys tuple，找到key所在下标，然后在value中取出该下标偏移对应的值。因此是O(n)复杂度的。详见maps:get源码($BEAM_SRC/erl_map.c erts_maps_get)。
        
        如此的maps，只能作为record的替用，并不是真正的Key->Value映射，因此不能存放大量数据。而在OTP18中，maps加入了针对于big map的hash机制，
        当maps:size < MAP_SMALL_MAP_LIMIT时，使用flatmap结构，也就是上述OTP17中的结构，当maps:size >= MAP_SMALL_MAP_LIMIT时，
        将自动使用hashmap结构来高效存取数据。MAP_SMALL_MAP_LIMIT在erl_map.h中默认定义为32。
        
        仍然要注意Erlang本身的变量不可变原则，每次执行更新maps，都会导致新开辟一个maps，并且拷贝原maps的keys和values，在这一点上，maps:update比maps:put更高效，因为前者keys数量不会变，因此无需开辟新的keys tuple，拷贝keys tuples ETerm即可。实际使用maps时：
        
        更新已有key值时，使用update(:=)而不是put(=>)，不仅可以检错，并且效率更高
        当key/value对太多时，对其进行层级划分，保证其拷贝效率
        实际测试中，OTP18中的maps在存取大量数据时，效率还是比较高的，这里有一份maps和dict的简单测试函数，可通过OTP17和OTP18分别运行来查看效率区别。通常情况下，我们应当优先使用maps，比起dict，它在模式匹配，mongodb支持，可读性上都有很大优势。    
     
     3. array
        array下标从0开始
        array有两种模式，一种固定大小，另一种按需自动增长大小，但不会自动收缩
        支持稀疏存储，执行array:set(100,value,array:new())，那么[0,99]都会被设置为默认值(undefined)，该默认值可修改。
        在实现上，array最外层被包装为一个record:
     ... 其他等待被添加   
     
## 顺序
number < atom < reference < fun < port < pid < tuple < map < nil < list < bit string 
       %% Module	Description
       %% sets	   sets, a collection of unique elements.
       %% gb_sets	sets, but based on a general balanced data structure
       %% gb_tree	a general balanced tree
       %% dict	   maps, also called associative arrays
       %% queue	   double-ended queues
       %% ets	   hash tables and ordered sets (trees), stored outside the process
       %% dets	   on-disk hash tables
       (请注意：不常用的模块ordset和 orddict只是有序列表，因此对于诸如插入之类的常见操作具有O（n）)

# Erlang标准数据结构的选择
  实际上，Erlang程序使用列表（本机或通过dict）来处理涉及多达数百个元素的数据结构，
  并使用ETS（Erlang术语存储）或mnesia来处理更大的数据。
  ETS使用散列来允许几乎恒定时间访问几乎任意数量的数据。
  对于由几个（几十个或几百个）项组成的数据集合，列表通常要优于ETS和树。 对于大量小物品，ETS往往效果最好。
   对于较大的项目，数据插入ets和从ets读取都会复制数据, 需要掂量。

  lists ，maps 和record是erlang最为常用的数据结构，lists使用方便简单，maps则查询高效，record则需要预定义，
  对比测试数据maps在查询性能上比lists高， 而在遍历上lists则更优。对于频繁插入和查询的数据，maps是最佳的选择，
  record在数据量小的情况下 插入 更新 查询效率都很高， 而且使用的是模式匹配也很方便
  lists则适用于广播列表之类需要遍历的数据和数据量少的情况。
  更多数据结构
  utPdDs, utArrayDs, utTupleDs, utListsDs, utMapsDs, utEtsSetDs, utEtsOrdDs, utDictDs, utGb_treesDs, utSetsDs, utGb_setsDs, utOrddictDs, utOrdsetsDs, utAtomicsDs, utPTermDs
  测试代码见 testCase/DsTest
  数据结构测评结果见 dosc/erlang-DsBenchMark.txt