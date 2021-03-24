Erlang 进程相关学习
====================================

# 目录
1.  [actor模型和进程特性](#actor模型和进程特性)
2.  [进程创建](#进程创建)
3.  [进程监控与注册](#进程注册与监控)
4.  [进程调度](#进程调度)
5.  [进程发送消息](#进程发送消息)
6.  [进程接收消息](#进程接收消息)
7.  [进程GC](#进程GC)
8.  [更多](#更多)


  
## actor模型和进程特性
   ### actor模型
   在计算机科学中，它是一个并行计算的数学模型，最初为由大量独立的微处理器组成的高并行计算机所开发，Actor模型的理念非常简单：
   天下万物皆为Actor。Actor之间通过发送消息来通信，消息的传送是异步的，通过一个邮件队列（mail queue）来处理消息。每个Actor
   是完全独立的，可以同时执行它们的操作。每一个Actor是一个计算实体，映射接收到的消息到以下动作：
        1. 发送有限个消息给其它Actor
        2. 创建有限个新的Actor
        3. 为下一个接收的消息指定行为
        
   以上三种动作并没有固定的顺序，可以并发地执行。Actor会根据接收到的消息进行不同的处理。   
   简而言之: 一个Actor指的是一个最基本的计算单元，它能接收一个消息并且基于其执行计算。
   综上，我们知道可以把系统中的所有事物都抽象成一个Actor,那么在一个系统中，可以将一个大规模的任务分解为一些小任务，这些小任务
   可以由多个Actor并发处理，从而减少任务的完成时间和任务复杂度。
   为什么会在讲Erlang进程的时候讲Actor模型的概念，就是因为对于Erlang的并发编程模型正是基于Actor模型，Erlang的代码运行在
   进程中，而进程就是Erlang称呼Actor的方式，Eralng也是最著名的使用Actor规则的编程的语言。

   ### Eralng进程特性
   在Erlang的进程不是我们传统上的进程，Erlang进程是轻量级进程，它的生成、上下文切换和消息传递是由虚拟机管理的,操作系统
   线程进程和Erlang进程之间没有任何联系，这使并发有关的操作不仅独立于底层的操作系统，而且也是非常高效和具有很强可扩展性。
   它运行在 Erlang 虚拟机上，非常小，非常轻，可以瞬间创建上万，甚至几十万个，进程间完全是独立的内存空间执行，不共享内存，
   这些独立的内存空间可以独立的进行垃圾回收，基于独立运行，在发生错误的时候也是隔离的，其他不相关的进程可以继续运行。
   在进程运行时若出现错误，由于进程的轻量级，Erlang 采取的措施是“任其崩溃”和“让其他进程修复”。
   在Erlang上查看默认限制数量是26万多，可以进行修改。每个进程创建后都会有一个独一无二的 Pid，这些进程之间通过 Pid 来互相发
   送消息，进程的唯一交互方式也是消息传递，消息也许能被对方收到，也许不能，收到后可以处理该消息。消息发送是异步的如果想知道某
   个消息是否被进程收到，必须向该进程发送一个消息并等待回复。

## 进程创建
Erlang 中的并发编程只需要如下几个简单的函数。
```erlang
Pid = spawn(Mod，Func， Args)
```
创建一个新的并发进程来执行Mod模块中的 Fun()，Args 是参数。
跟上面提供的spawn/3功能相同的函数还有：
```erlang
spawn(Fun) -> pid()
spawn(Node, Fun) -> pid()
spawn(Module, Function, Args) -> pid()
spawn(Node, Module, Function, Args) -> pid()
spawn_link(Fun) -> pid()
spawn_link(Node, Fun) -> pid()
spawn_link(Module, Function, Args) -> pid()
spawn_link(Node, Module, Function, Args) -> pid()
spawn_monitor(Fun) -> {pid(), reference()}
spawn_monitor(Module, Function, Args) -> {pid(), reference()}
spawn_opt(Fun, Options) -> pid() | {pid(), reference()}
spawn_opt(Node, Fun, Options) -> pid() | {pid(), reference()}
spawn_opt(Module, Function, Args, Options) ->pid() | {pid(), reference()}
spawn_opt(Node, Module, Function, Args, Options) ->pid() | {pid(), reference()}
```
创建好进程，返回对应的Pid之后向就可以向进程进程发送消息，erlang用 “！”来发送消息，格式如下。notice:消息发送是异步的，
发送方不等待而是继续之前的工作。
```erlang
Pid ！Message,
Pid1 ！ Pid2 ！ Pid3 ！ Pid..n ！ Message.
```

erlang用 receve ... end 来接受发送给某个进程的消息，匹配后处理，格式如下。
```erlang
receive
	Pattern1 [when Guard1] ->
		Expression1;
	Pattern2 [when Guard2] ->
		Expression2;
	...
	after T ->
		ExpressionTimeout
end
```
某个消息到达后，会先与 Pattern 进行匹配，匹配相同后执行，若未匹配成功消息则会保存起来待以后处理，进程会开始下一轮操作，
若等待超时T，则会执行表达式 ExpressionTimeout。


## 进程注册与监控
   ### 进程注册
   有些时候使用通过进程Pid来标识进程需要维护进程Pid,出于某些原因维护进程Pid,不方便灵活，比如你给某个服务器进程请求数据，
   你还得考虑怎么得到服务器进程的Pid，有些时候进程由于某种异常重启后Pid会发生变化，如果没有及时同步机制，会导致功能异常，
   于是乎Erlang提供了一套进程注册管理的机制----注册进程Erlang中管理注册进程的有4个内置函数，register、unregister、
   whereis、registered，它们的用法如下:
    1）register(Atom, Pid)：将一个进程Pid注册一个名为AnAtom的原子，如果原子AnAtom已经被另一个注册进程所使用，
        那么注册就会失败。
    2）unregister(Atom)：移除与AnAtom相对应进程的所有注册信息。如果一个注册死亡，那么它也会被自动取消注册。
    3）whereis(Atom) -> Pid | undefined：判断AnAtom是否已经被其他进程注册。如果成功，则返回进程标识符Pid。
        如果AnAtom没有与之相对应的进程，那么就返回原子undefined。
    4）registered() -> [AnAtom ::atom()]：返回一个系统中所有已经注册的名称列表。

   ### 进程监控
   Erlang 对于进程处理理念之一是“任其崩溃”和“让其他进程修复”,常规Erlang系统中有很多进程同时运行，进程之间可能相互依赖，
   这么复杂的情况之下怎么实现该理念呢？Erlang除了提供exception，try catch等语法，还支持Link和Monitor两种监控进程的机制，
   使得所有进程可以连接起来，组成一个整体。当某个进程出错退出时，其他进程都会收到该进程退出的消息通知。有了这些特点，使用erlang
   建立一个简单，并且健壮的系统就不是什么难事。
   #### 进程双向监控-Link
   相关API link(Pid), A进程调用了link(Pid) 则A进程与Pid之间就建立起了双向连接，如果两个进程相连接，如果其中一个终止时，
   讲发送exit信号给另一方，使其终止，同时终止进程会依次发送exit信号给所有与其连接的进程，这使得exit信号在系统内层层蔓延。 
   该函数连接不存在的进程时会导致发起连接的进程终止
   spawn_link()系列函数 它与link(Pid)的差别就是 原子性与非原子性
   unlink(Pid) 移除调用进程与Pid的连接
   通过调用process_flag(trap_exit, true)可以设置捕捉exit信号，
   假如有A,B两个进程且彼此link
   总结...
   1.当A的结束原因是normal时(进程正常执行完就是normal)，B是不会退出的，此时link机制不发生作用
   2.若A的结束原因是killed，例如调用exit（PidA，kill） ，则无论B是否有设置trap_exit，B都会terminate，此时退出信号捕捉机制是无效的
   3.若A的结束原因不是normal也不是killed(例如exit(PidA,Reason))，那么B在设置了trap_exit时，会捕捉到退出信号，
   取而代之的是收取到一条消息｛‘EXIT’，Pid，Reason｝，这时B不会结束，用户可以根据收到的消息对A进程的结束进行处理；若B没有设置trap_exit，B就会terminate
   
   |捕获状态               |退出信号(原因)         |动作                                     |
   | :-------------------| ------------------: | :--------------------------------------:|  
   |false                |    normal           |    不做任何事                             |
   |false                |    kill             |    消亡,向链接的进程广播退出信号(killed)     |
   |false                |    X                |    消亡,向链接的进程广播退出信号X            |
   |true                 |    normal           |    接收到{'EXIT', Pid, nomal}            |
   |true                 |    kill             |    消亡,向链接的进程广播退出信号(killed)     |
   |true                 |    X                |    将{'EXIT', Pid, X} 加入到邮箱          |
   #### 监视器(monitor)
   相关API
   monitor(process, monitor_process_identifier()) %monitor_process_identifier() 为Pid或者已注册的进程名称
   demonitor(MonitorRef)
   demonitor(MonitorRef, OptionList)
   监视器与link不同的是它是单向式观察一些进程终止，各个监视器通过Erlang的引用相互区分，是调用monitor返回的，具有唯一性，
   而且A进程可以设置多个对B进程的监视器，每一个通过不同的引用区分。
   当被监视的进程终止时，一条格式{'Down',Reference, process, Pid, Reason}的消息会被发给监视此进程的进程
   调用erlang:demonitor(Reference)可以移除监视器，
   调用erlang:demonitor(Reference,[flush])可以让该监视进程邮箱中所有与Reference对应的{'DOWN', Reference,process,Pid,Reason}
   的消息被冲刷掉。
   如果尝试监视一个不存在的进程会导致收到一条{'DOWN', Reference, process, Pid,Reason}的消息，其中Reason为noproc，这和link()不一样

## 进程调度
就目前计算机体系结构而言，任何进程或线程要执行就需要得到CPU资源，对于erlang的进程同样如此。erlang虚拟机同时存在成千上万的进程，
但是cpu核心数又是有限的，所有erlang并发特性就需要一个合适的调度规则来安排各个进程的运行，
简单而言，erlang虚拟机调度程序保留两个队列，准备好运行的就绪队列以及等待接收消息的进程的等待队列。当等待队列中的进程收到消息或获
得超时时，它将被移动到就绪队列。调度程序从就绪队列中选择第一个进程并将其交给BEAM执行一个时间片。当时间片用完时，BEAM会抢占正在
运行的进程，并将进程添加到就绪队列的末尾。如果在时间片用完之前在接收中阻止了进程，则会将其添加到等待队列中。

Erlang调度器主要有以下特点：                                                               
1. 进程调度运行在用户空间 ：Erlang进程不同于操作系统进程，Erlang的进程调度也跟操作系统完全没有关系，是由Erlang虚拟机来完成的；
2. 调度是抢占式的：每一个进程在创建时，都会分配一个固定数目的reduction（这个数量默认值是2000），每一次操作（函数调用），
    reduction就会减少，当这个数量减少到0时或者进程没有匹配的消息时，抢占就会发生（无视优先级）；  
3. 每个进程公平的使用CPU：每个进程分配相同数量的reduction，可以保证进程可以公平的（不是相等的）使用CPU资源
4. 调度器保证软实时性：Erlang中的进程有优先级，调度器可以保证在下一次调度发生时，高优先级的进程可以优先得到执行。

Reduction
受操作系统中基于时间片调度算法的影响，一开始知道有reduction这个概念时，一直想搞清楚这个reduction到底对应多长的绝对时间，不过，
从Erlang本身对reduction的使用来看，完全没有必要纠结这个问题。《Erlang编程指南》一书中对reduction的说明如下：
程序中的每一个命令，无论它是一个函数调用，还是一个算术操作，或者内置函数，都会分配一定数量的reduction。虚拟机使用这个值来衡量一个
进程的活动水平。

进程优先级
Erlang进程有四种优先级：max, high, normal, low（max只在Erlang运行时系统内部使用，普通进程不能使用）。Erlang运行时有两个
运行队列对应着max和high优先级的运行任务，normal和low在同一个队列中。调度器在调度发生时，总是首先查看具体max优先级的进程队列，
如果队列中有可以进行的进程，就会运行，直到这个队列为空。然后会对high优先级的进程队列做同样的操作（在SMP环境，因为同时有几个调度器，所以在同一时间，可能会有不同优先级的任务在同时运行；
但在同一个调度器中，同一时间，肯定是高优先级的任务优先运行）。普通进程在创建时，一般是normal优先级。normal和low优先级的进程只有
在系统中没有max和high优先级的进程可运行时才会被调度到。通常情况下，normal和low优先级的进程交替执行，low优先级获得CPU资源相对
更少（一般情况下）：low优先级的任务只有在运行了normal优先级任务特定次数后（在R15B中，这个数字是8）才会被调度到（也就是说只有
在调度了8个normal优先级的进程后，low优先级的进程才会被调度到，即使low优先级的进程比normal优先级的进程更早进入调度队列，这种
机制可能会引起优先级反转：假如你有成千上万的活动normal进程，而只有几个low优先级进程，那么相比normal进程，low优先级可能会获得
更多的CPU资源）。

## 进程发送消息
Erlang系统中，进程之间的通信是通过消息传递来完成的。消息使用Pid ! Message的形式发送，通过receive语句获取。每个Erlang进程
都有用来存储传入消息的信箱。当一个消息发送的时候，它会从发送进程中拷贝到接收进程的信箱，并以它们到达的时间次序存储。消息的传递是
异步的，一个发送进程不会在发送消息后被暂停。

上面提到发送消息时，会在两个进程之间存在消息复制，为什么需要复制呢？这就跟进程的堆内存有关。虽然在Erlang的文档（heap_type）中
说明堆内存有三种类型：private，shared，hybrid，但是在实际的代码中，只有两种private和hybrid
（参见[$R15B_OTP_SRC/erts/emulator/beam/erl_bif_info.c --> system_info_1]），
（参见[$R15B_OTP_SRC/erts/Makefile.in：# Until hybrid is nofrag, don't build it.），
也就是说Erlang目前的堆内存只有一种：private。
private类型的堆内存是跟shared类型相对的：shared是指所有线程共享同一块内存（比如Java），多个线程对同一块内存的访问需要锁保护；
而private类型的堆内存是指每个进程独享一块内存，对于内存的访问不需要锁保护。
在Erlang的private堆内存架构下，发送消息需要做三件事件：
  1. 计算消息的大小，并在接收进程的内存空间中给消息分配内存；
  2. 将消息的内容拷贝到接收进程的堆内存中；
  3. 最后将消息的地址添加到接收进程的消息队列。
从上面的步骤可以看出，拷贝消息的代码是O(n)，n是消息的长度，也就是说消息越长，花费越大。所以在使用Erlang时，要避免大数据量的大消息传递。

在shared堆内存架构下，发送消息只需要O(1)（只传递消息地址），那为什么Erlang要默认选择private类型的堆内存呢？
其实这跟后面要讲到的Erlang的GC相关：private的优势就是GC的延迟很低，可以很快的完成（因为只保存一个进程的数据，
GC扫描时的数据量很小）。在SMP环境下，实际上每个进程有两个消息队列。进程发送消息时，实际上消息是添加到目标进程的公有队列
（通过锁来保证互斥访问）；而目标进程在消费消息时，实际上是在自己的私有消息队列上处理的，从而减小锁带来的访问开销。但是，
如果目标进程在自己的私有消息队列上无法匹配到消息，那么公有队列中的消息将被添加到私有队列。

## 进程接收消息
```erlang
receive
	Pattern1 [when Guard1] ->
		Expression1;
	Pattern2 [when Guard2] ->
		Expression2;
	...
	after T ->
		ExpressionTimeout
end
```
整个过程如下
  1. 当我们输入receive语句时，我们启动一个计时器(如果有after T)。
  2. 获取邮箱中的第一个消息，并尝试将其与Pattern1、Pattern2等进行匹配。
     如果匹配成功，则从邮箱中删除消息，并计算模式后面的表达式。
  3. 如果receive语句中的任何模式都不匹配邮箱中的第一个消息，那么第一个消息将从邮箱中删除并放入“save队列”中。
     然后尝试邮箱中的第二条消息。重复此过程，直到找到匹配的消息或检查邮箱中的所有消息为止。
  4. 如果邮箱中的所有消息都不匹配，则进程将被挂起，并在下次将新消息放入邮箱时重新安排执行时间。注意，当新消息到达时，
     保存队列中的消息不会重新匹配;只匹配新消息(   Erlang的实现是非常“聪明”的，并且能够最小化每个消息被接收方的receive测试的次数)
  5. 一旦匹配了消息，那么所有放入save队列的消息都将按照到达进程的顺序重新进入邮箱。如果设置了计时器，
     则清除计时器。
  6. 如果计时器在等待消息时超时，则计算表达式ExpressionsTimeout，并按到达进程的顺序将任何保存的消息放回邮箱。

## 进程GC
erlang 进程GC
Memory Layout 内存分布

在我们深入垃圾回收机制之前,我们先来看看Erlang进程的内存布局. 一个Erlang进程的内存布局通常分为是三个部分(有人认为是四个部分, 
把mailbox作为单独的一个部分), 进程控制块, 堆和栈,和普通的Linux进程的内存布局非常类似.
```
            Shared Heap                        Erlang Process Memory Layout                  
                                                                                             
 +----------------------------------+      +----------------------------------+              
 |                                  |      |                                  |              
 |                                  |      |  PID / Status / Registered Name  |       Process
 |                                  |      |                                  |       Control
 |                                  |      |   Initial Call / Current Call    +---->  Block  
 |                                  |      |                                  |       (PCB)  
 |                                  |      |         Mailbox Pointers         |              
 |                                  |      |                                  |              
 |                                  |      +----------------------------------+              
 |                                  |      |                                  |              
 |                                  |      |        Function Parameters       |              
 |                                  |      |                                  |       Process
 |                                  |      |         Return Addresses         +---->  Stack  
 |                                  |      |                                  |              
 |    +--------------+              |      |         Local Variables          |              
 |    |              |              |      |                                  |              
 |    | +------------+--+           |      +-------------------------------+--+              
 |    | |               |           |      |                               |  |              
 |    | | +-------------+--+        |      |  ^                            v  +---->  Free   
 |    | | |                |        |      |  |                               |       Space  
 |    | | | +--------------+-+      |      +--+-------------------------------+              
 |    +-+ | |                |      |      |                                  |              
 |      +-+ |  Refc Binary   |      |      |  Mailbox Messages (Linked List)  |              
 |        +-+                |      |      |                                  |              
 |          +------^---------+      |      |  Compound Terms (List, Tuples)   |       Process
 |                 |                |      |                                  +---->  Private
 |                 |                |      |     Terms Larger than a word     |       Heap   
 |                 |                |      |                                  |              
 |                 +--+ ProcBin +-------------+ Pointers to Large Binaries    |              
 |                                  |      |                                  |              
 +----------------------------------+      +----------------------------------+     
```         
进程控制块: 进程控制块持有关于进程的一些信息, 比如PID, 进程状态(running, waitting), 进程注册名, 初始和当前调用,
指向进程mailbox的指针

栈: 栈是向下增长的, 栈持有函数调用参数,函数返回地址,本地变量以及一些临时空间用来计算表达式.

堆: 堆是向上增长的, 堆持有进程的mailbox, 复合terms(Lists, Tuples, Binaries),以及大于一个机器字的对象(比如浮点数对象). 
大于64个字节的二进制terms,被称为Reference Counted Binary,  他们不是存在进程私有堆里面,他们是存在一个大的共享堆里,所有进程
都可以通过指向RefC Binary的指针来访问该共享堆,RefC Binary指针本身是存在进程私有堆里面的.

GC Details
为了更准确的解释默认的Erlang垃圾回收机制, 实际上运行在每个独立Erlang进程内部的是分代拷贝垃圾回收机制, 还有一个引用计数的
垃圾回收运行在共享堆上.

Private Heap GC 私有堆垃圾回收
私有堆的垃圾回收是分代的. 分代机制把进程的堆内存分为两个部分,年轻代和年老代. 区分是基于这样一个考虑, 如果一个对象在运行
一次垃圾回收之后没有被回收,那么这个对象短期内被回收的可能性就很低. 所以, 年轻代就用来存储新分配的数据,年老代就用来存放运行
一定次数的垃圾回收之后依然幸存的数据. 这样的区分可以帮助GC减少对那些很可能还不是垃圾的数据不必要的扫描. 对应于此, Erlang的
GC扫描有两个策略, Generational(Minor) 和 Fullsweep(Major).  Generational GC只回收年轻代的区域, 而Fullsweep则同时回收年轻代和
年老代.

下面我们一起来review一下一个新创建的Erlang进程触发GC的步骤, 假设以下不同的场景:

场景 1: 

Spawn > No GC > Terminate
假设一个生存期较短的进程, 在存活期间使用的堆内存也没有超过 min_heap_size,那么在进程结束是全部内存即被回收.

场景 2:

Spawn > Fullsweep > Generational > Terminate
假设一个新创建的进程,当进程的数据增长超过了min_heap_size时, fullsweep GC即被触发, 因为在此之前还没有任何GC被触发,所以堆区
还没有被分成年轻代和年老代. 在第一次fullsweep GC结束以后, 堆区就会被分为年轻代和年老代了, 从这个时候起,  GC的策略就被切换为 
generational GC了, 直到进程结束.

场景 3:

Spawn > Fullsweep > Generational > Fullsweep > Generational > ... > Terminate
在某些情景下, GC策略会从generation再切换回fullsweep. 一种情景是, 在运行了一定次数(fullsweep_after)的genereration GC之后,
系统会再次切换回fullsweep. 这个参数fullsweep_after可以是全局的也可以是单进程的. 全局的值可以通过函数erlang:system_info(fullsweep_after)获取, 
进程的可以通过函数erlang:process_info(self(),garbage_collection)来获取. 另外一种情景是, 当generation GC(minor GC)不能够收集到足够的内存空间时. 
最后一种情况是, 当手动调用函数garbage_collector(PID)时. 在运行fullsweep之后, GC策略再次切换回generation GC直到以上的任意一个情景再次出现.

场景 4:

Spawn > Fullsweep > Generational > Fullsweep > Increase Heap > Fullsweep > ... > Terminate
假设在场景3里面,第二个fullsweep GC依然没有回收到足够的内存, 那么系统就会为进程增加堆内存, 然后该进程就回到第一个场景,像刚创建的进程一样首先
开始一个fullsweep,然后循环往复.

那么对Erlang来说, 既然这些垃圾回收机制都是自动完成的, 为什么我们需要花时间去了解学习呢? 首先, 通过调整GC的策略可以使你的系统运行的更快. 其次,
 了解GC可以帮助我们从GC的角度来理解为什么Erlang是一个软实时的系统平台. 因为每个进程有自己的私有内存空间和私有GC,所以每次GC发生的时候只在进程
 内部进行,只stop本进程, 不会stop其他进程,这正是一个软实时系统所需要的.

Shared Heap GC 共享堆垃圾回收
共享堆的GC是通过引用计数来实现的. 共享堆里面的每个对象都有一个引用计数,这个计数就是表示该对象被多少个Erlang进程持有(对象的指针存在进程的私有堆里).
 如果一个对象的引入计数变成0的时候就表示该对象不可访问可以被回收了. 
 
 进程调度 
 ## ## 进程调度
就目前计算机体系结构而言，任何进程或线程要执行就需要得到CPU资源，对于erlang的进程同样如此。erlang虚拟机同时存在成千上万的进程，
但是cpu核心数又是有限的，所有erlang并发特性就需要一个合适的调度规则来安排各个进程的运行，
简单而言，erlang虚拟机调度程序保留两个队列，准备好运行的就绪队列以及等待接收消息的进程的等待队列。当等待队列中的进程收到消息或获
得超时时，它将被移动到就绪队列。调度程序从就绪队列中选择第一个进程并将其交给BEAM执行一个时间片。当时间片用完时，BEAM会抢占正在
运行的进程，并将进程添加到就绪队列的末尾。如果在时间片用完之前在接收中阻止了进程，则会将其添加到等待队列中。

Erlang调度器主要有以下特点：                                                               
1. 进程调度运行在用户空间 ：Erlang进程不同于操作系统进程，Erlang的进程调度也跟操作系统完全没有关系，是由Erlang虚拟机来完成的；
2. 调度是抢占式的：每一个进程在创建时，都会分配一个固定数目的reduction（这个数量默认值是2000），每一次操作（函数调用），
    reduction就会减少，当这个数量减少到0时或者进程没有匹配的消息时，抢占就会发生（无视优先级）；  
3. 每个进程公平的使用CPU：每个进程分配相同数量的reduction，可以保证进程可以公平的（不是相等的）使用CPU资源
4. 调度器保证软实时性：Erlang中的进程有优先级，调度器可以保证在下一次调度发生时，高优先级的进程可以优先得到执行。

1. What operators does Erlang have?
```
Arithmetic operators: + - * / div rem 
Comparison operators: =:= == =/= /= > >= < =< 
Logical operators: and andalso or orelse 
Bitwise operators: bsl bsr Bitwise logical operators: band Bor bxor bnot
```
---------------------
   
     
  
  
      
