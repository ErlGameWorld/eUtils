#### erlang 各种 优化设置 
     一、 erl启动时参数:
    +K true  开启epoll调度，在linux中开启epoll，会大大增加调度的效率
    +A 100   异步线程池，为某些port调用服
    +P 1024000  最大进程数
    +Q 65535 最大port数
    +sbt db 绑定调度器，绑定后调度器的任务队列不会在各个CPU线程之间跃迁，结合sub使用，可以让CPU负载均衡的同时也避免了大量的跃迁发生。
         将scheduler绑定到具体的cpu核心上，再配合erlang进程和port绑定，可以显著提升性能，但是如果绑定错误，反而会有反效果
         （ 进程调度器绑定：erlang:process_flag(scheduler, 1)，当进程使用了port时，还需要port绑定支持，防止进程在不同调度器间迁移引起性能损失，如cache、跨numa node拷贝等，当进程使用了port时，主要是套接字，若进程与port不在一个scheduler上，可能会引发严重的epoll fd锁竞争及跨numa node拷贝，导致性能严重下降）
    注意：一个linux系统中，最好只有一个evm开启此选项，若同时有多个erlang虚拟机在系统中运行，还是关闭为好
   
    +sub true  开启CPU负载均衡，false的时候是采用的CPU密集调度策略，优先在某个CPU线程上运行任务，直到该CPU负载较高为止。
    +swct eager  此选项设置为eager后，CPU将更频繁的被唤醒，可以增加CPU利用率
    +spp true  开启并行port并行调度队列，当开启后会大大增加系统吞吐量，如果关闭，则会牺牲吞吐量换取更低的延迟。
    +zdbbl 65536  分布式erlang的端口buffer大小，当buffer满的时候，向分布式的远程端口发送消息会阻塞
    
    二、erlang内部进程启动参数
    示例：创建一个新进程并进行注册,该进程是全局唯一的自增ID生成进程，因此无法做多进程处理，这个时候单进程的性能就是至关重要的
    首先，出于性能和功能考虑，这个进程不是gen_server；其次进行了部分参数调优能
    register(num_generator, spawn_opt(?MODULE, init, [],[{priority,high},{scheduler,0},{min_heap_size, 65536 * 2},{min_bin_vheap_size,65536 * 2}])).
    参数讲解:
    1.priority 
        erlang是公平调度策略，因此默认情况下每个进程得到的运行时间片是相同的：2000reductions,但是对于我们的应用场景来说，这个进程应该是优先级较高的，需要得到更多的调度，因此设置为high,还可以设置为max,但是max是系统进程的预留优先级，用high即可
    2. scheduler 
        将该进程绑定到指定的scheduler上，防止进程的任务被scheduler分配来分配去，可以减少CPU调用,注意这个和+sbt db是不同的，+sbt db是防治调度器的任务队列在CPU线程间跃迁，scheduler是为了防止进程在时间片切换过程中被分配给其它的调度器
    3.min_heap_size
        进程初始堆大小，用内存换CPU的典型做法，增大初始大小，可以显著降低GC次数和内存再分配次数， 减少处理过程中产生大量term，尤其是list时的gc次数
    4.min_bin_vheap_size
    进程初始二进制堆大小，当该进程对于binary数据的处理交换很多时，可以获得和增大min_heap_size一样的效果， 减少大量消息到达或处理过程中产生大量binary时的gc次数

     三、port(socket)调优
    示例：服务器监听端口，接受客户端请求。典型应用场景web服务器，需要实现高吞吐，低延迟的目标
    Res = gen_tcp:listen(Port, [binary,
    {reuseaddr, true},
    {nodelay, true},
    {delay_send,true},
    {high_watermark,64 * 1024},
    {send_timeout, 30000},
    {send_timeout_close, true},
    {keepalive, true}])
    
    参数详解:
    binary:
    接收到客户端的消息后，作为binary来处理，binary在erlang中是很高效的数据结构，超过64字节，就是全局保存的，因此在很多操作下是不需要复制的，仅仅复制binary的指针即可，详细请搜索refc binary，注意：binary大量使用需要有丰富的经验，不然可能会内存泄漏
    reuseaddr:
    允许系统复用port,对于高吞吐的系统，这个参数很重要,请搜索:linux port 复用
    nodelay:
    开启linux中的TCP_NODELAY参数，请搜索:TCP_NODELAY 40毫秒延迟
    delay_send:
    默认的erlang port消息发送，是直接发送，若失败则排队处理，然后由调度器进行队列poll操作，如果设置为true,那么就不尝试直接发送，而且扔进队列，等待poll，开启选项会增加一点点消息延迟，换来吞吐量的大量提升
    high_watermark:
    port的发送缓存，缓存满了后，下次发送会直接阻塞，直到缓存低于某个阈值low_watermark。如果是密集网络IO系统，请增大该buffer,避免发送阻塞
         延迟发送：{delay_send, true}，聚合若干小消息为一个大消息，性能提升显著
         发送高低水位：{high_watermark, 128 * 1024} | {low_watermark, 64 * 1024}，辅助delay_send使用，delay_send的聚合缓冲区大小为high_watermark，数据缓存到high_watermark后，将阻塞port_command，使用send发送数据，直到缓冲区大小降低到low_watermark后，解除阻塞，通常这些值越大越好，但erlang虚拟机允许设置的最大值不超过128K
         发送缓冲大小：{sndbuf, 16 * 1024}，操作系统对套接字的发送缓冲大小，在延迟发送时有效，越大越好，但有极值
         接收缓冲大小：{recbuf, 16 * 1024}，操作系统对套接字的接收缓冲大小
    send_timeout:
    在high_watermark中提到了发送阻塞，如果阻塞超过这个时间，那么就会超时，发送直接返回,停止发送
    send_timeout_close:
    如果发生了send_timeout同时设置了send_timeout_close选项，那么超时后，会直接关闭socket.如果发送进程不是很重要，例如web用户进程，强烈建议开启这个选项，当发送30秒超时的时候，就说明该用户出现了很大的麻烦，断开连接是最理想的做法，否则可能出现很多奇怪的bug.
    keepalive:
    遵循HTTP/1.1协议的keepalive规定,这个根据业务需求选择是否开启,如果同一个客户端会连续发起http请求，那么建议设置为true,避免多次TCP握手
    示例：服务器发起大量的http请求,在优化了参数后，同样的吞吐量所耗费的时间是未优化前的1/3 - 1/2(经过严苛的测试得出的数据)
    
    inets:start(),
       httpc:set_options([{max_keep_alive_length,500},{max_sessions,100},{nodelay,true},{reuseaddr,true}]),
    
    参数详解:
    max_keep_alive_length:  
    在同一条http连接上允许发送的最大包数，默认为5，超过5个包，就会重连
    max_sessions:
    跟目标服务器之间最大的并行http连接数目,大大的增加了数据上行吞吐量
    nodelay_true:
    见上文
    reuseaddr:
    
      6. 数据结构：
         减少遍历，尽量使用API提供的操作
         由于各种类型的变量实际可以当做c的指针，因此erlang语言级的操作并不会有太大代价
         lists：reverse为c代码实现，性能较高，依赖于该接口实现的lists API性能都不差，避免list遍历，[||]和foreach性能是foldl的2倍，不在非必要的时候遍历list
         dict：find为微秒级操作，内部通过动态hash实现，数据结构先有若干槽位，后根据数据规模变大而逐步增加槽位，fold遍历性能低下
         gb_trees：lookup为微秒级操作，内部通过一个大的元组实现，iterator+next遍历性能低下，比list的foldl还要低2个数量级
    9. 文件预读，批量写，缓存：
        这些方式都是局部性的体现：
        预读：读空间局部性，文件提供了read_ahead选项
        批量写：写空间局部性
          对于文件写或套接字发送，存在若干级别的批量写：
            1. erlang进程级：进程内部通过list缓存数据
            2. erlang虚拟机：不管是efile还是inet的driver，都提供了批量写的选项delayed_write|delay_send，
               它们对大量的异步写性能提升很有效
            3. 操作系统级：操作系统内部有文件写缓冲及套接字写缓冲
            4. 硬件级：cache等
        缓存：读写时间局部性，读写空间局部性，主要通过操作系统系统，erlang虚拟机没有内部的缓存
    10.套接字标志设置：
        延迟发送：{delay_send, true}，聚合若干小消息为一个大消息，性能提升显著
        发送高低水位：{high_watermark, 128 * 1024} | {low_watermark, 64 * 1024}，辅助delay_send使用，delay_send的聚合缓冲区大小为high_watermark，数据缓存到high_watermark后，将阻塞port_command，使用send发送数据，直到缓冲区大小降低到low_watermark后，解除阻塞，通常这些值越大越好，但erlang虚拟机允许设置的最大值不超过128K
        发送缓冲大小：{sndbuf, 16 * 1024}，操作系统对套接字的发送缓冲大小，在延迟发送时有效，越大越好，但有极值
        接收缓冲大小：{recbuf, 16 * 1024}，操作系统对套接字的接收缓冲大小
        
#### Erlang 虚拟机调优
    目录
    SMP
    Schedulers
    Port Settings
    Asynchronous Thread Pool
    Kernel Polling
    Warning Messages
    Process Limit
    Distribution Buffer
    Erlang Built-in Storage
    Crash Dumps
    Net Kernel Tick Time
    Shutdown Time
    Riak 是用Erlang语言写的,运行在Erlang虚拟机之上.所以Erlang虚拟机的调优对Riak的性能优化就显得尤为重要. Erlang虚拟机本身提供了非常多的配置参数对性能调优, Riak支持其中的一部分参数,你可以在每个node的Riak配置文件中进行设置.


下表列出了其中的一部分,左边一列是Erlang中的参数名称, 右边一列是在Riak中的参数名称.
```Erlang parameter	Riak parameter
+A	erlang.async_threads
+K	erlang.K
+P	erlang.process_limit
+Q	erlang.max_ports
+S	erlang.schedulers.total, erlang.schedulers.online
+W	erlang.W
+a	erlang.async_threads.stack_size
+e	erlang.max_ets_tables
+scl	erlang.schedulers.compaction_of_load
+sfwi	erlang.schedulers.force_wakeup_interval
-smp	erlang.smp
+sub	erlang.schedulers.utilization_balancing
+zdbbl	erlang.distribution_buffer_size
-kernel net_ticktime	erlang.distribution.net_ticktime
-env FULLSWEEP_AFTER	erlang.fullsweep_after
-env ERL_CRASH_DUMP	erlang.crash_dump
-env ERL_MAX_ETS_TABLES	erlang.max_ets_tables
-name	nodename
```
Note on upgrading to 2.0
在Riak2.0版本之前, Erlang虚拟机相关的参数放在配置文件 vm.args 里面. 在2.0及之后的版本中, 所有Erlang虚拟机相关的配置参数放在配置文件 riak.conf 里面. 如果你从Riak2.0之前的版本升级到Riak 2.0, 你仍然可以继续使用旧的配置文件 vm.args. 但是, 如果你同时设置了配置文件 vm.args 和riak.conf,  在 vm.args里面的配置将会覆盖riak.conf里面的配置.
##### SMP 
    有些操作系统提供Erlang虚拟机对称多处理器能力(SMP)以利用多处理器硬件架构的优势. SMP的支持可以通过设置erlang.smp参数来打开和关闭, 默认是打开的. 下面的例子是关闭SMP的支持.
    riak.conf
    erlang.smp = disable
    由于Riak也可以运行在一些不支持SMP的操作系统上, 所以在使用之前需要确认操作系统是否支持SMP,如果操作系统本身不支持,那么需要在启动Riak集群之前在配置文件riak.conf中关闭SMP的选项.
    
    比较安全的一个选择是把erlang.smp设置成auto, 这个选项会指示Erlang虚拟机启动SMP支持之前检查操作系统是否支持以及是否有一个以上的逻辑处理器,只有这两个条件都满足的时候,Erlang虚拟机才启动SMP支持.
    
##### Schedulers
    Note on missing scheduler flags
    We recommend that all users set the +sfwi to 500 (milliseconds) and the +sclflag to false if using the older, vm.args-based configuration system. If you are using the new, riak.conf-based configuration system, the corresponding parameters are erlang.schedulers.force_wakeup_interval anderlang.schedulers.compaction_of_load.
    Please note that you will need to uncomment the appropriate lines in your riak.conf for this configuration to take effect.
    如果在Erlang虚拟机里已经打开了支持SMP的选项, 比如erlang.smp已经被设置成enabled 或者auto,而且机器本身超过一个逻辑处理器同时也支持SMP, 那么当你启动Riak的时候, 你可以配置逻辑处理器的数量或者调度线程的数量,同时也可以设置online线程的数量. 
    全部调度线程的数量可以通过参数erlang.schedulers.total来设置, online线程的数量则是通过参数erlang.schedulers.online来配置. 这两个参数可以分别对应到Erlang虚拟机的参数Schedulers 和SchedulersOnline.
    两个参数的最大值都是1024,  参数并没有统一的默认值. 但是, Erlang 虚拟机自己会尝试去判定有多少配置的CPU(core)和可用的CPU(core). 如果Erlang虚拟机能够做出这个判定,那么参数schedulers.total会默认设置成配置的CPU(core)数量,
    参数schedulers.online会默认设置成可用的CPU(core)数量. 但是, 如果Erlang虚拟机不能做出判定, 两个参数的默认值将会设置成1.
    如果两个参数中的任意一个被设置成负数, 那么意味着这个参数值将会被设成默认配置的处理器数量(如果scheduler.total是负数)或者可用的处理器数量(如果schedulers.online是负数) 减去配置的负值. 比如, 如果机器配置有100个cpu(cores)然后参数schedulers.total配置为-50, 计算以后的值就是50. 
    如果两个参数中的任意一个被设置为0,两个值都会被重新设为默认值.
    如果SMP支持被关闭, 比如erlang.smp被设成disabled或者设成auto 但是机器本身不支持SMP或者机器只有一个逻辑处理器,那么两个参数schedulers.total 和 schedulers.online都将会被忽略.
    
Scheduler Wakeup Interval
调度器唤醒是一个可选处理, 通过这个Erlang 虚拟机调度器被周期性的扫描来判定是否已经陷入睡眠, 比如是否调度器有一个空的运行列表. 这个扫描时间间隔可以通过参数erlang.schedulers.force_wakeup_interval设置, 单位为毫秒.这个参数对应于Erlang虚拟机的+sfwi选项.该参数默认设为0, 不激活调度器唤醒功能.
Erlang在R15Bx版本里有把调度器睡眠过于频繁的倾向,如果你使用的是更新的版本,比如Riak2.0 及以后, 那多数情况下不需要启动唤醒功能.
注: OTP的工程师曾经解释过这个功能,如果需要调度的任务不是很多,没有很多task在运行列表上的话, R15B的Erlang虚拟机会倾向于把这些task尽量集中到尽可能少的调度器上来调度, 睡眠没有调度任务的调度器, 这样可以减少调度器之间的通信花费overhead, 提高CPU的利用率. 但这个也是一个trade off, 具体还是需要用户来根据自己的实际环境来调优.  因为一旦task的数量增加比较多,或者task数量没有增加但是task本身比较耗时,那么很可能就会触发调度器的唤醒, 而唤醒调度器是比较expensive的操作, 如果频繁睡眠唤醒的话,可能会得不偿失.

#####  Scheduler Compaction and Balancing
    Erlang调度器提供了两种方式来分发负载到不同的调度器上, 集中负载和utilization balancing.
    集中负载是默认打开的, 打开的时候Erlang虚拟机会尝试去尽可能多的使调度器繁忙,比如通过把任务集中到有限的几个调度器上(假设这几个有限的调度器充分运行的情况下可以调度完目前的tasks)使这几个调度器一直有工作做(not run out of work). 为了达到这个目的, 当虚拟机分配任务的时候会考虑哪些调度器应该被分配任务. 用户可以设置参数erlang.schedulers.compaction_of_load为false来关闭这个功能.
    另外一个选项, utilization balancing, 为了支持负载平衡, 默认是关闭的. 如果打开了这个选项, Erlang虚拟机则努力在不同调度器之间平衡调度器的利用. 如果不考虑每个调度器没有任务可调度的频度的话, 可以打开这个设置, erlang.schedulers.utilization_balancing 设为true(老版本里面通过设置+scl false)
    在任何时候, 只可以是使用两个功能中的一个. 如果同时设置这两个选项为false的话, Riak 会默认使用集中负载选项.如果同时设置为true, Riak会使用那个在配置文件riak.conf中最先出现的那个.(如果是旧版本的话,配置文件会是vm.args)
    
##### Port Settings
Riak 使用epmd, Erlang 端口映射Daemon来进行大多数的节点间的通信. 在这个系统里, 集群里的其他节点使用由nodename参数(或者是name in vm.args)来作为节点ID. 比如, riak@10.9.8.7.  在每个节点上, daemon把这些节点ID解析成一个TCP的端口. 用户可以指定一个端口范围给Riak节点来监听使用,同时也可以知道最大数量的并ports/sockets.
Port Range
默认情况下 , epmd绑定到TCP端口4369上并且侦听通配符接口. epmd 默认使用一个不能预测的端口作为节点间的通信, 通过绑定到端口0上， 意味着会使用第一个可用的端口. 这样就使得防火墙非常难配置.
为了是防火墙配置简化, 用户可以指导Erlang虚拟机使用一个有限范围的端口或者单一端口. 这个最小和最大值可以设置在参数erlang.distribution.port_minimum和erlang.distribution.port_maximum里面. 比如, 下面的值被设为3000和5000.
riak.conf
app.config
erlang.distribution.port_range.minimum = 3000
erlang.distribution.port_range.maximum = 5000
用户可以设置Erlang虚拟机使用一个单一端口, 如果只设置了最小值没有设置最大值,则表示使用单一端口. 比如, 下面设置使用单一端口5000.
riak.conf
app.config
erlang.distribution.port_range.minimum = 5000
如果最小端口没有设置, Erlang虚拟机将会在随机的高编号端口上侦听.

##### Maximum Ports
用户可以通过设置参数erlang.max_ports来指定Erlang虚拟机可以使用的最大并发的 ports/sockets数量, 范围从1024到134217727. 默认值是65536. 在vm.args里面对应的参数是+Q 或者-env ERL_MAX_PORTS. 
Asynchronous Thread Pool
如果Erlang虚拟机支持线程可用, 用户可以为Erlang虚拟机设置异步线程池的线程数量, 使用参数erlang.async_threads(+A in vm.args). 线程数量范围从0至1024, 默认值是64,下面的例子是设置成600的情况.
riak.conf
vm.args
erlang.async_threads = 600


#####  Stack Size
除了可以指定异步线程的数量之外, 用户还可以为每个异步线程指定stack size. 参数是erlang.async_threads.stack_size, 对应到Erlang的+a参数. 用户可以在Riak中为这个参数指定size以KB, MB,GB 为单位, 有效的范围值是16至8192个字, 在32位的系统上就是64至32768字节. 该参数没有默认值, 我们建议设置为16K words, 对应为64 KB在32位系统上.  我们建议这么小一个值是考虑到异步线程数量可能会很大. 
注:The 64 KB default is enough for drivers delivered with Erlang/OTP but might not be large enough to accommodate drivers that use the driver_async()functionality, documented here. We recommend setting higher values with caution, always keeping the number of available threads in mind.
Kernel Polling
如果系统支持, 用户可以在Erlang中利用内核轮询. 内核轮询可以在使用很多文件描述符的时候提高性能. 在使用中的文件描述符越多, 内核轮询发挥的作用就越大. 该选择在Riak的Erlang虚拟机中是默认打开的, 该参数对应到Erlang虚拟机中的+K参数

#####  Warning Messages
Erlang虚拟机的error_logger 是一个事件管理器, 从Erlang运行时系统注册错误, 告警和信息事件. 默认情况下, error_logger的信息事件被映射为告警,但是用户可以设置映射成错误或者信息. 该设置为参数erlang.W, 可以设置的值为w(warning), errors 或者i(info reports).

#####  Process Limit
参数erlang.process_limit可以用来设置系统同时存在的最大进程数量(对应到Erlang的+P参数), 有效范围从1024至134217727. 默认值是256000.

#####  Distribution Buffer
用户可以通过参数erlang.distribution_buffer_size设置Erlang虚拟机的distribution buffer busy limit(对应到Erlang的+zdbbl参数). 修改这个参数对那些有许多busy dist port事件的节点可能会有帮助, 默认值是32MB, 最大值是2097151KB. 增大这个参数可以允许进程缓存更多的待发消息, 当缓存满的时候，发送线程被挂起直到缓存减小到设定值. 所以, 更大的缓存有助于降低延迟以及增加吞吐量,代价就是使用了更多的RAM. 用户需要根据机器的RAM资源来考虑设定这个值.

##### Erlang Built-in Storage
Erlang使用一个内置的数据库,ets(Erlang Term Storage)用来快速访问内存(constant access time rather than logarithmic access time). erts 表的最大数量设置在参数erlang.max_erts_tables里面, 默认值是256000,这个值要大于Erlang虚拟机自身的默认值1400(对应到vm.args 的参数e). 更大的erlang.max_erts_tables值可以提供更快的数据访问,代价是消耗更高的内存.

#####  Crash Dumps
默认情况下, Riak 的Erlang crash dumps文件是存放在位置./log/erl_crash.dump. 用户可以通过设置参数erlang.crash_dump来更改存放位置. 该参数对应到Erlang虚拟机的ERL_CRASH_DUMP环境变量.

#####  Net Kernel Tick Time
网络内核是Erlang的一个系统进程, 提供了不同的网络监视形式. 在一个Riak集群里面, 网络内核的功能之一就是去周期性的检测节点存活. Tick time就是这个检查频度, 可以通过erlang.distribution.net_ticktime设置,单位是秒. 该参数对应到vm.args里面的参数-kernal net_ticktime.

#####  Shutdown Time
用户可以设定Erlang虚拟机的关闭时间, 该设置参数为erlang.shutdown_time,默认是10秒, 一旦10秒过了, 所有存在的进程就会被杀掉. 减少关闭时间在某些情景下可能是有帮助的, 比如说在测试的时候需要频繁的启停Riak集群. 在vm.args里参数是shutdown_time, 单位是毫秒.    