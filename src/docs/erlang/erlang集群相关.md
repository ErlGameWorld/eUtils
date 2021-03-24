节点连接 

分布式erlang系统中的节点是松散连接的， 第一次使用另一个节点名称 例如调用 spawn(Node, M, F, A)或者
    net_adm:ping(Node)的时候 就会尝试连接该节点
    
   
默认情况下  节点连接是可以传递的 如果节点A连接了节点B 节点B连接了节点C 则节点A会尝试连接到节点C 
    可以通过命令  `-connect_all false`  来关闭这个功能
    
如果想主动断开与某个节点的连接 可以使用 `erlang:disconnect_node(Node)` 强制断开节点连接

Erlang Port Mapper Daemon epmd会在启动Erlang节点的每个主机上自动启动。它负责将符号节点名映射到机器地址。请参见ERTS中的 epmd（1）手册页。
四、跨机器连通防火墙问题
要想连通某个节点，该节点（即被连接的）要保证：
1. epmd的端口（默认是4369）在防火墙打开；
2. erl要加 `-kernel inet_dist_listen_min Min inet_dist_listen_max Max` 设定使用的端口范围（若只有一个端口，则Min==Max），要保证这些端口在防火墙打开，并且这些端口不能全部被占用
也就是要连接某个节点，是和该节点所在机器的epmd以及该节点通讯。所以发起连接的节点不需要上面的2个要求，即所在机器不需要防火墙打开4369端口，也不需要加-kernel inet_dist_listen_min Min inet_dist_listen_max Max

隐藏节点 在分布式erlang系统中 有时候连接到所有节点是不好的 可以用使用 命令行标记 `-hidden` 隐藏节点和其他节点的连接是不可传递的  同样隐藏节点也不会显示在nodes()函数返回的节点列表
这也意味这未将隐藏节点添加到global跟踪的节点集群中，另外`nodes(hidden) or nodes(connected)` 会返回隐藏的节点

节点cookie
身份验证确定允许哪些节点相互通信。在不同Erlang节点的网络中，它以最低的级别内置到系统中。每个节点都有自己的cookie，它是一个Erlang原子。
当一个节点尝试连接到另一个节点时，将对魔术cookie进行比较。如果它们不匹配，则连接的节点拒绝连接。
可以使用 `erlang:set_cookie(node(), Cookie)` 将本地节点的Cookie设置为Cookie, `erlang:get_cookie()` 返回本地节点的cookie
为了使cookie为Cookie1的节点Node1能够连接到具有不同cookie Cookie2的节点Node2或者让Node2接收到Node1的连接，必须首先在Node1处调用
`erlang:set_cookie(Node2, Cookie2)`（该调用不会修改 Node1本地的cookie, 而且这样操作之后 Node1会自动与Node2所连节点列表中的节点cookie都为Cookie1的节点互联，不相同的cookie不会自动互联）
 这样具有多个cookie的分布式系统就可以互联了 
 
 关于分布式的BIFS
 Some useful BIFs for distributed programming (for more information, see the erlang(3) manual page in ERTS:
 
BIF	Description
erlang:disconnect_node(Node)	Forces the disconnection of a node.

erlang:get_cookie()	Returns the magic cookie of the current node.

is_alive()	Returns true if the runtime system is a node and can connect to other nodes, false otherwise.

monitor_node(Node, true|false)	Monitors the status of Node. A message{nodedown, Node} is received if the connection to it is lost.

node()	Returns the name of the current node. Allowed in guards.

node(Arg)	Returns the node where Arg, a pid, reference, or port, is located.

nodes()	Returns a list of all visible nodes this node is connected to.

nodes(Arg)	Depending on Arg, this function can return a list not only of visible nodes, but also hidden nodes and previously known nodes, and so on.

erlang:set_cookie(Node, Cookie)	Sets the magic cookie used when connecting to Node. If Node is the current node, Cookie is used when connecting to all new nodes.

spawn[_link|_opt](Node, Fun)	Creates a process at a remote node.

spawn[_link|opt](Node, Module, FunctionName, Args)	Creates a process at a remote node.

Distribution Command-Line Flags

Examples of command-line flags used for distributed programming (for more information, see the erl(1) manual page in ERTS:

Command-Line Flag	Description

-connect_all false	Only explicit connection set-ups are used.

-hidden	Makes a node into a hidden node.

-name Name	Makes a runtime system into a node, using long node names.

-setcookie Cookie	Same as calling erlang:set_cookie(node(), Cookie).

-sname Name	Makes a runtime system into a node, using short node names.

Distribution Modules Examples of modules useful for distributed programming:
In the Kernel application:

Module	Description

global	A global name registration facility.

global_group	Grouping nodes to global name registration groups.

net_adm	Various Erlang net administration routines.

net_kernel	Erlang networking kernel.


Kernel Modules Useful For Distribution. In the STDLIB application:
Module	Description

slave	Start and control of slave nodes.


%% ***************************************** net_adm 模块 **********************************************

## dns_hostname(Host) -> {ok, Name} | {error, Host}
    Types
        Host = atom() | string()
        Name = string()
    返回的正式名称主机，或 {错误，主机}如果没有这样的名字中找到  

## host_file() -> Hosts | {error, Reason}  
    Types
    Hosts = [Host :: atom()]
    Reason = 
        file:posix() |
        badarg | terminated | system_limit |
        {Line :: integer(), Mod :: module(), Term :: term()}
    读取文件.hosts.erlang，请参阅文件部分 。以列表形式返回此文件中的主机。如果无法读取文件或无法解释文件上的Erlang术语，则返回{error，Reason}。
    
## localhost() -> Name
    Types
        Name = string()
    返回本地主机的名称。如果Erlang以命令行标志-name开头，则Name是标准名称。
    
## names() -> {ok, [{Name, Port}]} | {error, Reason}
## names(Host) -> {ok, [{Name, Port}]} | {error, Reason}
    Types
    Host = atom() | string() | inet:ip_address()
    Name = string()
    Port = integer() >= 0
    Reason = address | file:posix()    
    ie. 
        (arne@dunn)1> net_adm:names().
        {ok,[{"arne",40262}]}
    与epmd -names类似，请参阅 erts：epmd（1）。 主机默认为本地主机。返回epmd在指定主机上注册的Erlang节点的名称和关联的端口号 。如果epmd无法运行，则返回 {error, address}。
    
## ping(Node) -> pong | pang
    Types
    Node = atom()
    Sets up a connection to Node. Returns pong if it is successful, otherwise pang.    
    
## world() -> [node()]
## world(Arg) -> [node()]    
    Types
    Arg = verbosity()
    verbosity() = silent | verbose
    调用Erlang主机文件.hosts.erlang中指定的所有主机的names(Host)，收集答复，然后在所有这些节点上评估ping(Node)。返回已成功ping通的所有节点的列表。
    Arg默认为silent。如果Arg == verbose，则该函数将写入有关将其ping到标准输出的节点的信息。
    当启动一个节点并且最初不知道其他网络节点的名称时，此功能很有用。
    Returns {error, Reason} if host_file() returns {error, Reason}.
    
## world_list(Hosts) -> [node()]
## world_list(Hosts, Arg) -> [node()]
    Types
    Hosts = [atom()]
    Arg = verbosity()
    verbosity() = silent | verbose
    Same as world/0,1, but the hosts are specified as argument instead of being read from .hosts.erlang.   
    
    
## .hosts.erlang
    文件.hosts.erlang由许多以Erlang术语编写的主机名组成。在当前工作目录，用户的主目录和$OTP_ROOT （Erlang / OTP的根目录）中依次查找。
    文件.hosts.erlang的格式必须是每行一个主机名。主机名必须用引号引起来。 
    example
        'super.eua.ericsson.se'.
        'renat.eua.ericsson.se'.
        'grouse.eua.ericsson.se'.
        'gauffin1.eua.ericsson.se'.
        ^ (new line)   
        
%% ***************************************** net_kernel 模块 **********************************************  
## 描述
    网络内核是注册为net_kernel的系统进程， 必须运行才能使分布式Erlang正常工作。该过程的目的是实现BIF的部分spawn / 4和spawn_link / 4并提供对网络的监视。
    使用命令行标志-name或-sname启动一个Erlang节点 ：
    $ erl -sname foobar
    也可以 直接从普通的Erlang Shell提示符下调用net_kernel：start（[foobar]）：
    1> net_kernel：start（[[foobar, shortnames]）。
    {ok，<0.64.0>}
    （foobar @ gringotts）2>
    
    如果节点以命令行标志-sname开头，则节点名称为foob​​ar @ Host，其中Host是主机的简称（不是完全限定的域名）。如果以flag -name开头，则节点名称为foob​​ar @ Host，其中Host是标准域名。有关更多信息，请参见 erl。
    
    通常，引用另一个节点时会自动建立连接。可以通过将内核配置参数dist_auto_connect设置为never来禁用此功能 ，请参阅 kernel（6）。在这种情况下，必须通过调用connect_node / 1显式建立连接 。
    
## allow(Nodes) -> ok | error
    Types
    Nodes = [node()]    
    
    允许访问指定的节点集。
    在第一次调用allow / 1之前，可以连接具有正确cookie的任何节点。当允许/ 1被调用，建立允许节点列表。从（或到）不在该列表中的节点进行的任何访问尝试都将被拒绝。
    随后对allow / 1的调用会将指定的节点添加到允许的节点列表中。无法从列表中删除节点。
    如果Nodes中的任何元素都不是原子，则返回错误。

## connect_node(Node) -> boolean() | ignored
    Types
    Node = node()
    建立与Node的连接。如果已建立连接或已经建立连接，或者Node是本地节点本身，则返回 true。如果连接尝试失败，则返回false；如果本地节点未处于活动状态， 则将其忽略。

## get_net_ticktime() -> Res
    Types
    Res = NetTicktime | {ongoing_change_to, NetTicktime} | ignored
    NetTicktime = integer() >= 1
    获取net_ticktime（请参阅 kernel（6））。
    定义的返回值（Res）：
    NetTicktime
        net_ticktime is NetTicktime seconds.。
    {ongoing_change_to，NetTicktime}
        net_kernel is currently changing net_ticktime to NetTicktime seconds.
    ignored
        The local node is not alive.

##getopts(Node, Options) -> {ok, OptionValues} | {error, Reason} | ignored
    Types
    Node = node()
    Options = [inet:socket_getopt()]
    OptionValues = [inet:socket_setopt()]
    Reason = inet:posix() | noconnection
    获取连接到Node的配电插座的一个或多个选项。
    如果Node是连接的节点，则返回值与inet：getopts（Sock，Options） 中的返回值相同 ，其中Sock是Node的分发套接字。
    返回忽略，如果本地节点是不是活的或 {错误，noconnection}如果节点未连接。

## monitor_nodes(Flag) -> ok | Error
## monitor_nodes(Flag, Options) -> ok | Error
    Types
    Flag = boolean()
    Options = [Option]
    Option = {node_type, NodeType} | nodedown_reason
    NodeType = visible | hidden | all
    Error = error | {error, term()}
    调用过程订阅或取消订阅节点状态更改消息。当新的节点连接时nodeup消息，一个节点断开时nodedown消息被传递到所有订阅的进程
    如果Flag为true，则开始新的订阅。如果Flag为false，则将 停止所有使用相同选项启动的先前订阅。如果两个选项列表包含相同的选项集，则认为它们是相同的。
    从内核版本2.11.4和ERTS版本5.5.4开始，保证以下内容：
        在从远程节点传递通过新建立的连接传递的任何消息之前，先传递nodeup消息。
        直到已传递了来自远程节点的通过连接传递的所有消息后，才会传递nodedown消息。


    从内核2.13版和ERTS 5.7版开始，保证以下内容：
    在erlang：nodes / X结果中出现相应节点后，将传递nodeup消息 。
    在erlang：nodes / X的结果中对应的节点消失之后，将传递nodedown消息 。

    节点状态更改消息的格式取决于 Options。如果Options为 []，这是默认设置，则格式如下：
    {nodeup，Node} | {nodedown，Node} Node= node()
    如果Options不是[]，则格式如下：
    {nodeup，Node，InfoList} | {nodedown，Node，InfoList} Node= node() InfoList = [{Tag，Val}]
    InfoList是一个元组列表。其内容取决于 Options，请参见下文。
    另外，当OptionList == []时，仅监视可见节点，即出现在erlang：nodes / 0结果中的 节点。
    选项可以是以下任意一项：
    {node_type，NodeType} NodeType的有效值：
        visible
        订阅仅针对可见节点的节点状态更改消息。元组{node_type，visible}包含在InfoList中。
        hidden
        订阅仅针对隐藏节点的节点状态更改消息。元组{node_type，hidden}已包含在InfoList中。
        all
        订阅可见和隐藏节点的节点状态更改消息。元组 {node_type，visible | hidden}已包含在 InfoList中。

        nodedown_reason
        元组{nodedown_reason，Reason}包含 在nodedown消息的InfoList中。
        原因可以取决于所使用的分发模块或进程是任何术语，但是对于标准TCP分发模块，可以是以下任意一种：
            connection_setup_failed
                连接设置失败（ 发送nodeup消息后）。
            no_network
                没有可用的网络。
            net_kernel_terminated
                所述net_kernel过程终止。
            shutdown
                未指定的连接关闭。
            connection_closed
                连接已关闭。
            disconnect
                连接已断开连接（从当前节点强制连接）。
            net_tick_timeout
                Net tick time-out.
            send_net_tick_failed
                Failed to send net tick over the connection.
            get_status_failed
                从保持连接的端口检索状态信息失败。

## set_net_ticktime（NetTicktime）-> Res
## set_net_ticktime（NetTicktime，TransitionPeriod）-> Res
    Types
    NetTicktime = integer() >= 1
    TransitionPeriod = integer() >= 0
    Res = 
        unchanged | change_initiated |
        {ongoing_change_to, NewNetTicktime}
    NewNetTicktime = integer() >= 1
    将net_ticktime（请参阅 kernel（6））设置为 NetTicktime秒。 TransitionPeriod默认为60。
    一些定义：
        Minimum transition traffic interval (MTTI)
            minimum(NetTicktime, PreviousNetTicktime)*1000 div 4 milliseconds.
        Transition period
            调用set_net_ticktime / 2之后，要覆盖TransitionPeriod秒的最少连续MTTI的时间（即（（（TransitionPeriod * 1000-1）div MTTI + 1）* MTTI 毫秒）。
    如果 NetTicktime <PreviousNetTicktime，则net_ticktime更改在过渡期结束时进行；否则在开始时。在过渡期间，net_kernel确保至少每MTTI毫秒在所有连接上都有传出流量。
    注意
    所述net_ticktime变化必须在网络中的所有节点（具有相同的上启动NetTicktime任何节点上的任何过渡期结束前）; 否则可能会错误地断开连接。

    返回以下之一：
        unchanged
            net_ticktime已经拥有的价值 NetTicktime和保持不变。
        change_initiated
            net_kernel启动了将net_ticktime更改 为NetTicktime 秒。
        {ongoing_change_to，NewNetTicktime}
            该请求被忽略，因为 net_kernel忙于将net_ticktime更改为 NewNetTicktime秒。

## setopts(Node, Options) -> ok | {error, Reason} | ignored
    Types
    Node = node() | new
    Options = [inet:socket_setopt()]
    Reason = inet:posix() | noconnection
    Set one or more options for distribution sockets。参数节点可以是一个节点名称，也可以是新的原子，以影响所有将来连接的节点的分配套接字。
    如果Node不是连接的节点或new，则返回值与 inet：setopts / 2 或{error，noconnection}相同。
    如果Node是新的，则Options 还将添加到内核配置参数 inet_dist_listen_options 和 inet_dist_connect_options。
    如果本地节点不活动，则返回忽略。

## start([Name]) -> {ok, pid()} | {error, Reason}
## start([Name, NameType]) -> {ok, pid()} | {error, Reason}
## start([Name, NameType, Ticktime]) -> {ok, pid()} | {error, Reason}
    Types
    Name = atom()
    NameType = shortnames | longnames
    Reason = {already_started, pid()} | term()
    通过启动net_kernel和其他必要的过程，将非分布式节点转变为分布式节点。
    请注意，该参数是仅包含一个，两个或三个参数的列表。NAMETYPE默认为longnames 和滚动时间至15000。

## stop() -> ok | {error, Reason}
    Types
    Reason = not_allowed | not_found
    将分布式节点转变为非分布式节点。对于网络中的其他节点，这与发生故障的节点相同。仅当使用start / 1启动网络内核时才可能 ，否则返回{error，not_allowed}。如果本地节点未处于活动状态，则返回 {error，not_found}。    
    
    
    
    
       

    



 


    
    
    