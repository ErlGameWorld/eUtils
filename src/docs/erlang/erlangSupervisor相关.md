### 监督原则
    主管负责启动，停止和监视其子进程。主管的基本思想是，必须通过在必要时重新启动子进程来保持其子进程活动。
     主管的孩子被定义为孩子规格列表 。当主管启动时，将根据此列表从左到右依次启动子进程。主管终止时，它首先以相反的启动顺序从右到左终止其子进程。
     
     sup_flags() =
        #{strategy => strategy(),         % optional
        intensity => non_neg_integer(),   % optional
        period => pos_integer()           % optional   
     }  
#### 重启策略
     one_for_one-如果一个子进程终止并要重新启动，则仅影响该子进程。这是默认的重启策略。       
     one_for_all-如果一个子进程终止并要重新启动，则所有其他子进程均终止，然后重新启动所有子进程。
     rest_for_one-如果一个子进程终止并要重新启动，则子进程的“剩余”（即，按照启动顺序终止的子进程之后的子进程）将终止。
                    然后，终止的子进程以及重新启动后的所有子进程。
     
     simple_one_for_one-简化的one_for_one 主管，其中所有子进程都是动态添加的，具有相同进程类型（即，运行相同代码）的实例。    
     功能 delete_child / 2和 restart_child / 2 是无效simple_one_for_one监事和回报 {错误，simple_one_for_one}如果指定的主管使用此重启策略。  
     
     通过将子级的pid（）指定为第二个参数，可以将函数terminate_child / 2用于simple_one_for_one主管下的子级。
     如果改用子规范标识符，则 terminate_child / 2返回 {error，simple_one_for_one}。     
     
     由于simple_one_for_one主管可以有多个子代，因此它会异步关闭所有子代。这意味着孩子们并行进行清理，因此未定义他们停止的顺序    
     
     为了防止主管进入子进程终止和重新启动的无限循环，使用上面映射中的键强度和周期指定的两个整数值来定义最大重新启动强度。
     假设值MAXR为强度 和MAXT为周期，然后，如果超过MAXR 重新启动内发生MAXT秒，监控终止所有的子进程，然后本身。在这种情况下，
     主管本身的终止原因将被关闭。 强度默认为1，期间默认为 5。
     
#### 子进程规范
     child_spec() =
        #{
        id => child_id(),       % mandatory
        start => mfargs(),      % mandatory
        restart => restart(),   % optional
        shutdown => shutdown(), % optional
        type => worker(),       % optional
        modules => modules()    % optional 
     }   
     
     id用于由主管内部标识子规范。该ID关键是强制性的。
        请注意，这个在职业上的标识符被称为“名称”。现在尽可能使用术语“标识符”或“ id”，但为了保持向后兼容性，仍然可以找到“名称”的某些出现，例如在错误消息中。   
     start定义用于启动子进程的函数调用。它必须是用作apply（M，F，A）的模块功能参数元组{M，F，A}。  
        启动函数必须创建并链接到子进程，并且必须返回{ok，Child}或 {ok，Child，Info}，其中Child是子进程的pid，Info是主管忽略的任何术语。 
     restart定义终止的子进程何时必须重新启动。
        一个permanent 的子进程总是会重启。
        一个temporary 的子进程不会再重新启动（甚至当主管的重启策略是rest_for_one或one_for_all和兄弟姐妹的死亡原因临时进程被终止）。
        一个transient的子进程重新启动，只有当它异常终止，即与另一个出口原因，而不是 normal，shutdown，或{shutdown，Term}。
        该restart是可选的。如果未指定，则默认为permanent.。   
     shutdown定义必须终止子进程的方式。
        brutal_kill意味着子进程使用exit（Child，kill）无条件终止   
        整数超时值表示主管通过调用exit（Child，shutdown）告诉子进程终止 ，然后等待退出信号，原因是该子进程退出了shutdown。如果在指定的毫秒数内未收到退出信号，则使用exit（Child，kill）无条件终止子进程 。
        infinity 如果子进程是另一个主管，则必须将关闭时间设置为infinity，以使子树有足够的时间关闭。
        
        对于类型为Supervisor的孩子，将关闭时间设置为无穷大以外的任何时间，都 可能导致比赛状态，在该情况下，所讨论的孩子会取消其自己的孩子的链接，但无法在杀死孩子之前终止他们。
        
        如果子进程是工作进程，也可以将其设置为infinity。
        当子进程为工作进程时，将关闭时间设置为无穷大时要小心 。因为在这种情况下，监视树的终止取决于子进程，所以它必须以安全的方式实现，并且其清除过程必须始终返回。
        
        在shutdown 是可选的。如果未指定，则在子类型为worker的情况下默认为5000，在子类型为supervisor的情况下默认为infinity。
     type specifies if the child process is a supervisor or a worker.
        该type关键是可选的。如果未指定，则默认为worker。.   
     
     modules   在代码替换期间，释放处理程序将使用modules模块来确定哪些进程正在使用某个模块。作为一个经验法则，如果孩子过程是一个 主管，gen_server或，
        gen_statem，这是为与一个元素列表[模块]，其中模块是回调模块。如果子进程是具有动态回调模块集的事件管理器（gen_event），则 必须使用动态值。
        有关发布处理的更多信息，请参见 OTP设计原则中的发布处理。
        该modules的关键是可选的。如果未指定，则默认为[M]，其中M来自孩子的开头{M，F，A}。  
     Internally 主管还跟踪PID的 孩子的孩子的过程中，或者不确定如果没有PID存在。
     
     
#### 函数  
    count_children(SupRef) -> PropListOfCounts
        Types SupRef = sup_ref()
         PropListOfCounts = [Count]
          Count = 
               {specs, ChildSpecCount :: integer() >= 0} |        已死或活着的孩子总数。
               {active, ActiveProcessCount :: integer() >= 0} |   此主管管理的所有正在运行的子进程的计数 
                   对于 simple_one_for_one主管，不会执行任何检查以确保每个子进程仍处于活动状态，尽管除非主管非常重载，否则此处提供的结果可能非常准确。
               {supervisors, ChildSupervisorCount :: integer() >= 0} |   规范列表中标记为child_type =主管的所有子进程的计数 ，无论该子进程是否仍在运行
               {workers, ChildWorkerCount :: integer() >= 0}   -标记为所有儿童的数量 CHILD_TYPE =工人在规范列表中，如果不管孩子进程仍然活着。
    
    delete_child(SupRef, Id) -> Result
        Types SupRef = sup_ref()
        Id = child_id()
        Result = ok | {error, Error} 
        Error = running | restarting | not_found | simple_one_for_one  
        
    告诉主管SupRef删除Id标识的子规范。相应的子进程一定不能运行。使用 terminate_child / 2终止它。 
    如果成功，函数将返回ok。如果存在由ID标识的子规范，但相应的子进程正在运行或将要重新启动，则该函数分别返回{error，running}或 {error，restarting}。如果由ID标识的子规范不存在，则该函数返回{error，not_found}。            
    
    
    get_childspec(SupRef, Id) -> Result
        Types
            SupRef = sup_ref()
            Id = pid() | child_id()
            Result = {ok, child_spec()} | {error, Error}
            Error = not_found
            
        返回由Id在主管SupRef下标识的子项的子项规范图。返回的映射包含所有键，包括必需键和可选键。  
    
    
    restart_child(SupRef, Id) -> Result
        Types
        SupRef = sup_ref()
        Id = child_id()
        Result =
            {ok, Child :: child()} |
            {ok, Child :: child(), Info :: term()} |
            {error, Error}
        Error =
            running | restarting | not_found | simple_one_for_one | term()     
            
        告诉主管SupRef重新启动与Id标识的子规范相对应的子进程。子规范必须存在，并且相应的子进程一定不能运行。
        注意，对于临时子代，子代说明在子代终止时会自动删除；因此，不可能重新启动此类子级。
        如果由ID标识的子规范不存在，则该函数返回{error，not_found}。如果子规范存在但相应的进程已在运行，则该函数返回{error，running}。
        如果子进程启动函数返回{ok，Child} 或{ok，Child，Info}，则将pid添加到主管，并且该函数返回相同的值。
        如果子进程启动函数返回ignore，则pid保持设置为undefined，该函数返回{ok，undefined}。
        如果子进程启动函数返回错误元组或错误值，或者失败，则该函数返回 {error，Error}，其中Error是包含有关错误信息的术语。 
        
    start_child(SupRef, ChildSpec) -> startchild_ret()
    Types
    SupRef = sup_ref()
    ChildSpec = child_spec() | (List :: [term()])
    startchild_ret() =
        {ok, Child :: child()} |
        {ok, Child :: child(), Info :: term()} |
        {error, startchild_err()}
    startchild_err() =
        already_present | {already_started, Child :: child()} | term()
    ChildSpec必须是有效的子规范（除非主管是simple_one_for_one 主管；请参见下文）。通过使用子规范中定义的启动功能来启动子进程。
    对于simple_one_for_one主管，将使用Module：init / 1中定义的子规范，而ChildSpec必须改为是List的任意列表。然后，通过将List附加到现有的启动函数参数（即，通过调用apply（M，F，A ++ List））来 启动子进程，其中{M，F，A}是子规范中定义的启动函数。
    
    如果已经存在带有指定标识符的子规范，则将丢弃ChildSpec，并且该函数将根据相应的子进程是否在运行而返回{error，already_present}或 {error，{already_started，Child}}。
    如果子进程启动函数返回 {ok，Child}或 {ok，Child，Info}，则将子规范和pid添加到主管，并且该函数返回相同的值。
    如果子进程启动函数返回ignore，则将子规范添加到主管（除非该主管是simple_one_for_one主管，请参见下文），将pid设置为undefined，并且该函数返回 {ok，undefined}。
    对于simple_one_for_one主管，当子进程启动函数返回ignore时，该函数将返回 {ok，undefined}，并且没有子级添加到主管。
    如果子进程启动函数返回错误元组或错误值，或者失败，则子规范被丢弃，函数返回{error，Error}，其中 Error是包含有关错误和子规范的信息的术语。    
    
    start_link（模块，Args）-> startlink_ret（）
    start_link（SupName，Module，Args）-> startlink_ret（）
    种类
    SupName = sup_name（）
    模块= module（）
    Args = term（）
    startlink_ret（）=
        {确定，pid（）} | 忽略| {错误，startlink_err（）}
    startlink_err（）=
        {已经开始，pid（）} | {关机，term（）} | 术语（）
    sup_name（）=
        {本地，名称:: atom（）} |
        {global，Name :: atom（）} |
        {via，Module :: module（），Name :: any（）}
    创建一个监督程序，作为监督树的一部分。例如，该功能可确保主管链接到呼叫过程（其主管）。
    
    创建的主管进程将调用 Module：init / 1来查找有关重启策略，最大重启强度和子进程的信息。为了确保同步启动过程，在返回Module：init / 1并启动所有子进程之前，不会返回 start_link / 2,3。
    
    如果SupName = {local，Name}，则主管使用register / 2在本地注册为Name。
    
    如果SupName = {global，Name}，则使用 global：register_name / 2将主管全局注册为Name。
    
    如果 SupName = {via，Module，Name}，则使用Module表示的注册表将主管注册为Name。所述模块的回调必须导出功能REGISTER_NAME / 2， unregister_name / 1，和发送/ 2，它必须表现得像在相应的功能 全球。因此， {via，global，Name}是有效的引用。
    
    如果未提供姓名，则主管未注册。
    
    模块是回调模块的名称。
    
    Args是作为参数传递给Module：init / 1的任何术语。
    
    如果成功创建了主管及其子进程（即，如果所有子进程启动函数都返回 {ok，Child}，{ok，Child，Info}或ignore），则该函数返回{ok，Pid}，其中Pid是主管的pid。
    
    如果已经存在具有指定SupName的进程，则 该函数返回 {error，{already_started，Pid}}，其中Pid是该进程的pid。
    
    如果Module：init / 1返回ignore，则此函数也返回ignore，并且supervisor因normal终止。
    
    如果Module：init / 1失败或返回错误值，则此函数返回{error，Term}，其中 Term是包含有关错误信息的术语，而主管则以Term终止。
    
    如果任何子进程启动功能失败或返回错误的元组或错误值，则主管首先使用原因shutdown终止所有已启动的子进程， 然后终止自身并返回 {error，{shutdown，Reason}}。
    
    
    Terminate_child（SupRef，Id）->结果
    种类
    SupRef = sup_ref（）
    ID = pid（）| child_id（）
    结果=正常| {错误，错误}
    错误=找不到| simple_one_for_one
    告诉主管SupRef终止指定的孩子。
    
    如果主管不是simple_one_for_one，则 ID必须是子规范标识符。该过程（如果有的话）将终止，并且除非它是一个临时子进程，否则该子进程规范将由主管保留。子进程以后可以由主管重新启动。子进程也可以通过调用restart_child / 2显式重启 。使用 delete_child / 2 删除子级规范。
    
    如果子级是临时的，则该子级规范将在过程终止后立即删除。这意味着delete_child / 2没有意义，并且restart_child / 2不能用于这些子级。
    
    如果主管是simple_one_for_one，则 ID 必须是子进程的pid（）。如果指定的进程处于活动状态，但不是指定的主管的子进程，则该函数返回 {error，not_found}。如果指定了子规范标识符而不是pid（），则函数返回{error，simple_one_for_one}。
    
    如果成功，函数将返回ok。如果没有带有指定ID的子规范，则该函数返回{error，not_found}。
    
    有关SupRef的描述，请参见 start_child / 2。  
    
    
    which_children（SupRef）-> [{Id，Child，Type，Modules}]
    种类
    SupRef = sup_ref（）
    ID = child_id（） | 未定义
    子= 子（） | 重新开始
    类型= worker（）
    模块= modules（）
    返回一个新创建的列表，其中包含有关属于主管SupRef的所有子规范和子进程的信息。
    
    请注意，在内存不足的情况下监视多个子项时调用此函数可能会导致内存不足异常。
    
    有关SupRef的描述，请参见 start_child / 2。
    
    为每个子规范/过程提供以下信息：
    
    Id-在子规范中定义，或 为simple_one_for_one主管未定义。
    
    子 -相应子进程的pid，如果该进程将要重新启动，则原子重新启动；如果没有这样的进程，则未定义。
    
    类型 -子规范中定义的类型。
    
    模块 -子规范中定义的模块。  
    
    
    Module：init（Args）->结果
    种类
    Args = term（）
    结果= {确定，{SupFlags，[ChildSpec]}} | 忽视
     SupFlags = sup_flags（）
     ChildSpec = child_spec（）
    每当使用start_link / 2,3启动管理员时 ，新进程就会调用此函数以查找有关重新启动策略，最大重新启动强度和子级规范的信息。
    
    Args是提供给start函数的Args参数。
    
    SupFlags是主管标志，用于定义主管的重新启动策略和最大重新启动强度。[ChildSpec]是有效的子规范的列表，这些规范定义了主管必须启动和监视的子进程。请参阅前面的“ 监督原则”一节中的讨论 。
    
    请注意，当重新启动策略为 simple_one_for_one时，子规范的列表必须是仅包含一个子规范的列表。（忽略子规范标识符。）然后，在初始化阶段不启动任何子进程，但是假定所有子进程都使用start_child / 2动态启动 。
    
    该函数还可以返回ignore。
    
    请注意，此功能也可以作为代码升级过程的一部分来调用。因此，该功能不应有任何副作用。有关管理程序代码升级的更多信息，请参阅《OTP设计原则》中的“ 更改管理程序”部分 。  
             
      
      
      
      
        