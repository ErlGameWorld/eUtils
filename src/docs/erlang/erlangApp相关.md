文件命名为
	application_Name.app

格式如下：
{application,"app名字",  
	[  
		{description,"app描述"},  
		{vsn ,"版本号"},  
		{id ,Id},%%app id 同 erl -id ID  
		{modules,[Modules]},%%app包含的模块，systools模块使用它来生成script、tar文件  
		{maxP,Num},%%进程最大值  
		{maxT,Time},%%app运行时间 单位毫秒  
		{registered,[mod]},%%指定app 名字模块，systools用来解决名字冲突  
		{included_applictions ,[XX]},%%指定子 app，只加载，但是不启动  
		{applictions,[xxxx]},%%启动自己的app前，appliation:ensure_all_started将会首先启动此列表的app application:start会检查该列表是否都启动 
		{env,[xxxx]},%%配置app的env，可以使用application:get_env(AppName, Key)获取  
		{mod,{xxx,args}},%%指定app启动模块，参数，对应自己app的application behavior  
		{start_phases,[{xxx,xxx}]]%%指定启动阶段一些操作，对应otp application  start_phase函数  
	]  
}  

必须要配置的为description，vsn，modules，registered，applications。  
Application为应用名，
descripttion为应用的简单描述
id 产品标识
vsn 应用版本
modules 应用所涉及到的module
registered 注册进程
applications 本应用启动时需要事先启动的其他应用

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
erlang官网说明

{application, Application,
  [{description,  Description},
   {id,           Id},
   {vsn,          Vsn},
   {modules,      Modules},			本应用程序引入的所有模块。systools 在生成启动脚本和tar文件时使用这个列表。一个模块只能在一个应用程序中定义
   {maxP,         MaxP},			已经弃用了
   {maxT,         MaxT},			时间单位为毫秒
   {registered,   Names},			注册过程的所有名称均在此应用程序中启动。systools使用这个列表来检测不同应用程序之间的名字冲突
   {included_applications, Apps},	此应用程序包含的所有应用程序 当此应用程序启动时，应用程序控制器会自动加载所有包含的应用程序，但不会启动。假设包含应用程序的最高管理者由本应用程序的主管启动
   {applications, Apps},			允许启动此应用程序之前必须启动的所有应用程序。systools使用这个列表来生成正确的启动脚本。缺省为空列表，但请注意所有应用程序对（至少）Kernel和STDLIB都有依赖关系
   {env,          Env},				应用程序使用的配置参数。通过调用application：get_env / 1,2来检索配置参数的值
   {mod,          Start},			指定应用程序回调模块和启动参数
   对于作为监督树实施的应用程序，密钥mod是必需的，否则应用程序控制器不知道如何启动它。 对于没有进程的应用程序（通常是代码库，例如STDLIB），可以省略mod
   {start_phases, Phases},
   {runtime_dependencies, RTDeps}]}. 应用程序依赖的应用程序版本列表

             Value                Default
             -----                -------
Application  atom()               -
Description  string()             ""
Id           string()             ""
Vsn          string()             ""
Modules      [Module]             []
MaxP         int()                infinity
MaxT         int()                infinity
Names        [Name]               []
Apps         [App]                []
Env          [{Par,Val}]          []
Start        {Module,StartArgs}   []
Phases       [{Phase,PhaseArgs}]  undefined
RTDeps       [ApplicationVersion] []

Module = Name = App = Par = Phase = atom()
Val = StartArgs = PhaseArgs = term()
ApplicationVersion = string()

如果要使用systools中的函数 需要设置下面的key参数
	description vsn modules registered applications
	其他的key被systools忽略

应用的策略
 application:start(Name, Type)
type:
• permanent: if the app terminates, the entire system is taken down, excluding manual termination of the app with application:stop/1.
• transient: if the app terminates for reason normal, that’s ok. Any other reason for termination shuts down the entire system.
• temporary: the application is allowed to stop for any reason. It will be reported, but nothing bad will happen.

	

	
	
	
	
	
	