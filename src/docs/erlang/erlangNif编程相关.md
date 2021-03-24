# 描述
    NIF库包含Erlang模块的某些功能的本机实现。像其他任何函数一样，调用本机实现的函数（NIF），与调用方没有任何区别。
    NIF库被构建为动态链接的库文件，并通过调用erlang：load_nif / 2在运行时加载。
    
    警告
    谨慎使用此功能。
    执行本机功能作为VM的本机代码的直接扩展。执行不是在安全的环境中进行的。VM 无法提供与执行Erlang代码时相同的服务，
    例如抢先式调度或内存保护。如果本机功能运行不正常，则整个VM都会出现异常。
    崩溃的本机功能将使整个VM崩溃。
    错误实现的本机功能可能会导致VM内部状态不一致，从而导致VM崩溃或在调用本机功能后的任何时候VM的其他异常行为。
    在返回之前进行长时间工作的本机功能会降低VM的响应能力，并可能导致其他奇怪的行为。这种奇怪的行为包括但不限于极端的内存
    使用情况以及调度程序之间的不良负载平衡。在Erlang / OTP发行版之间，由于冗长的工作而可能发生的奇怪行为也会有所不同。
    
# 简单示例
```
    /* niftest.c */
    #include <erl_nif.h>
    static ERL_NIF_TERM hello(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
    {
        return enif_make_string(env, "Hello world!", ERL_NIF_LATIN1);
    }
    static ErlNifFunc nif_funcs[] =
    {
        {"hello", 0, hello}
    };
    ERL_NIF_INIT(niftest,nif_funcs,NULL,NULL,NULL,NULL)  
```
```
    -module(niftest).
    -export([init/0, hello/0]).
    -on_load(init/0).
    init() ->
          erlang:load_nif("./niftest", 0).
    
    hello() ->
          erlang:nif_error("NIF library not loaded").
```  
     在上面的示例中，使用了on_load指令，该命令功能是在加载模块时自动调用的指定的函数-init/0。
     init函数初始化依次调用erlang：load_nif / 2 ，
     该加载器会加载NIF库，并用C中的本机实现替换hello函数。加载后，NIF库将保持不变。在清除它所属的模块代码版本之前，不会将其卸载。 
     如果在成功加载NIF库之前调用了该函数，则每个NIF必须具有用Erlang调用的实现。典型的此类存根实现是调用erlang：nif_error，
     这将引发异常。如果NIF库缺少某些操作系统或硬件体系结构的实现，则Erlang函数也可以用作后备实现。     
     
     注意
     NIF不必导出，它可以在模块本地。但是，编译器会优化未使用的本地存根函数，从而导致NIF库的加载失败。
     
# 功能性
     NIF代码和Erlang运行时系统之间的所有交互都是通过调用NIF API函数来执行的。存在以下功能的功能：   
     读写Erlang术语
     任何Erlang术语都可以作为函数参数传递给NIF，并作为函数返回值返回。这些术语属于C类型的 ERL_NIF_TERM，只能使用API​​函数读取或写入。大部分用于读取术语内容的函数都以enif_get_为前缀，并且如果该术语属于预期类型（或非预期类型），则通常返回 true（或false）。编写术语的函数都带有enif_make_前缀 ，通常返回创建的ERL_NIF_TERM。还有一些查询术语的函数，例如enif_is_atom，enif_is_identical和enif_compare。
     
     类型的所有方面ERL_NIF_TERM属于类型的环境ErlNifEnv。术语的生存期由其环境对象的生存期控制。读取或写入术语的所有API函数都将术语所属的环境作为第一个函数参数
     
增加和减少资源的引用计数的次数必须匹配，否则可能引发问题。
至此，持久资源的主要接口的实现就介绍完了，用户使用时，可以先通过enif_open_resource_type建立资源类型的描述符，
然后利用此描述符，使用enif_alloc_resource分配资源所占用的内存空间，使用enif_make_resource将资源导出到erlang模块层，
在进程间传递资源描述符，资源再传回NIF时，可以通过enif_get_resource取回资源描述符中的资源数据结构，
同时可以通过enif_keep_resource来共享资源，通过enif_release_resource来放弃使用资源，gc系统也会正确回收引用计数为0的资源，
开发者再也不用担心内存没有被正确释放了。
持久资源为NIF的开发带来了极大的便利，用户可以将一些大规模的数据结构一次传入内存，生成一个资源描述符，
然后在进程间传递资源描述符而不是资源数据本身，减轻每次资源数据拷贝的开销，同时持久资源也是线程安全的，
写erlang程序也可以像写c程序一样高效了。     
     
     