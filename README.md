# eUtils
    An OTP library Otp21.2+
    erlang各种有用的函数包括一些有用nif封装，还有一些性能测试case。
    经过各种测试确保这些函数封装实现方式在erlang中是最优的选择之一！！！
   
# dataType目录
    主要为erlang中类型相关的函数
    
# docs目录
    主要为erlang中疑难或者常用的知识点
    
# dynamicCompile目录
    为erlang动态编译相关的代码
    
# httpSocket目录
    涉及http socket的函数
    
# nifSrc目录    
    各个nif对应的erlang代码
    
# srvNodeMgr目录
    erlang节点管理
    
# stackStrace目录
    erlang堆栈美化函数
    
# testCase目录
    用来测试对比各种函数 各种写法以及erang中各种数据类型效率的函数封装
   
# timeData目录
    获取各种本地时间与世界时间，以及本地时间与世界时间相互转换的时间函数封装    
    C++20 代码逻辑没问题：代码本身是完美的。
    依赖问题：C++20 的时区库强依赖操作系统本地的 /usr/share/zoneinfo 数据。
    运维动作：务必确保你的生产环境 Dockerfile 里有一行 apt-get install tzdata 或 apk add tzdata。只要有了这个包，你的 NIF 就能正常工作。
    
# md5 目录
    封装了Md5UUid的函数    
                           
# comMisc目录
    未分类的模块 
    
# c_src  
    收集部分有用的nif，准备封装一个高效的可多进程共享的hash容器， 封装到一半测试效率并不怎么好，放弃了这个想法，还是用ets吧


