# log
    由于部分nif依赖于c99编译,而部分环境gcc版本低于依赖导致编译不过，特此记录Gcc升级命令
    
# 步骤
    1. 安装centos-release-scl
        sudo yum install centos-release-scl
    2. 查看最新的版本并安装
        yum list | grep devtoolset 然后选择你想要安装的版本，比如：
        sudo yum install devtoolset-9-gcc*
    3. 激活对应的devtoolset，所以你可以一次安装多个版本的devtoolset，需要的时候用下面这条命令切换到对应的版本
        scl enable devtoolset-9 bash
        gcc -v
    
        注意：：：这条激活命令只对本次会话有效，重启会话后还是会变回原来的版本 想随意切换可按如下操作。
        首先，安装的devtoolset是在 /opt/sh 目录下的，如图
        
        每个版本的目录下面都有个 enable 文件，如果需要启用某个版本，只需要执行
        source ./enable
        所以要想切换到某个版本，只需要执行
        source /opt/rh/devtoolset-8/enable
        可以将对应版本的切换命令写个shell文件放在配了环境变量的目录下，需要时随时切换，或者开机自启  
              
    4. 直接替换旧的gcc
        旧的gcc是运行的 /usr/bin/gcc，所以将该目录下的gcc/g++替换为刚安装的新版本gcc软连接，免得每次enable
        复制代码
        mv /usr/bin/gcc /usr/bin/gcc-4.8.5
        ln -s /opt/rh/devtoolset-8/root/bin/gcc /usr/bin/gcc
        mv /usr/bin/g++ /usr/bin/g++-4.8.5
        ln -s /opt/rh/devtoolset-8/root/bin/g++ /usr/bin/g++
        gcc --version
        g++ --version 
        
            
            