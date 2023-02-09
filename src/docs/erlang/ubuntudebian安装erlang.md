#WSL设置root登录
   C:\Users\用户名\AppData\Local\Microsoft\WindowsApps\debian.exe config --default-user root  重新打开WSL即可看到登录用户为Root


# 下载dep包

# 执行安装
    dpkg -i esl-erlang_21.0-1_ubuntu_artful_amd64.deb

# 按照需求安装依赖包
   apt-get install libwxbase3.0-0v5

   一键安装所有的依赖包(这个命令可以把依赖都解决掉)
   apt-get -f install


# erlang编译安装
        安装 erlang

         apt-get install libncurses5-dev

         aptitude search openssl #查找可安装的OPENSSL
         sudo apt-get install apache2-api-20120211-openssl1.1

         apt-get install libssl-dev

         apt-get install unixodbc unixodbc-dev
         apt-get install unixodbc unixodbc-dev



        ./otp_build autoconf
        运行configure配置
        ./configure --prefix=/usr/local/erlang  --with-ssl  -enable-threads -enable-smmp-support -enable-kernel-poll  --without-javac

        fop is missing可以忽略

        运行make install安装
        8 安装
        8.1 将erlang源码包解压到/root目录下（一定要/root目录, 非/root目录需要配置参数，还没搞明白)
        8.2 运行./configrue
        8.3 运行 make install

        添加环境变量
 

        方法三：修改/etc/profile文件使其永久性生效，并对所有系统用户生效，在文件末尾加上如下两行代码
        PATH=$PATH:/usr/local/erlang/bin

        export PATH

        最后：执行 命令source /etc/profile或 执行点命令 ./profile使其修改生效，执行完可通过echo $PATH命令查看是否添加成功。