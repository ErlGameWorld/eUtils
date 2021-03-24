# window下编译nif dll  需要安装vs

## 第一种 makefile配置(可参考jiffy的Makefile) 命令行下编译dll 需要设置vs相关环境变量
具体要设置的环境变量可参考下面几个
```
path 新增
	D:\Program Files (x86)\Microsoft Visual Studio\2019\Enterprise\VC\Tools\MSVC\14.24.28314\bin\Hostx64\x64

    %% E:\Program Files (x86)\Microsoft Visual Studio\2019\Professional\VC\Tools\MSVC\14.27.29110\bin\Hostx64\x64
	
LIB
	D:\Windows Kits\10\Lib\10.0.18362.0\ucrt\x64
	D:\Windows Kits\10\Lib\10.0.18362.0\um\x64
	D:\Program Files (x86)\Microsoft Visual Studio\2019\Enterprise\VC\Tools\MSVC\14.24.28314\lib\x64

    %% E:\Windows Kits\10\Lib\10.0.18362.0\ucrt\x64
    %% E:\Windows Kits\10\Lib\10.0.18362.0\um\x64
    %% E:\Program Files (x86)\Microsoft Visual Studio\2019\Professional\VC\Tools\MSVC\14.27.29110\lib\x64

INCLUDE
	D:\Windows Kits\10\Include\10.0.18362.0\ucrt
	D:\Program Files (x86)\Microsoft Visual Studio\2019\Enterprise\VC\Tools\MSVC\14.24.28314\include

    %% E:\Windows Kits\10\Include\10.0.18362.0\ucrt
    %% E:\Program Files (x86)\Microsoft Visual Studio\2019\Professional\VC\Tools\MSVC\14.27.29110\include
```

### 脚本配置
    VsDevCmd.bat 文件设置适当的环境变量来生成命令行。
     备注
        Visual Studio 2015 及更早版本基于相同目的使用 VSVARS32.bat，而不是 VsDevCmd.bat。
        此文件保存在 
        Program Files\Microsoft Visual Studio\Version \Common7\Tools 或
        Program Files (x86)\Microsoft Visual Studio\Version \Common7\Tools。
	
## 第二种 在vs单独编译 然后拷贝使用	
    VS编译 
    	1 新建空项目或者从现有代码创建项目
    	2 先选择 编辑框上边的  解决方案配置 与 解决方案平台
    	3 右键项目属性 设置 配置与第2步 解决方案配置 一样 设置 平台与第二步设置的 解决方案平台 一样
    	4 右键项目属性 配置属性 -> 常规 -> 配置类型 ->动态库(.dll)
    	5 右键项目属性 配置属性 -> VC++目录 -> 包含目录 新增 D:\Program Files\erl10.6\erts-10.6\include
    	6 右键项目属性 生成 
    	注意编译使用的erlang include要合使用的erl版本对应