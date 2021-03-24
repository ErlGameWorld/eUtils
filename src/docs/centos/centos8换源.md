# CentOS 8 换源，设置dnf / yum镜像
    aliyun更新了centos8的说明
    
    1、备份
    mv /etc/yum.repos.d/CentOS-Base.repo /etc/yum.repos.d/CentOS-Base.repo.backup
    
    2、下载新的CentOS-Base.repo 到/etc/yum.repos.d/
    curl -o /etc/yum.repos.d/CentOS-Base.repo http://mirrors.aliyun.com/repo/Centos-8.repo
    
    3、生成缓存
    yum makecache
    
    
    4 配置镜像的。
    
    cd /etc/yum.repos.d
    #备份
    cp CentOS-Base.repo CentOS-Base.repo.bak;
    cp CentOS-AppStream.repo CentOS-AppStream.repo.bak;
    cp CentOS-Extras.repo CentOS-Extras.repo.bak;
    
    sed -i 's/mirrorlist=/#mirrorlist=/g' CentOS-Base.repo CentOS-AppStream.repo CentOS-Extras.repo;
    sed -i 's/#baseurl=/baseurl=/g' CentOS-Base.repo CentOS-AppStream.repo CentOS-Extras.repo;
    sed -i 's/http:\/\/mirror.centos.org/https:\/\/mirrors.aliyun.com/g' CentOS-Base.repo CentOS-AppStream.repo CentOS-Extras.repo
    
    