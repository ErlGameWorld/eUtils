# 安装
    yum install openssl
    yum install openssh-server

# ssh 生成密钥
    ssh-keygen -q -t rsa -b 2048 -f /etc/ssh/ssh_host_rsa_key -N '' ; 
    ssh-keygen -q -t ecdsa -f /etc/ssh/ssh_host_ecdsa_key -N '';
    ssh-keygen -t dsa -f /etc/ssh/ssh_host_ed25519_key -N '';
    
    8、配置ssh无密码登录
    先退出 ssh客户端
    ssh-keygen -t rsa
    cd ~/.ssh
    cat id_rsa.pub >> authorized_keys