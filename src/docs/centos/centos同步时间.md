# 安装ntpdate工具
	yum -y install ntp ntpdate
# 设置系统时间与网络时间同步
	 ntpdate 0.asia.pool.ntp.org
	这里主要就是通过时间服务器对系统时间进行同步，所以0.asia.pool.ntp.org并不是固定的，大家可以选择time.nist.gov、time.nuri.net、0.asia.pool.ntp.org、1.asia.pool.ntp.org、2.asia.pool.ntp.org、3.asia.pool.ntp.org中任意一个，只要保证可用就OK
	
# 将系统时间写入硬件时间
	hwclock --systohc	
	这里是为了防止系统重启后时间被还原，因此需要写到硬件设备中去。