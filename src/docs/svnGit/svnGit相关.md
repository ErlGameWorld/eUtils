# SVN 显示两个版本之间修改过的文件名
    svn diff --summarize -r 2119:head
    svn diff --summarize -r 2119:2120
    
# SVN 获取最新head 的commit id  
    svn info --show-item  revision    
    
# Git 显示两个版本之间修改过的文件名
    git diff 456bcb head --name-only   
    git diff 456bcb 93593a --name-only 

# Git 获取最新head 的commit id   
    git rev-parse HEAD


# SVN A C D M G U R I 的含义 
	A：add，新增
	C：conflict，冲突
	D：delete，删除
	M：modify，本地已经修改
	G：modify and merGed，本地文件修改并且和服务器的进行合并
	U：update，从服务器更新
	R：replace，从服务器替换
	I：ignored，忽略