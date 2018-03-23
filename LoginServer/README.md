# LoginServer
erlang帐号服务器
# 设计要点
使用http模式建立玩家账号数据库，并生成一个唯一的UUID即代表玩家的ROLEID，到GameServer上建立唯一的玩家数据
# 文件夹层级说明
-- config (配置文件)   
--|-- app (声明的app)   
-- deps (依赖第三方库，别人写的)   
-- doc (设计文档，怕自己忘记的)  
-- include (头文件源码)  
-- script (数据库脚本等)  
-- src (主要的源码逻辑都在这里了)  
-- ebin (编译出来的erlang，发布使用)  
-- log (打印日志位置)  

