XPro
====

A Niubility Information Exchange System



Developer
========

可以使用make help 查看帮助

make list-templates //可以查看产生哪些开发模板

make new t=TPL n=NAME //产生出模板, 文件会自动建立在src下.


Ps:
现在引入了jsx来解析json, jsx默认使用rebar, 但却不能找到项目根下的rebar
所在在自己的环境上需要把rebar的路径export到$PATH中去.
