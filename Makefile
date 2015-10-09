PROJECT = xpro
DEPS = cowboy lager jsx poolboy riakpbclient
dep_cowboy = git https://github.com/ninenines/cowboy.git 1.0.1
dep_lager = git https://github.com/basho/lager.git 2.1.0
dep_jsx = git https://github.com/talentdeficit/jsx.git 2.3.1
dep_poolboy = git https://github.com/devinus/poolboy.git 1.5.1
dep_riakpbclient = git https://github.com/basho/riak-erlang-client.git 2.1.1
include erlang.mk
