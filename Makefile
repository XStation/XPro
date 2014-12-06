PROJECT = xpro
DEPS = cowboy lager
dep_cowboy = git https://github.com/ninenines/cowboy.git 1.0.1
dep_lager = git https://github.com/basho/lager.git 2.1.0
include erlang.mk
