#!/bin/sh
ROOT=`pwd`
EBIN=$ROOT"/ebin"
erl -boot start_sasl -sname xpro@localhost -pa $EBIN -pa $ROOT/deps/*/ebin -s xpro_app -s reloader \
	 +K true -P 134217727 -Q 134217727 -env  ERL_MAX_PORTS 409600  -env -env ERL_MAX_ETS_TABLES 409600 -setcookie xpro \
	 -config /opt/apps/xpro/_rel/xpro_release/releases/1/sys.config \
     -eval "io:format(\"* xpros service already started~n~n\")."
