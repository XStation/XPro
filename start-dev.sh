#!/bin/sh
ROOT=`pwd`
EBIN=$ROOT"/ebin"
erl -boot start_sasl -sname xpro@localhost -pa $EBIN -pa $ROOT/deps/*/ebin -s xpro_app -s reloader \
	 +K true -P 134217727 -Q 134217727 -env  ERL_MAX_PORTS 409600  -setcookie xpro \
    -eval "io:format(\"* xpros service already started~n~n\")."
