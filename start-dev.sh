#!/bin/sh
ROOT=`pwd`
EBIN=$ROOT"/ebin"
erl -boot start_sasl -sname xpro@localhost -pa $EBIN -pa $ROOT"/deps/*/ebin" -s xpro_app -s reloader +K true -setcookie xpro \
    -eval "io:format(\"* xpros service already started~n~n\")."
