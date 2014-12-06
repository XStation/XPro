#!/bin/sh
erl -boot start_sasl -sname xpro@localhost -pa ebin -pa deps/*/ebin -s xpro -s reloader +K true -setcookie xpro \
    -eval "io:format(\"* xpros service already started~n~n\")."
