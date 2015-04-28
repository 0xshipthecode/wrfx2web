#!/usr/bin/env bash
HOST=`hostname -s`
erl \
 -boot start_sasl
 -sasl sasl_error_logger '{file, "log/system-sasl.log"}' \
 -kernel error_logger '{file, "log/system.log"}' \
 -detached \
 -sname wrfx2web@$HOST \
 -pa ebin deps/*/ebin ../wrfx2/ebin/ \
 -s wrfx2web

