#!/usr/bin/env bash
HOST=`hostname -s`
erl -detached -sname wrfx2web@$HOST -pa ebin deps/*/ebin ../wrfx2/ebin/ -mnesia dir '"../wrfx2/db"' -s wrfx2web

