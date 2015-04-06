#!/usr/bin/env bash
HOST=`hostname -s`
erl -sname wrfx2web_cons -remsh wrfx2web@$HOST
