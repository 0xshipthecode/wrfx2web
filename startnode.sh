#!/usr/bin/env bash
erl -sname wrfx2web -pa ebin deps/*/ebin ../wrfx2/ebin/ -s wrfx2web

