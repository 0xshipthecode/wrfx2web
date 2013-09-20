-module(wrfx2web).

-export([start/0]).

start() ->
  net_adm:ping(wrfx2@gross),
  ok = application:start(crypto),
  ok = application:start(cowlib),
  ok = application:start(ranch),
  ok = application:start(cowboy),
  ok = application:start(wrfx2web).

