-module(wrfx2web).

-export([start/0]).

start() ->
  ok = application:start(crypto),
  ok = application:start(cowlib),
  ok = application:start(ranch),
  ok = application:start(cowboy),
  ok = application:start(wrfx2web).

