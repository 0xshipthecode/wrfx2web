-module(wrfx2web).

-export([start/0]).

start() ->
  connect_to_wrfx2_node(),
 
  ok = application:start(crypto),
  ok = application:start(cowlib),
  ok = application:start(ranch),
  ok = application:start(cowboy),
  ok = application:start(mnesia),
  ok = application:start(wrfx2web).


connect_to_wrfx2_node() ->
  {ok,H} = inet:gethostname(),
  % this is the only place, where the list_to_atom is used in wrfx2web
  Node = list_to_atom("wrfx2@" ++ H),
  net_adm:ping(Node).
