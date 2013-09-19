-module(mon_ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
  {ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
   try
    _R = rfc4627:decode(Msg),
    {reply, {text, <<"{ \"message\" : \"job_status\", \"compltime_s\" : 520, \"jobid\": \"25b55327-2c43-4d62-beb9-a314ccf91c9f\" }">>}, Req, State}
  catch _ ->
    {reply, {text, <<"{ \"result\" : \"error\", \"reason\" : \"invalid json\" }">>}, Req, State}
  end;
websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
  {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
  {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.
