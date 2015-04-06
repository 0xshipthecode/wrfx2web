-module(ctrl_ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
  erlang:start_timer(1000, self(), <<"{ \"action\": \"display\", \"message\" : \"Hello from wrfx2 web server.\\n\\nUSAGE\\nClick anywhere on map to place ignition point. Coordinates will appear on control panel. Adjust parameters as desired and submit to start a fire simulation. Note the ignition time is fixed in the prototype. After submission, you will be redirected to a page that will render the results of the simulation.\\n\\n\" }">>),
  erlang:start_timer(1000, self(), update_state),
  {ok, Req, undefined_state}.


websocket_handle({text, Msg}, Req, State) ->
  try
    {ok, {obj, PL}, []} = rfc4627:decode(Msg),
    case plist:get("request", undefined, PL) of
      <<"submit">> ->
        Lat = plist:get("lat", PL),
        Lon = plist:get("lon", PL),
        FC = plist:get("fc_len", PL),
        case jobmaster:submit('nasa-fire-job', [{'ign-specs', [{{Lat, Lon}, {1*1800, 500}} ]}, {'sim-from', {{2012, 9, 9}, {0,0,0}}},
                                                   {'num-nodes', 2}, {ppn, 12}, {'wall-time-hrs', 4}, {'forecast-length-hrs', FC}]) of
          {ok, U} -> 
            Repl = io_lib:format("{ \"result\" : \"success\", \"action\" : \"submit\", \"jobid\": ~p }", [U]),
            {reply, {text, list_to_binary(Repl)}, Req, State};
          _ ->
            {reply, {text, <<"{\"result\" : \"failure\", \"reason\" : \"reply from wrfx2 not understood\"}">>}, Req, State}
        end;
      _ ->
        {reply, {text, <<"{ \"result\"  \"error\", \"reason\" : \"invalid command\" }">>}, Req, State}
    end
  catch Cls:Err ->
    io:format("websocket_handle: caught error ~p:~p~n", [Cls,Err]),
    {reply, {text, <<"{ \"result\" : \"error\", \"reason\" : \"invalid json\" }">>}, Req, State}
  end;
websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.


websocket_info({timeout, _Ref, update_state}, Req, State) ->
  erlang:start_timer(2000, self(), update_state),
  try
    S = sysmon:getstate(),
    NSA = length(jobmaster:livejobs()),
    H = plist:get(host, S),
    TN = plist:get(nodes, "unknown", S),
    FN = plist:get(freenodes, "unknown", S),
    QL = plist:get(qlen, "unknown", S),
    LU = lists:flatten('timelib':'to-esmf-str'(plist:get(lastupdated, S))),
    Payload = io_lib:format("{ \"action\" : \"state_update\", \"system\" : ~p, \"nodes\" : ~p, \"freenodes\" : ~p,"
                            " \"numsims\" : 0, \"activesims\" : ~p, \"qlen\" : ~p, \"lastupdated\" : ~p }", [H, TN, FN, NSA, QL, LU]),
    {reply, {text, list_to_binary(Payload)}, Req, State}
  catch T:E ->
    io:format("websocket_info: caught exc ~p:~p with backtrace ~p.~n", [T,E,erlang:get_stacktrace()]),
    {ok, Req, State}
  end;
websocket_info({timeout, _Ref, Msg}, Req, State) ->
  {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
  {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.
