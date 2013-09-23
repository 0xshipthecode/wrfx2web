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
  erlang:start_timer(1000, self(), <<"{ \"action\": \"display\", \"message\" : \"Hello from wrfx2 web frontend. USAGE\\nClick anywhere on map to place ignition point. Coordinates will appear on control panel. Adjust parameters as desired and submit to start a fire simulation. Note the ignition time is fixed in the prototype. After submission, you will be redirected to a page that will render the results of the simulation.\\n\\n\" }">>),
  erlang:start_timer(1000, self(), update_state),
  {ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
  try
    {ok, {obj, PL}, []} = rfc4627:decode(Msg),
    case plist:get("request", undefined, PL) of
      <<"submit">> ->
        Lat = plist:get("lat", PL),
        Lon = plist:get("lon", PL),
        IT = {{2013, 9, 1}, {4, 0, 0}},
        _FC = plist:get("fc_len", PL),
        case jobmaster:submitjob('fire-sim', [{'ign-when', IT}, {'ign-where', {Lat, Lon}}, {'num-nodes', 12}, {ppn, 12}, {'wall-time-hrs', 4}]) of
          {ok, U, _} -> 
            Repl = io_lib:format("{ \"result\" : \"success\", \"action\" : \"submit\", \"jobid\": ~p }", [U]),
            {reply, {text, list_to_binary(Repl)}, Req, State};
          busy ->
            {reply, {text, <<"{\"result\" : \"failure\", \"reason\" : \"Another job is running at this time.\"}">>}, Req, State}
        end;
      _ ->
        {reply, {text, <<"{ \"result\"  \"error\", \"reason\" : \"invalid command\" }">>}, Req, State}
    end
  catch _:_ ->
    {reply, {text, <<"{ \"result\" : \"error\", \"reason\" : \"invalid json\" }">>}, Req, State}
  end;
websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

websocket_info({timeout, _Ref, update_state}, Req, State) ->
  erlang:start_timer(2000, self(), update_state),
  try
    S = sysmon:getstate(),
    NST = length(jobmaster:listjobs()),
    NSA = length(jobmaster:activejobs()),
    H = plist:get(host, S),
    TN = plist:get(nodes, "unknown", S),
    FN = plist:get(freenodes, "unknown", S),
    QL = plist:get(qlen, "unknown", S),
    LU = lists:flatten('time-arith':'to-esmf-str'(plist:get(lastupdated, S))),
    Payload = io_lib:format("{ \"action\" : \"state_update\", \"system\" : ~p, \"nodes\" : ~p, \"freenodes\" : ~p,"
                            " \"numsims\" : ~p, \"activesims\" : ~p, \"qlen\" : ~p, \"lastupdated\" : ~p }", [H, TN, FN, NST, NSA, QL, LU]),
    {reply, {text, list_to_binary(Payload)}, Req, State}
  catch _T:_E ->
    {ok, Req, State}
  end;
websocket_info({timeout, _Ref, Msg}, Req, State) ->
  {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
  {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.
