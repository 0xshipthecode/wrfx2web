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
    {ok, {obj, PL}, []} = rfc4627:decode(Msg),
    case plist:get("request", PL) of
      <<"monitor">> ->
        J = binary_to_list(plist:get("jobid", PL)),
        case jobmaster:getjobstate(J) of
          'no-such-job' ->
            {reply, {text, <<"{ \"request\" : \"monitor\", \"result\" : \"failed\", \"reason\" : \"Sorry, I cannot find the job you are looking for.\" }">>}, Req, State};
          JSP ->
            erlang:start_timer(2000, self(), update_status),
            %jobstate:subscribe(JSP),
            {reply, {text, <<"{ \"request\" : \"monitor\", \"result\" : \"success\" }">>}, Req, JSP}
         end;
      _ ->
        {reply, {text, <<"{ \"result\" : \"error\", \"reason\" : \"Request not understood by server.\" }">>}, Req, State}
    end
  catch _:_ ->
    {reply, {text, <<"{ \"result\" : \"error\", \"reason\" : \"error in handler\" }">>}, Req, State}
  end;
websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.


time_to_str("undefined") ->
    "undefined";
time_to_str(T) ->
    lists:flatten('time-arith':'to-esmf-str'(T)).


websocket_info(update_status, Req, JSP) ->
  PL = jobstate:'get-plist'(JSP),
  S = plist:get(stage, "undefined", PL),
  ST = time_to_str(plist:get('sim-time', "undefined", PL)),
  CT = time_to_str(plist:get('completion-time', "undefined", PL)),
  PD = plist:get('percent-done', "undefined", PL),
  SA = plist:get('sim-acceleration', "undefined", PL),
  Ks = plist:get(kmls, [], PL),
  Payload = lists:flatten(io_lib:format("{ \"action\" : \"update_status\", \"sim_time\" : ~p,"
                          " \"sim_accel\" : ~p, \"stage\" : ~p,"
                          " \"completion_time\" : ~p, \"percent_done\" : ~p,", [ST, SA, S, CT, PD]) ++
            " \"kmls\" : [ " ++ string:join(lists:map(fun(X) -> "\"" ++ X ++ "\"" end, Ks), ", ") ++ " ] }"),
  {reply, {text, list_to_binary(Payload)}, Req, JSP};
websocket_info({'state-changed', _}, Req, State) ->
  websocket_info(update_status, Req, State);
websocket_info({timeout, _Ref, update_status}, Req, JSP) ->
  erlang:start_timer(4000, self(), update_status),
  websocket_info(update_status, Req, JSP);
websocket_info(Info, Req, State) ->
  io:format("Unprocessed message ~p~n", [Info]),
  {ok, Req, State}.


websocket_terminate(_Reason, _Req, State) ->
  case State of
    undefined_state -> ok;
    _JSP ->
      %jobstate:unsubscribe(JSP)
      ok
  end.

