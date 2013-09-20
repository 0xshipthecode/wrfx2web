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
            {reply, {text, <<"{ \"request\" : \"monitor\", \"result\" : \"success\" }">>}, Req, JSP}
         end;
      _ ->
        {reply, {text, <<"{ \"result\" : \"error\", \"reason\" : \"Request not understood by server.\" }">>}, Req, State}
    end
  catch _ ->
    {reply, {text, <<"{ \"result\" : \"error\", \"reason\" : \"error in handler\" }">>}, Req, State}
  end;
websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

websocket_info({timeout, _Ref, update_status}, Req, JSP) ->
  PL = jobstate:'get-plist'(JSP),
  S = plist:get(stage, PL),
  case S of
    "done" -> ok;
    _ -> erlang:start_timer(2000, self(), update_status)
  end,
  ST = lists:flatten('time-arith':'to-esmf-str'(plist:get('sim-time', PL))),
  CT = lists:flatten('time-arith':'to-esmf-str'(plist:get('completion-time', PL))),
  Ks = plist:get(kmls, PL),
  SA = plist:get('sim-acceleration', PL),
  Payload = io_lib:format("{ \"action\" : \"update_status\", \"sim_time\" : ~p, \"completion_time\" : ~p,", [ST, CT]) ++
            " \"kmls\" : [ " ++ string:join(lists:map(fun(X) -> "\"" ++ X ++ "\"" end, Ks), ", ") ++ " ] }",
  io:format("Sending out update ~p~n", [Payload]),
  {reply, {text, list_to_binary(Payload)}, Req, JSP};
websocket_info(_Info, Req, State) ->
  {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.
