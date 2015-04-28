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
    error_logger:info_msg("got request ~p~n", [PL]),
    case plist:get("request", PL) of
      <<"monitor">> ->
        U = binary_to_list(plist:get("jobid", PL)),
        error_logger:info_msg("request is to monitor job ~p~n", [U]),
        case catmaster:get_state(U) of
          not_found ->
            error_logger:info_msg("job ~p not found with the catmaster.~n", [U]),
            {reply, {text, <<"{ \"request\" : \"monitor\", \"result\" : \"failed\", \"reason\" : \"Sorry, I cannot find the job you are looking for.\" }">>}, Req, State};
          _S ->
            error_logger:info_msg("job ~p found with catmaster~n", [U]),
            erlang:start_timer(2000, self(), update_status),
            {reply, {text, <<"{ \"request\" : \"monitor\", \"result\" : \"success\" }">>}, Req, U}
        end;
      _ ->
        {reply, {text, <<"{ \"result\" : \"error\", \"reason\" : \"Request not understood by server.\" }">>}, Req, State}
    end
  catch T:E ->
    error_logger:error_msg("websocket_handle threw exception ~p:~p~nat message ~p~nwith stacktrace~p~n", [T,E,Msg,erlang:get_stacktrace()]),
    {reply, {text, <<"{ \"result\" : \"error\", \"reason\" : \"error in handler\" }">>}, Req, State}
  end;
websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.


time_to_str("undefined") ->
    "undefined";
time_to_str(T) ->
    lists:flatten(timelib:to_esmf_str(T)).


websocket_info(update_status, Req, U) ->
  case jobmaster:get_state(U) of
    not_found ->
      error_logger:info_msg("job ~p not found in jobmaster in info~n", [U]),
      case catmaster:get_state(U) of
        not_found ->
          error_logger:info_msg("job ~p not found in catmaster in info~n", [U]),
          {noreply, Req, U};
        PL ->
          error_logger:info_msg("job ~p found in catmaster in info~nwith state~p~n", [U,PL]),
          {reply, {text, list_to_binary(build_payload(PL,U))}, Req, U}
      end;
    PL ->
     error_logger:info_msg("job ~p found in jobmaster in info~nwith state~p~n", [U,PL]),
     {reply, {text, list_to_binary(build_payload(PL,U))}, Req, U}
  end;
websocket_info({state_changed, _}, Req, State) ->
  websocket_info(update_status, Req, State);
websocket_info({timeout, _Ref, update_status}, Req, U) ->
  erlang:start_timer(4000, self(), update_status),
  websocket_info(update_status, Req, U);
websocket_info(Info, Req, State) ->
  error_logger:error_msg("Unprocessed message ~p~n", [Info]),
  {ok, Req, State}.


websocket_terminate(_Reason, _Req, _State) -> ok.


build_payload(PL,U) ->
  S = plist:get(stage, "undefined", PL),
  ST = time_to_str(plist:get(sim_time, "undefined", PL)),
  CT = time_to_str(plist:get(completion_time, "undefined", PL)),
  PD = plist:get(percent_done, "undefined", PL),
  SA = plist:get(sim_acceleration, "undefined", PL),
  Ps = plist:get(products, [], PL),
  Ks = build_kml_names(U,Ps),
  Payload = lists:flatten(io_lib:format("{ \"action\" : \"update_status\", \"sim_time\" : ~p,"
                  " \"sim_accel\" : ~p, \"stage\" : ~p,"
                  " \"completion_time\" : ~p, \"percent_done\" : ~p,", [ST, SA, S, CT, PD]) ++
                  " \"kmls\" : [ " ++ Ks ++ " ] }"),
  io:format("Payload sent out ~p~n", [Payload]),
  Payload.

build_kml_names(_U,[]) -> [];
build_kml_names(U,Ps) ->
  FAs = lists:filter(fun ({Var,_Dom,_Type,_TS}) -> Var == "FIRE_AREA" end, Ps),
  io:format("filtered list is ~p~n", [FAs]),
  Ns = lists:map(fun (P) -> build_kml_name(U,P) end, FAs),
  string:join(Ns, ", ").


build_kml_name(U,{Var,Dom,kml,TS}) ->
  % example filename: F_INT-02-2015-04-25_06:00:00.kmz
  Name = lists:flatten(io_lib:format("~s-~2..0B-~s.kmz", [Var,Dom,TS])),
  lists:flatten(["\"", filename:join([U,TS,Name]), "\""]);
build_kml_name(U,{Var,Dom,contour_kml,TS}) ->
  Name = lists:flatten(io_lib:format("~s-~2..0B-~s.kmz", [Var,Dom,TS])),
  lists:flatten(["\"", filename:join([U,TS,Name]), "\""]).

