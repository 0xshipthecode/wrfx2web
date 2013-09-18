%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(wrfx2web_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.
start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
                        {"/pages/[...]", cowboy_static, [
                                {directory, {priv_dir, wrfx2web, [<<"pages">>]}},
				%{file, <<"index.html">>},
				{mimetypes, [{<<".html">>, [<<"text/html">>]}]}
			]},
			{"/websocket/control", ctrl_ws_handler, []},
                        {"/websocket/monitor", mon_ws_handler, []},
			{"/resources/[...]", cowboy_static, [
				{directory, {priv_dir, wrfx2web, [<<"resources">>]}},
                                {mimetypes, [{<<".js">>, [<<"application/javascript">>]},
                                             {<<".css">>, [<<"text/css">>]},
                                             {<<".png">>, [<<"image/png">>]}]}
			]}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
		[{env, [{dispatch, Dispatch}]}]),
	wrfx2web_sup:start_link().

stop(_State) ->
	ok.
