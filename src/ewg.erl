%% @author author <matteo.redaelli AT libero.it>
%% @copyright 2009(c) Matteo Redaelli.

%% @doc TEMPLATE.

-module(ewg).
-author('author <matteo.redaelli AT libero.it>').
-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    ewg_deps:ensure(),
    ensure_started(crypto),
    ensure_started(webmachine),
    ewg_sup:start_link().

%% @spec start() -> ok
%% @doc Start the ewg server.
start() ->
    ewg_deps:ensure(),
    ensure_started(crypto),
    application:start(ewg).

%% @spec stop() -> ok
%% @doc Stop the ewg server.
stop() ->
    Res = application:stop(ewg),
    application:stop(crypto),
    Res.
