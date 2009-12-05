%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(ewg).
-author('author <author@example.com>').
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
    ensure_started(webmachine),
    application:start(ewg).

%% @spec stop() -> ok
%% @doc Stop the ewg server.
stop() ->
    Res = application:stop(ewg),
    application:stop(webmachine),
    application:stop(crypto),
    Res.
