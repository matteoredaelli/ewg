-module(ewg).
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
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
