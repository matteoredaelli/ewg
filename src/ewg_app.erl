%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the ewg application.

-module(ewg_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for ewg.
start(_Type, _StartArgs) ->
    ewg_deps:ensure(),
    ewg_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for ewg.
stop(_State) ->
    ok.
