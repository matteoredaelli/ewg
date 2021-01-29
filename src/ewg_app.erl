%%%-------------------------------------------------------------------
%% @doc ewg public API
%% @end
%%%-------------------------------------------------------------------

-module(ewg_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ewg_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
