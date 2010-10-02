-module(ewg).

-export([start/0, stop/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(ewg).

stop() ->
    application:stop(ewg).

-ifdef(TEST).

simple_test() ->
    ok = application:start(ewg),
%    wg_generator:generate_words(""),
    ?assertNot(undefined == whereis(ewg_sup)).

-endif.
