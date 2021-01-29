%%%-------------------------------------------------------------------
%% @doc ewg top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ewg_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    WgDumper = ?CHILD(wg_dumper, worker),
    WgGenerator = ?CHILD(wg_generator, worker),
    WgValidator = ?CHILD(wg_validator, worker),
    SupFlags = #{strategy => one_for_one,
		 intensity => 5,
		 period => 10},
    ChildSpecs = [WgDumper,
		  WgGenerator,
		  WgValidator],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
