%%%-------------------------------------------------------------------
%%% File    : wg_validator.erl
%%% Author  : Matteo Redaelli <matteo.redaelli@libero.it>
%%% Description : 
%%% License: GPL V3
%%% 
%%% Created :  3 Aug 2009 by Matteo Redaelli <matteo.redaelli@libero.it>
%%%-------------------------------------------------------------------
-module(wg_validator).

-behaviour(gen_server).

-include("ewg.hrl").

-define(SERVER, ?MODULE).

%% API
-export([start_link/0, is_candidate/1, is_valid/1, statistics/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {candidate=0, valid=0}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

is_candidate(Word) ->
    gen_server:call(?MODULE, {is_candidate, Word}).

is_valid(Word) ->
    gen_server:call(?MODULE, {is_valid, Word}).

statistics() ->
    gen_server:call(?MODULE, {statistics}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({is_candidate, Word}, _From, State) ->
    Reply = is_valid_max_length(Word) andalso
	is_valid_max_char_occurs(Word) andalso
	is_valid_max_consecutive_char_occurs(Word),
    NewState = 
	case Reply of
	    true ->
		State#state{candidate = State#state.candidate + 1};
	    false ->
		State
	end,
    {reply, Reply, NewState};

handle_call({is_valid, Word}, _From, State) ->
    Reply = is_valid_min_length(Word) andalso
	is_valid_max_length(Word) andalso
	is_valid_regexps(Word) andalso
	is_valid_min_char_occurs(Word) andalso
	is_valid_max_char_occurs(Word) andalso
	is_valid_max_consecutive_char_occurs(Word),
    NewState = 
	case Reply of
	    true ->
		State#state{valid = State#state.valid + 1};
	    false ->
		State
	end,
    {reply, Reply, NewState};

handle_call({statistics}, _From, State) ->
    Reply = "Validator: candidate=" ++ 
	integer_to_list(State#state.candidate) ++
	", valid=" ++ 
	integer_to_list(State#state.valid),
    NewState = State,
    {reply, Reply, NewState};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

is_valid_min_length(Word) ->
    {ok, Value} = application:get_env(min_length),
    length(Word) >=  Value.

is_valid_max_length(Word) ->
    {ok, Value} = application:get_env(max_length),
    length(Word) =<  Value.

%% ------------------------------------------
%% is_valid_min_char_occurs
%% ------------------------------------------
is_valid_min_char_occurs(Word) ->
    {ok, Value} = application:get_env(char_occurs),
    lists:all(
      fun({String, Min, _Max}) ->
	      is_valid_min_char_occurs(String, Min, Word)
      end, 
      Value
     ).

is_valid_min_char_occurs(String, Min, Word) ->
    lists:all(
      fun(Char) ->
	      N = get_occurrences(Char, Word),
	      N >= Min
      end,
      String).

%% ------------------------------------------
%% is_valid_max_char_occurs
%% ------------------------------------------

is_valid_max_char_occurs(Word) ->
    {ok, Value} = application:get_env(char_occurs),
    lists:all( 
      fun({String, _Min, Max}) ->
	      is_valid_max_char_occurs(String, Max, Word)
      end, 
      Value
     ).

is_valid_max_char_occurs(String, Max, Word) ->
    lists:all(
      fun(Char) ->
	      N = get_occurrences(Char, Word),
	      N =< Max
      end,
      String
     ).

%% ------------------------------------------
%% is_valid_max_consecutive_char_occurs
%% ------------------------------------------

is_valid_max_consecutive_char_occurs(Word) ->
    {ok, Value} = application:get_env(max_consecutive_char_occurs),
    lists:all( 
      fun({String, Max}) ->
	      is_valid_max_consecutive_char_occurs(String, Max, Word)
      end, 
      Value). 
   
is_valid_max_consecutive_char_occurs(String, Max, Word) ->
    lists:all( 
      fun(Char) ->
	      Cons = string:chars(Char, Max + 1),
	      case string:str(Word, Cons) of
		  1 -> false;
		  0 -> true
	      end
      end,
      String
     ).

%% ------------------------------------------
%% is_valid_regexps
%% ------------------------------------------

is_valid_regexps(Word) ->
    {ok, Regexps} = application:get_env(regexps),
    lists:all(
      fun({Match, RE}) ->
	      Match == re:run(Word, RE, [{capture, none}])		  
      end,
      Regexps).

get_occurrences(Char, String ) ->
    S = [C || C <- String, C == Char],
    length(S).
