%%%-------------------------------------------------------------------
%%% File    : wg_generator.erl
%%% Author  : Matteo Redaelli <matteo.redaelli@libero.it>
%%% Description : 
%%% License: GPL V3
%%%
%%% Created :  3 Aug 2009 by Matteo Redaelli <matteo.redaelli@libero.it>
%%%-------------------------------------------------------------------
-module(wg_generator).

-behaviour(gen_server).

-include("wg.hrl").

-define(SERVER, ?MODULE).

%% API
-export([start_link/0, generate_words/1, statistics/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {count=0}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

generate_words(Word) ->
    gen_server:cast(?MODULE, {generate_words, Word}).

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

handle_call({statistics}, _From, State) ->
    Reply = "Generator: count=" ++ 
	integer_to_list(State#state.count),
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
handle_cast({generate_words, Word}, State) ->
    NewState = State#state{count = State#state.count + 1},
    case wg_validator:is_valid(Word) of
 	true ->
	    wg_dumper:dump_valid_word(Word);
	false -> 
	    io:fwrite("No valid word: Skipping '~s'~n", [Word])
    end,
    case wg_validator:is_candidate(Word) of
	true ->
	    lists:foreach( fun(X) -> spawn(?MODULE, generate_words, [[X] ++ Word]) end, ?CHARACTERS );
	false -> 
	    io:fwrite("No candidate word: Skipping '~s'~n", [Word])
    end,
    {noreply, NewState};

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
