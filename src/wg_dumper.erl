%%%-------------------------------------------------------------------
%%% File    : wg_dumper.erl
%%% Author  : Matteo Redaelli <matteo.redaelli@libero.it>
%%% Description : 
%%% License: GPL V3
%%%
%%% Created :  3 Aug 2009 by Matteo Redaelli <matteo.redaelli@libero.it>
%%%-------------------------------------------------------------------
-module(wg_dumper).

-behaviour(gen_server).

-include("ewg.hrl").

-define(SERVER, ?MODULE).

%% API
-export([start_link/0, dump_valid_word/1, options/0, statistics/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {config, count=0}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

dump_valid_word(Word) ->
    gen_server:call(?MODULE, {dump_valid_word, Word}).

options() ->
    gen_server:call(?MODULE, {options}).

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
    % TODO: deleting WORDS_FILE if it exists. 
    {ok, Words_file} = application:get_env(words_file),
    file:delete(Words_file),
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
handle_call({dump_valid_word, Word}, _From, State) ->
    {ok, Postfix} = application:get_env(postfix),
    {ok, Prefix} = application:get_env(prefix),
    {ok, Words_file} = application:get_env(words_file),
    NewState = State#state{count = State#state.count + 1},
    {ok, WriteDescr} = file:open(Words_file, [raw, append]), 
    file:write(WriteDescr, Prefix ++ Word ++ Postfix ++ "\n"), 
    file:close(WriteDescr),
    Reply = ok,
    {reply, Reply, NewState};

handle_call({options}, _From, State) ->
    {ok, Characters} = application:get_env(postfix),
    io:fwrite("Characters: '~s'~n", [Characters]),
    Reply = ok,
    {reply, Reply, State};

handle_call({statistics}, _From, State) ->
    Reply = "Dumper: count=" ++ 
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
