%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(ewg_resource).
-export([
	 content_types_provided/2,
	 init/1, 
	 to_text/2
	]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

content_types_provided(ReqData, State) ->
   {[{"text/plain",to_text}], ReqData, State}.

to_text(ReqData, State) ->
%    Path = wrq:disp_path(ReqData),
%    Body = io_lib:format("Hello ~s from webmachine.~n", [Path]),
    Params = wrq:path_info(ReqData),
    case dict:find(action, Params) of
	{ok, "generate_words"} ->
	    wg_generator:generate_words(""),
	    Result = "Started";
	_Else ->
	    Result = "Unknown action"
    end,
    {Result, ReqData, State}.
