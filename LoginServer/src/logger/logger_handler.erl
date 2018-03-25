%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc logger_handler
%%% @author biganans
%%% Created 2018/3/25
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-module(logger_handler).
-behaviour(gen_event).
-export([init/1,handle_event/2,handle_call/2,handle_info/2,terminate/2,code_change/3]).
-record(state, {}).


%%%============================================================================
%%% API functions
%%%============================================================================


%%%============================================================================
%%% Behaviour functions
%%%============================================================================
init(_Args) ->
    {ok,#state{}}.


handle_event(Event, State) ->
    case whereis(logger_dump) of
    undefined -> ignore;
    Pid -> Pid ! {event,Event}
    end,
    {ok,State}.


handle_call(_Request, State) ->
    {ok,{error,bad_query},State}.


handle_info(_Info, State) ->
    {ok,State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok,State}.


%%%============================================================================
%%% Internal functions
%%%============================================================================

