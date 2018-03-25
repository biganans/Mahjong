%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc logger_app
%%% @author biganans
%%% Created 2018/3/25
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-module(logger_app).
-behaviour(application).
-include("common.hrl").
-export([start/2,stop/1]).


%%%============================================================================
%%% API functions
%%%============================================================================


%%%============================================================================
%%% Behaviour functions
%%%============================================================================
-spec start(Type, StartArgs) -> Return when
    Type :: application:start_type(),
    StartArgs :: term(),
    Return :: {ok, Pid :: pid()}
        | {ok, Pid :: pid(), State :: term()}
        | {error, Reason :: term()}.
start(_Type, _StartArgs) ->
    [gen_event:delete_handler(error_logger, H, [])
        || H <- gen_event:which_handlers(error_logger)],
    gen_event:add_handler(error_logger, logger_handler, []),
    logger_gen:set(get_log_level()),
    {ok,SupPid} = logger_sup:start_link(),
    {ok,SupPid}.


-spec stop(State) -> Return when
    State :: term(),
    Return :: term().
stop(_State) ->
    ok.


%%%============================================================================
%%% Internal functions
%%%============================================================================
get_log_level() ->
    try
        ConfigFName = filename:join([?ROOT_DIR,"config","data_common.config"]),
        {ok,KVList} = file:consult(ConfigFName),
        {_,LogLevel1} = lists:keyfind(log_level, 1, KVList),
        LogLevel1
    catch _:_ ->
        case application:get_env(log_level) of
        {ok,LogLevel2} -> LogLevel2;
        _ -> 2
        end
    end.

