%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc logger_sup
%%% @author biganans
%%% Created 2018/3/25
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-module(logger_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).


%%%============================================================================
%%% API functions
%%%============================================================================
start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).


%%%============================================================================
%%% Behaviour functions
%%%============================================================================
% some type defination in supervisor.erl
% -type child()    :: 'undefined' | pid().
% -type child_id() :: term().
% -type mfargs()   :: {M :: module(), F :: atom(), A :: [term()] | undefined}.
% -type modules()  :: [module()] | 'dynamic'.
% -type restart()  :: 'permanent' | 'transient' | 'temporary'.
% -type shutdown() :: 'brutal_kill' | timeout().
% -type worker()   :: 'worker' | 'supervisor'.
% -type sup_name() :: {'local', Name :: atom()}
%                   | {'global', Name :: atom()}
%                   | {'via', Module :: module(), Name :: any()}.
% -type sup_ref()  :: (Name :: atom())
%                   | {Name :: atom(), Node :: node()}
%                   | {'global', Name :: atom()}
%                   | {'via', Module :: module(), Name :: any()}
%                   | pid().
% -type child_spec() :: {Id :: child_id(),
%                        StartFunc :: mfargs(),
%                        Restart :: restart(),
%                        Shutdown :: shutdown(),
%                        Type :: worker(),
%                        Modules :: modules()}.

% -type strategy() :: 'one_for_all' | 'one_for_one'
%                   | 'rest_for_one' | 'simple_one_for_one'.
% one_for_one 假如一个进程终止了，仅仅这个进程会被重启
% one_for_all 假如一个进程停止了，所有其他子进程也要被停止，然后所有子进程，包括这个引发停止的子进程都被重启
% {one_for_one,3,10}表达的语义是{How, Max, Within}:
% 在多长时间内(Within)重启了几次(Max),如何重启(HOW 重启策略);
% 设计最大重启频率是为了避免反复重启进入死循环,一旦超出了此阈值,
% supervisor进程会结束掉自己以及它所有的子进程,并通过进程树传递退出消息,更上层的supervisor就会采取适当的措施,
% 要么重启终止的supervisor要么自己也终止掉.可能比较纠结这几个值怎么配置,多数资料上都会告诉你"如何配置完全取决于你的应用程序".
% 这个还是有经验值的,生成环境的经验值是一小时内重启4次,也可以参考一些和你应用类似的开源项目看看它们是怎么配置的.
% 如果填写的是{one_for_one,0,1}就是不允许重启.
-spec init(Args) -> Return when
    Args :: term(),
    Return :: {ok, {{RestartStrategy, MaxR, MaxT}, [ChildSpec]}} | ignore,
    RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(),
    MaxT :: non_neg_integer(),
    ChildSpec :: supervisor:child_spec().
init([]) ->
    AChild = {logger_dump,{logger_dump,start_link,[]}
             ,permanent,5000,worker,[logger_dump]},
    {ok,{{one_for_one,10,10},[AChild]}}.


%%%============================================================================
%%% Internal functions
%%%============================================================================

