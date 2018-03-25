%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc logger_dump
%%% @author biganans
%%% Created 2018/3/25
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-module(logger_dump).
-behaviour(gen_server).
-include("common.hrl").
-export([start_link/0]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-record(state, {fd}).


%%%============================================================================
%%% API functions
%%%============================================================================
start_link() ->
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).


%%%============================================================================
%%% Behaviour functions
%%%============================================================================
-spec init(Args) -> Return when
    Args :: term(),
    Return :: {ok, State :: term()}
        | {ok, State :: term(), timeout() | hibernate}
        | {stop, Reason :: term()}
        | ignore.
init(Args) ->
    try do_init(Args) catch
    _ErrType:_ErrReason ->
        {stop,do_init_error}
    end.


-spec handle_call(Request, From, State) -> Return when
    Request :: term(),
    From :: {pid(), Tag :: term()},
    State :: term(),
    Return :: {reply, Reply :: term(), NewState :: term()}
        | {reply, Reply :: term(), NewState :: term(), timeout() | hibernate}
        | {noreply, NewState :: term()}
        | {noreply, NewState :: term(), timeout() | hibernate}
        | {stop, Reason :: term(), Reply :: term(), NewState :: term()}
        | {stop, Reason :: term(), NewState :: term()}.
handle_call(Request, From, State) ->
    try do_handle_call(Request, From, State) catch
    _ErrType:_ErrReason ->
        {noreply,State}
    end.


-spec handle_cast(Request, State) -> Return when
    Request :: term(),
    State :: term(),
    Return :: {noreply, NewState :: term()}
        | {noreply, NewState :: term(), timeout() | hibernate}
        | {stop, Reason :: term(), NewState :: term()}.
handle_cast(Request, State) ->
    try do_handle_cast(Request, State) catch
    _ErrType:_ErrReason ->
        {noreply,State}
    end.


-spec handle_info(Info, State) -> Return when
    Info :: term(),
    State :: term(),
    Return :: {noreply, NewState :: term()}
        | {noreply, NewState :: term(), timeout() | hibernate}
        | {stop, Reason :: term(), NewState :: term()}.
handle_info(Info, State) ->
    try do_handle_info(Info, State) catch
    _ErrType:_ErrReason ->
        {noreply,State}
    end.


-spec terminate(Reason, State) -> Return when
    Reason :: normal | shutdown | {shutdown, term()} | term(),
    State :: term(),
    Return :: term().
terminate(Reason, State) ->
    try do_terminate(Reason, State) catch
    _ErrType:_ErrReason ->
        do_terminate_error
    end.


-spec code_change(OldVsn, State, Extra) -> Return when
    OldVsn :: term() | {down, term()},
    State :: term(),
    Extra :: term(),
    Return :: {ok, NewState :: term()} | {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok,State}.


%%%============================================================================
%%% Internal functions
%%%============================================================================
do_init(_Args) ->
    File = make_log_file(),
    trunk_at_next_day(),
    {ok,#state{fd=File}}.


do_handle_call(_Request, _From, State) ->
    {noreply,State}.


do_handle_cast(_Request, State) ->
    {noreply,State}.


do_handle_info({event,Event}, State) ->
    write_event(State#state.fd, {erlang:localtime(),Event}),
    {noreply,State};
do_handle_info(trunk_file, State) ->
    State2 = State#state{fd=make_log_file()},
    trunk_at_next_day(),
    {noreply,State2};
do_handle_info(_Msg, State) ->
    {noreply,State}.


do_terminate(_Reason, State) ->
    {stop,normal,State}.


log_time() ->
    {{Y,Mo,D},{H,Mi,S}} = erlang:localtime(), 
    io_lib:format("~w/~w/~w ~w:~w:~w",
          [Mo, D, Y, H, Mi, S]).

do_write(Fd, Time, Type, Format, Args) ->
    {{Y,Mo,D},{H,Mi,S}} = Time, 
    Time2 = io_lib:format("== ~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w ===",
          [Y, Mo, D, H, Mi, S]),
    write_file(Fd, [Type, "\n", Time2]),
    try 
        M = io_lib:format(Format, Args),
        write_file(Fd, [M, "</FONT></div>"])
    catch _:Error ->
            ?ERR("log error ~p ~p ~p", [Error, Format, Args])
    end.

write_event(Fd, {Time, {error, _GL, {_Pid, Format, Args}}}) ->
    do_write(Fd, Time, "<div><font size=\"2\" color=\"#FF0000\">", Format, Args);

write_event(Fd, {Time, {warning, _GL, {_Pid, Format, Args}}}) ->
    do_write(Fd, Time, "<div><font size=\"2\" color=\"#FFCC33\">", Format, Args);

write_event(Fd, {Time, {debug, _GL, {_Pid, Format, Args}}}) ->
    do_write(Fd, Time, "<div><font size=\"2\" color=\"#00FFD2\">", Format, Args);

write_event(Fd, {Time, {info_msg, _GL, {_Pid, Format, Args}}}) ->
    do_write(Fd, Time, "<div><font size=\"2\" color=\"#00FF00\">", Format, Args);

write_event(Fd, {Time, {emulator, _GL, Chars}}) ->
    T = write_time(Time),
    case catch io_lib:format(Chars, []) of
    S when is_list(S) ->
        write_file(Fd, [T, S]);
    _ ->
        write_file(Fd, [T, io_lib:format("ERROR: ~p ~n", [Chars])])
    end;


write_event(Fd, {Time, {info, _GL, {Pid, Info, _}}}) ->
    T = write_time(Time),
    write_file(Fd, [T , io_lib:format(add_node("~p~n",Pid), [Info])]);


write_event(Fd, {Time, {error_report, _GL, {Pid, std_error, Rep}}}) ->
    T = write_time(Time),
    S = format_report(Rep),
    write_file(Fd, [T , S ++ add_node("", Pid)]);


write_event(Fd, {Time, {info_report, _GL, {Pid, std_info, Rep}}}) ->
    T = write_time(Time, "INFO REPORT"),
    S = format_report(Rep),
    write_file(Fd, [T , S , add_node("", Pid) ]);





write_event(_, _) ->
    ok.

format_report(Rep) when is_list(Rep) ->
    case string_p(Rep) of
    true ->
        io_lib:format("~s~n",[Rep]);
    _ ->
        format_rep(Rep)
    end;
format_report(Rep) ->
    io_lib:format("~p~n",[Rep]).

format_rep([{Tag,Data}|Rep]) ->
    io_lib:format("    ~p: ~p~n",[Tag,Data]) ++ format_rep(Rep);


format_rep([Other|Rep]) ->
    io_lib:format("    ~p~n",[Other]) ++ format_rep(Rep);


format_rep(_) ->
    [].

add_node(X, Pid) when node(Pid) /= node() ->
    lists:concat([X,"** at node ",node(Pid)," **~n"]);
add_node(X, _) ->
    X.

string_p([]) ->
    false;
string_p(Term) ->
    string_p1(Term).

string_p1([H|T]) when is_integer(H), H >= $\s, H < 255 ->
    string_p1(T);
string_p1([$\n|T]) -> string_p1(T);
string_p1([$\r|T]) -> string_p1(T);
string_p1([$\t|T]) -> string_p1(T);
string_p1([$\v|T]) -> string_p1(T);
string_p1([$\b|T]) -> string_p1(T);
string_p1([$\f|T]) -> string_p1(T);
string_p1([$\e|T]) -> string_p1(T);
string_p1([H|T]) when is_list(H) ->
    case string_p1(H) of
    true -> string_p1(T);
    _    -> false
    end;
string_p1([]) -> true;
string_p1(_) ->  false.

write_time(Time) -> write_time(Time, "ERROR REPORT").

write_time({{Y,Mo,D},{H,Mi,S}}, Type) ->
    io_lib:format("~n=~s==== ~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w ===",
          [Type, Y, Mo, D, H, Mi, S]).


%%生成日志文件名
make_log_file() ->
    {{Year, Month, Day}, {_Hour, _, _}} = erlang:localtime(),
    filename:join([get_log_dir(), io_lib:format("error_log_~p_~p_~p.html", [Year, Month, Day]) ]).

get_log_dir() ->
    try
        ConfigFName = filename:join([?ROOT_DIR,"config","data_common.config"]),
        {ok,KVList} = file:consult(ConfigFName),
        {_,LogDir1} = lists:keyfind(log_dir, 1, KVList),
        LogDir1
    catch _:_ ->
        case application:get_env(log_dir) of
        {ok,LogDir2} -> LogDir2;
        _ -> "./log"
        end
    end.

%%通知服务器在下一个整点刷新日志文件
trunk_at_next_day() ->
    {_, {H, M, S}} = erlang:localtime(),
    Time = ((23 - H) * 3600 + (59 - M) * 60 + (59 - S) + 2) * 1000,
    erlang:send_after(Time, self(), trunk_file).            

%% 封装write_file的参数
write_file(Fd, IoList) ->
    check_head(Fd),
    file:write_file(Fd, IoList, [append, delayed_write]).

check_head(File) ->
    case filelib:file_size(File) of
        0 ->
            write_head(File);
        _ ->
            ignore
    end.

write_head(File) ->
    Head = io_lib:format("<BODY bgcolor=\"#000000\">\n<pre>
<font color=\"#00FF00\">
<style type=\"text/css\">
    div {text-align:left;margin-left:4px;word-wrap:break-word;}
</style>
<CENTER><meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\" /> <TITLE>Log</TITLE> <H3><font color=\"#FFFF97\">Log for ~s</font><br><br></CENTER>
<hr>",[log_time()]),
    file:write_file(File, Head, [append, delayed_write]).

