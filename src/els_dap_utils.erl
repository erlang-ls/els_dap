-module(els_dap_utils).

-export([
    cmd/2,
    cmd/3,
    halt/1,
    to_binary/1,
    to_list/1,
    compose_node_name/2,
    is_windows/0
]).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% File and module functions
%%==============================================================================

-spec cmd(string(), [string()]) -> integer() | no_return().
cmd(Cmd, Args) ->
    cmd(Cmd, Args, []).

% @doc Replacement for os:cmd that allows for spaces in args and paths
-spec cmd(string(), [string()], string()) -> integer() | no_return().
cmd(Cmd, Args, Path) ->
    ?LOG_INFO("Running OS command [command=~p] [args=~p]", [Cmd, Args]),
    Executable =
        case filename:basename(Cmd) of
            Cmd ->
                cmd_path(Cmd);
            _ ->
                %% The command already contains a path
                Cmd
        end,
    Tag = make_ref(),
    F =
        fun() ->
            P = open_port(
                {spawn_executable, Executable},
                [
                    binary,
                    use_stdio,
                    stream,
                    exit_status,
                    hide,
                    {args, Args},
                    %% TODO: Windows-friendly version?
                    {env, [{"PATH", Path ++ ":" ++ os:getenv("PATH")}]}
                ]
            ),
            exit({Tag, cmd_receive(P)})
        end,
    {Pid, Ref} = erlang:spawn_monitor(F),
    receive
        {'DOWN', Ref, process, Pid, {Tag, Data}} ->
            Data;
        {'DOWN', Ref, process, Pid, Reason} ->
            exit(Reason)
    end.

%% @doc Return the path for a command
-spec cmd_path(string()) -> string().
cmd_path(Cmd) ->
    ErtsBinDir = filename:dirname(escript:script_name()),
    case os:find_executable(Cmd, ErtsBinDir) of
        false ->
            case os:find_executable(Cmd) of
                false ->
                    Fmt = "Could not find command ~p",
                    Args = [Cmd],
                    Msg = lists:flatten(io_lib:format(Fmt, Args)),
                    error(Msg);
                GlobalEpmd ->
                    GlobalEpmd
            end;
        Epmd ->
            Epmd
    end.

-spec halt(non_neg_integer()) -> ok.
halt(ExitCode) ->
    ok = init:stop(ExitCode).

-spec to_binary(unicode:chardata()) -> binary().
to_binary(X) when is_binary(X) ->
    X;
to_binary(X) when is_list(X) ->
    case unicode:characters_to_binary(X) of
        Result when is_binary(Result) -> Result;
        _ -> iolist_to_binary(X)
    end.

-spec to_list(unicode:chardata()) -> string().
to_list(X) when is_list(X) ->
    X;
to_list(X) when is_binary(X) ->
    case unicode:characters_to_list(X) of
        Result when is_list(Result) -> Result;
        _ -> binary_to_list(X)
    end.

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec cmd_receive(port()) -> integer().
cmd_receive(Port) ->
    receive
        {Port, {exit_status, ExitCode}} ->
            ExitCode;
        {Port, _} ->
            cmd_receive(Port)
    end.

-spec compose_node_name(Name :: string(), Type :: shortnames | longnames) ->
    NodeName :: atom().
compose_node_name(Name, Type) ->
    NodeName =
        case lists:member($@, Name) of
            true ->
                Name;
            _ ->
                HostName = els_dap_config_runtime:get_hostname(),
                Name ++ [$@ | HostName]
        end,
    case Type of
        shortnames ->
            list_to_atom(NodeName);
        longnames ->
            Domain = els_dap_config_runtime:get_domain(),
            case Domain of
                "" -> list_to_atom(NodeName);
                _ -> list_to_atom(NodeName ++ "." ++ Domain)
            end
    end.

-spec is_windows() -> boolean().
is_windows() ->
    {OS, _} = os:type(),
    OS =:= win32.
