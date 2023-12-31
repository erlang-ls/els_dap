-module(els_dap_config).

%% API
-export([
    do_initialize/4,
    initialize/3,
    get/1,
    set/2,
    start_link/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_dap.hrl").
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% Macros
%%==============================================================================
-define(DEFAULT_CONFIG_FILE, "erlang_ls.config").
-define(ALTERNATIVE_CONFIG_FILE, "erlang_ls.yaml").
-define(SERVER, ?MODULE).

%% TODO: Refine names to avoid confusion
-type key() ::
    apps_dirs
    | capabilities
    | config_path
    | deps_dirs
    | include_dirs
    | otp_path
    | plt_path
    | root_uri
    | code_reload
    | elvis_config_path
    | compiler_telemetry_enabled
    | refactorerl
    | edoc_custom_tags
    | providers.

-type path() :: file:filename().
-type state() :: #{
    apps_dirs => [path()],
    deps_dirs => [path()],
    include_dirs => [path()],
    otp_path => path(),
    plt_path => path(),
    root_uri => uri(),
    code_reload => map() | 'disabled',
    compiler_telemetry_enabled => boolean(),
    refactorerl => map() | 'notconfigured',
    providers => map()
}.

%%==============================================================================
%% Exported functions
%%==============================================================================

-spec initialize(uri(), map(), map()) -> ok.
initialize(RootUri, Capabilities, InitOptions) ->
    RootPath = els_dap_utils:to_list(els_dap_uri:path(RootUri)),
    ConfigPaths = config_paths(RootPath, InitOptions),
    {GlobalConfigPath, MaybeGlobalConfig} = find_config(global_config_paths()),
    {LocalConfigPath, MaybeLocalConfig} = find_config(ConfigPaths),
    ConfigPath =
        case LocalConfigPath of
            undefined ->
                report_missing_config(),
                GlobalConfigPath;
            _ ->
                LocalConfigPath
        end,
    Config =
        case {MaybeGlobalConfig, MaybeLocalConfig} of
            {{ok, GlobalConfig}, {ok, LocalConfig}} ->
                %% Augment LocalConfig onto GlobalConfig, note that this
                %% is not a deep merge of nested maps.
                maps:merge(GlobalConfig, LocalConfig);
            {{error, Reason}, _} ->
                %% We should not continue if the config is broken, but would
                %% need a bigger initialization refactor to make that work.
                report_broken_config(GlobalConfigPath, Reason),
                #{};
            {_, {error, Reason}} ->
                report_broken_config(LocalConfigPath, Reason),
                #{}
        end,
    do_initialize(RootUri, Capabilities, InitOptions, {ConfigPath, Config}).

-spec do_initialize(uri(), map(), map(), {undefined | path(), map()}) -> ok.
do_initialize(RootUri, Capabilities, _InitOptions, {ConfigPath, Config}) ->
    RootPath = els_dap_utils:to_list(els_dap_uri:path(RootUri)),
    OtpPath = maps:get("otp_path", Config, code:root_dir()),
    ?LOG_INFO("OTP Path: ~p", [OtpPath]),
    DepsDirs = maps:get("deps_dirs", Config, []),
    AppsDirs = maps:get("apps_dirs", Config, ["."]),
    IncludeDirs = maps:get("include_dirs", Config, ["include"]),
    ExcludeUnusedIncludes = maps:get("exclude_unused_includes", Config, []),
    Macros = maps:get("macros", Config, []),
    DialyzerPltPath = maps:get("plt_path", Config, undefined),
    CodeReload = maps:get("code_reload", Config, disabled),
    Runtime = maps:get("runtime", Config, #{}),
    CodePathExtraDirs = maps:get("code_path_extra_dirs", Config, []),
    ok = add_code_paths(CodePathExtraDirs, RootPath),
    ElvisConfigPath = maps:get("elvis_config_path", Config, undefined),
    IncrementalSync = maps:get("incremental_sync", Config, true),
    CompilerTelemetryEnabled =
        maps:get("compiler_telemetry_enabled", Config, false),
    EDocCustomTags = maps:get("edoc_custom_tags", Config, []),
    RefactorErl = maps:get("refactorerl", Config, notconfigured),
    Providers = maps:get("providers", Config, #{}),
    %% Passed by the LSP client
    ok = set(root_uri, RootUri),
    %% Read from the configuration file
    ok = set(config_path, ConfigPath),
    ok = set(otp_path, OtpPath),
    ok = set(deps_dirs, DepsDirs),
    ok = set(apps_dirs, AppsDirs),
    ok = set(include_dirs, IncludeDirs),
    ok = set(exclude_unused_includes, ExcludeUnusedIncludes),
    ok = set(macros, Macros),
    ok = set(plt_path, DialyzerPltPath),
    ok = set(code_reload, CodeReload),
    ok = set(providers, Providers),
    ?LOG_INFO("Config=~p", [Config]),
    ok = set(
        runtime,
        maps:merge(
            els_dap_config_runtime:default_config(),
            Runtime
        )
    ),
    ok = set(elvis_config_path, ElvisConfigPath),
    ok = set(compiler_telemetry_enabled, CompilerTelemetryEnabled),
    ok = set(edoc_custom_tags, EDocCustomTags),
    ok = set(incremental_sync, IncrementalSync),
    %% All (including subdirs) paths used to search files with file:path_open/3
    %% Init Options
    ok = set(capabilities, Capabilities),
    ok = set(refactorerl, RefactorErl),
    ok.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

-spec get(key()) -> any().
get(Key) ->
    gen_server:call(?SERVER, {get, Key}).

-spec set(key(), any()) -> ok.
set(Key, Value) ->
    gen_server:call(?SERVER, {set, Key, Value}).

%%==============================================================================
%% gen_server Callback Functions
%%==============================================================================

-spec init({}) -> {ok, state()}.
init({}) ->
    {ok, #{}}.

-spec handle_call(any(), any(), state()) ->
    {reply, any(), state()}.
handle_call({get, Key}, _From, State) ->
    Value = maps:get(Key, State, undefined),
    {reply, Value, State};
handle_call({set, Key, Value}, _From, State0) ->
    State = maps:put(Key, Value, State0),
    {reply, ok, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) -> {noreply, State}.

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec config_paths(path(), map()) -> [path()].
config_paths(
    RootPath,
    #{<<"erlang">> := #{<<"config_path">> := ConfigPath0}}
) ->
    ConfigPath = els_dap_utils:to_list(ConfigPath0),
    lists:append([
        possible_config_paths(ConfigPath),
        possible_config_paths(filename:join([RootPath, ConfigPath])),
        default_config_paths(RootPath)
    ]);
config_paths(RootPath, _Config) ->
    default_config_paths(RootPath).

-spec default_config_paths(path()) -> [path()].
default_config_paths(RootPath) ->
    [
        filename:join([RootPath, ?DEFAULT_CONFIG_FILE]),
        filename:join([RootPath, ?ALTERNATIVE_CONFIG_FILE])
    ].

-spec global_config_paths() -> [path()].
global_config_paths() ->
    GlobalConfigDir = filename:basedir(user_config, "erlang_ls"),
    [
        filename:join([GlobalConfigDir, ?DEFAULT_CONFIG_FILE]),
        filename:join([GlobalConfigDir, ?ALTERNATIVE_CONFIG_FILE])
    ].

%% @doc Bare `Path' as well as with default config file name suffix.
-spec possible_config_paths(path()) -> [path()].
possible_config_paths(Path) ->
    [
        Path,
        filename:join([Path, ?DEFAULT_CONFIG_FILE]),
        filename:join([Path, ?ALTERNATIVE_CONFIG_FILE])
    ].

-spec find_config([path()]) -> {FoundPath, OkConfig | Error} when
    FoundPath :: path() | undefined,
    OkConfig :: {ok, map()},
    Error :: {error, term()}.
find_config(Paths) ->
    case lists:dropwhile(fun(P) -> not filelib:is_regular(P) end, Paths) of
        [FoundPath | _] ->
            {FoundPath, consult_config(FoundPath)};
        _ ->
            {undefined, {ok, #{}}}
    end.

-spec consult_config(path()) -> {ok, map()} | {error, term()}.
consult_config(Path) ->
    case file:consult(Path) of
        {ok, [Config]} when is_map(Config) ->
            {ok, Config};
        Error ->
            {error, {invalid_config, Error}}
    end.

-spec report_missing_config() -> ok.
report_missing_config() ->
    ?LOG_INFO(
        "The current project is missing an erlang_ls.config file. "
        "Need help configuring Erlang LS for your project? "
        "Visit: https://erlang-ls.github.io/configuration/"
    ).

-spec report_broken_config(
    path(),
    Reason :: term()
) -> ok.
report_broken_config(Path, Reason) ->
    ?LOG_ERROR(
        "The erlang_ls.config file at ~s can't be read (~p) "
        "Need help configuring Erlang LS for your project? "
        "Visit: https://erlang-ls.github.io/configuration/",
        [Path, Reason]
    ).

-spec add_code_paths(
    Dirs :: list(string()),
    RooDir :: string()
) ->
    ok.
add_code_paths(WCDirs, RootDir) ->
    AddADir = fun(ADir) ->
        ?LOG_INFO("Adding code path: ~p", [ADir]),
        true = code:add_path(ADir)
    end,
    AllNames = lists:foldl(
        fun(Elem, AccIn) ->
            AccIn ++ filelib:wildcard(Elem, RootDir)
        end,
        [],
        WCDirs
    ),
    Dirs = [
        [$/ | safe_relative_path(Dir, RootDir)]
     || Name <- AllNames,
        filelib:is_dir([$/ | Dir] = filename:absname(Name, RootDir))
    ],
    lists:foreach(AddADir, Dirs).

-if(?OTP_RELEASE >= 23).
-spec safe_relative_path(
    Dir :: file:name_all(),
    RootDir :: file:name_all()
) ->
    Path :: file:name_all().
safe_relative_path(Dir, RootDir) ->
    filelib:safe_relative_path(Dir, RootDir).
-else.
-spec safe_relative_path(
    FileName :: file:name_all(),
    RootDir :: file:name_all()
) ->
    Path :: file:name_all().
safe_relative_path(Dir, _) ->
    filename:safe_relative_path(Dir).
-endif.
