-ifndef(__ELS_DAP_HRL__).
-define(__ELS_DAP_HRL__, 1).

-define(APP, els_dap).

-type uri() :: binary().
-type line() :: number().
-type column() :: number().
-type position() :: #{
    line := line(),
    character := column()
}.
-type range() :: #{
    start := position(),
    'end' := position()
}.

%% Defined by JSON RPC
-define(ERR_PARSE_ERROR, -32700).
-define(ERR_INVALID_REQUEST, -32600).
-define(ERR_METHOD_NOT_FOUND, -32601).
-define(ERR_INVALID_PARAMS, -32602).
-define(ERR_INTERNAL_ERROR, -32603).
-define(ERR_SERVER_ERROR_START, -32099).
-define(ERR_SERVER_ERROR_END, -32000).
-define(ERR_SERVER_NOT_INITIALIZED, -32002).
-define(ERR_UNKNOWN_ERROR_CODE, -32001).

%% Defined by the protocol
-define(ERR_REQUEST_CANCELLED, -32800).

-endif.
