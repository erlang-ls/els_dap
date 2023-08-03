-module(els_dap_test_utils).

-export([
    all/1,
    all/2,
    wait_for/2,
    wait_for_fun/3,
         wait_until_mock_called/2
]).

-include_lib("common_test/include/ct.hrl").

%%==============================================================================
%% Defines
%%==============================================================================
-define(TEST_APP, <<"code_navigation">>).

%%==============================================================================
%% API
%%==============================================================================

-spec all(module()) -> [atom()].
all(Module) -> all(Module, []).

-spec all(module(), [atom()]) -> [atom()].
all(Module, Functions) ->
    ExcludedFuns = [init_per_suite, end_per_suite, all, module_info | Functions],
    Exports = Module:module_info(exports),
    [F || {F, 1} <- Exports, not lists:member(F, ExcludedFuns)].

-spec wait_for(any(), non_neg_integer()) -> ok.
wait_for(_Message, Timeout) when Timeout =< 0 ->
    timeout;
wait_for(Message, Timeout) ->
    receive
        Message -> ok
    after 10 -> wait_for(Message, Timeout - 10)
    end.

-spec wait_for_fun(term(), non_neg_integer(), non_neg_integer()) ->
    {ok, any()} | ok | timeout.
wait_for_fun(_CheckFun, _WaitTime, 0) ->
    timeout;
wait_for_fun(CheckFun, WaitTime, Retries) ->
    case CheckFun() of
        true ->
            ok;
        {true, Value} ->
            {ok, Value};
        false ->
            timer:sleep(WaitTime),
            wait_for_fun(CheckFun, WaitTime, Retries - 1)
    end.

-spec wait_until_mock_called(atom(), atom()) -> ok.
wait_until_mock_called(M, F) ->
    case meck:num_calls(M, F, '_') of
        0 ->
            timer:sleep(100),
            wait_until_mock_called(M, F);
        _ ->
            ok
    end.
