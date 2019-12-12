%%%-------------------------------------------------------------------
%% @doc zero public API
%% @end
%%%-------------------------------------------------------------------

-module(zero_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:fwrite("Application start~n",[]),
    zero_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
