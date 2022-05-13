%%%-------------------------------------------------------------------
%% @doc websocket_example public API
%% @end
%%%-------------------------------------------------------------------

-module(websocket_example_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    websocket_example_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
