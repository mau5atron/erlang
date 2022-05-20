%%%-------------------------------------------------------------------
%% @doc websocket_example public API
%% @end
%%%-------------------------------------------------------------------

-module(websocket_example_app).

-behaviour(application).

-export([start/2, stop/1]).
-import(websocket_example_cowboy_handler_ws, []).

start(_StartType, _StartArgs) ->
  DispatchWS = cowboy_router:compile([
    {'_', [
      {"/ws", websocket_example_cowboy_handler_ws, []},
    ]}
  ]),

  {ok, _} = cowboy:start_http(example_ws, 100, [{port, 2233}], [{env, [
    {dispatch, DispatchWS}]}]),

  websocket_example_sup:start_link().

% In the example above we've a route with a websocket handler
% which should be implemented and told Cowboy to listen to port 8889. 
% Also, it's possible to listen to multiple ports. All you need to do is
% just call cowboy:start_http multiple times but please note that the first
% parameter of the function (listener name) should be different each time.

stop(_State) ->
    ok.

%% internal functions
