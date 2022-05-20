%%%-------------------------------------------------------------------
%% @doc http_example public API
%% @end
%%%-------------------------------------------------------------------

-module(http_example_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([
      {'_', [{"/", http_example_handler, []}]}
  ]),
  {ok, _} = cowboy:start_clear(my_http_listener, [{port, 8080}],
      #{env => #{dispatch => Dispatch}}),
  http_example_sup:start_link().
% Listening for Connections
  % First we define the routes that Cowboy will use to map requests to
  % handler modules, and then we start the listener. This is best done at
  % application startup.

  % Routes are explained in details in the Routing chapter. Forst this
  % tutorial we map the path "/" to the handler module
  % "http_example_handler". This module does not exist yet.

  % Build and start the release, then open http://localhost:8080 in your
  % browser. You will get a 500 error because the module is missing. Any
  % other URL, like http://localhost:8080, will result in a 404 error.

stop(_State) ->
  ok.

%% internal functions

% Handling Requests

  % Cowboy features different kinds of handlers, including REST and
  % Websocket handlers. For this tutorial we will use a plain HTTP handler.

  % Generate a handler from a template:

    % $ make new t=cowboy.http n=http_example_handler

  % Then, open the src/http_example_handler.erl file and modify the init/2
  % function like this to send a reply. (look at docs and write in the
  % code)

  % What the above code does (in http_example_handler.erl) is send a 200 OK
  % reply, with the content-type header set to "text/plain" and the
  % response body set to "Hello Erlang!".

  % If you run the release and open http://localhost:8080 in your browser,
  % you should get a nice "Hello Erlang!" displayed!