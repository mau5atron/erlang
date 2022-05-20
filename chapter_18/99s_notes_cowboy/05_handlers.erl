% Handlers

	% Handlers are Erlang modules that handle HTTP requests.

% -------------------------------------------------------------------------

% Plain HTTP Handlers

	% The most basic handler in Cowboy implements the mandatory init/2 
	% callback, which manipulates the request, and optionally sends a
	% response and then returns.

	% This callback receives the Req object (https://ninenines.eu/docs/en/cowboy/2.9/guide/req) and the initial state defined
	% in the router configuration 
	% (https://ninenines.eu/docs/en/cowboy/2.9/guide/routing).

	% A handler that does nothing would look like this:

		init(Req, State) ->
			{ok, Req, State}.

	% Despite sending no reply, a "204 No Content" response will be sent to
	% the client, as Cowboy makes sure that a response is sent for every
	% request.

	% We need to use the Req object to reply.

		init(Req0, State) ->
			Req = cowboy_req:reply(200, #{<<"content-type">> =>
				<<"text/plain">>}, <<"Hello world!">>, Req0),
			{ok, Req, State}.

	% Cowboy will immediately send a response when cowboy:reply/4 is called.

	% We then return a 3-tuple. "ok" means that the handler ran successfully.
	% We also give the modified Req back to Cowboy.

	% The last value of the tuple is a state that will be used in every
	% subsequent callbacks to this handler. Plain HTTP handlers only have one
	% additional callback, the optional and rarely use terminate/3.

% -------------------------------------------------------------------------

% Other Handlers

	% The init/2 callback can also be used to inform Cowboy that this is a
	% different kind of handler and that Cowboy should switch to it. To do
	% this you simply need to return the module name of the handler type you
	% want to switch to.

	% Cowboy comes with three handler types you can switch to: cowboy_rest,
	% cowboy_websocket and cowboy_loop. In addition to those, you can define
	% your own handler types.

	% Switching is simple. Instead of returning "ok", you simply return the
	% name of the handler type you want to use. The following snippet
	% switches to a Websocket handler.

		init(Req, State) ->
			{cowboy_websocket, Req, State}.

% -------------------------------------------------------------------------

% Cleaning Up

	% All handler types provide the optional terminate/3 callback.

		terminate(_Reason, _Req, _State) ->
			ok.

	% This callback is strictly reserved for any required cleanup. You cannot
	% send a response from this function. There is no other return value.

	% This callback is optional because it is rarely necessary. Cleanup
	% should be done in separate processes directly (by monitoring the
	% handler process to detect when it exits).

	% Cowboy does not reuse processes for different requests. The process
	% will terminate soon after this call returns.

	