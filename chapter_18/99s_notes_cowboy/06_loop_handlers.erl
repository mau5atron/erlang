% Loop Handlers
	
	% Loop handlers are a special kind of HTTP handlers used when the 
	% response can not be sent right away. The handler enters instead a
	% receive loop waiting for the right message before it can send a
	% response.

	% Loop handlers are used for requests where a response might not be
	% immediately available, but where you would like to keep the connection
	% open for a while in case the response arrives. The most known example
	% of such a practice is known as long polling.

	% Loop handlers can also be used for requests where a response is
	% partially available and you need to stream the response body while the
	% connection is open. The most known example of such practice is
	% server-sent events, but it also applies to any response that takes a
	% long time to send.

	% While the same can be accomplished using plain HTTP handlers, it is
	% recommended to use loop handlers because they are well-tested and allow
	% using built-in features like hibernation and timeouts.

	% Loop handlers essentially wait for one or more Erlang messages and
	% feed these messagess to the info/3 callback. It also features the
	% init/2 and terminate/3 callbacks which work the same as for plain HTTP
	% handlers.

% -------------------------------------------------------------------------

% Initialization

	% The init/2 function must return a cowboy_loop tuple to enable loop
	% handle behavior. This tuple may optionally contain the atom "hibernate"
	% to make the process enter hibernation until a messsage is received.

	% This snippet enables the loop handler:

		init(Req, State) ->
			{cowboy_loop, Req, State}.

	% This also makes the process hibernate:

		init(Req, State) ->
			{cowboy_loop, Req, State, hibernate}.

% -------------------------------------------------------------------------

% Receive Loop

	% Once initialized, Cowboy will wait for messsages to arrive in the
	% process' mailbox. When a message arrives, Cowboy calls the info/3
	% function with the message, the Req object and the handler's state.

	% The following snippet sends a reply when it receives a "reply" message
	% from another process, or waits for another message otherwise.

		info({reply, Body}, Req, State) ->
			cowboy_req:reply(200, #{}, Body, Req),
			{stop, Req, State};

		info(_Msg, Req, State) ->
			{ok, Req, State, hibernate}.

	% Do note that the "reply" tuple here may be any message and is simply an
	% example. This callback may perform any necessary operation including
	% sending all or parts of a reply, and will subsequently return a tuple
	% indicating if more messages are ot be expected.

	% The callback may also choose to do nothing at all and just skip the
	% message received.

	% If a reply is sent, then the "stop" tuple should be returned. This will
	% instruct Cowboy to end the request.

	% Otherwise an "ok" tuple should be returned.

% -------------------------------------------------------------------------

% Streaming Loop

	% Another common case well suited for loop handlers is streaming data
	% received in the form of Erlang messages. This can be done by initiating
	% a chunked reply in the init/2 callback and then using
	% cowboy_req:chunk/2 every time a message is received.

	% The following snippet does exactly that. As you can see a chunk is sent
	% every time an "event" message is received, and the loop is stopped by
	% sending an "eof" message.

		init(Req, State) ->
			Req2 = cowboy_req:stream_reply(200, Req),
			{cowboy_loop, Req2, State}.

		info(eof, Req, State) ->
			{stop, Req, State};

		info({event, Data}, Req, State) ->
			cowboy_req:stream_body(Data, nofin, Req),
			{ok, Req, State};

		info(_Msg, Req, State) ->
			{ok, Req, State}.

% -------------------------------------------------------------------------

% Cleaning Up

	% Refer to the Handlers chapter 
	% (https://ninenines.eu/docs/en/cowboy/2.9/guide/handlers) for general
	% instructions about cleaning up.

% -------------------------------------------------------------------------

% Hibernate
	
	% To save memory, you may hibernate the process in between messages
	% received. This is done by returning the atom "hibernate" as part of the
	% "loop" tuple callbacks normally return. Just add the atom at the end
	% and Cowboy will hibernate accordingly.