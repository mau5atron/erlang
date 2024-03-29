% The WebSocket Protocol
	
	% This chaapter explains what Websocket is any why it is vital component
	% of soft realtime Web applications.

% -------------------------------------------------------------------------

% Description
	
	% Websocket is an extension to HTTP that emulates plain TCP connections
	% between client, typically a web browser, and the server. It uses the
	% HTTP Upgrade mechanism to establish the connection.

	% Websocket connections are fully asynchronous, unlike HTTP/1.1 
	% (synchronous) and HTTP/2 (asynchronous, but the seerver can only
	% initiate streams in response to requests). With Websocket, the client
	% and the server can both send frames at any time without any
	% restriction. It is closer to TCP than any of the HTTP protocols.

	% Websocket is an IETF standard. Cowboy supports the standard and all
	% drafts that were previously implemented by browsers, excluding the
	% initial flawed draft sometimes known as "version 0".

% -------------------------------------------------------------------------

% Websocket vs HTTP/2
	
	% For a few years Websocket was the only way to have a bidirectional
	% asynchronous connection with the server. This changed when HTTP/2 was
	% introduced. While HTTP/2 requires the client to first perform a request
	% before the server can push data, this is only a minor restriction as
	% the client can do so just as it connects.

	% Websocket was designed as a kind-of-TCP channel to a server. It only
	% defines the framing and connection management and lets the developer
	% implement a protocol on top of it. For example you could implement IRC
	% over Websocket and use a JavaScript IRC client to speak to the server.

	% HTTP/2 on the other hadn is just an improvement over the HTTP/1.1
	% connection and request/response mechanism. It has the same semantics as
	% HTTP/1.1.

	% If all you need is to access an HTTP API, then HTTP/2 should be your
	% first choice. On the other hand, if what you need is a different
	% protocol, then you can use Websocket to implement it.

% -------------------------------------------------------------------------

% Implememtation

	% Cowboy implements Websocket as a protocol upgrade. Once the upgrade is
	% performed from the "init/2" callback, Cowboy switches to Websocket.
	% Please consult the next chapter for more information on initiating and
	% handling Websocket connections.

	% The implementation of Websocket in Cowboy is validated using the
	% Autobahn test suite, which is an extensive suite of tests covering all
	% aspects of the protocol. Cowboy passses the suite with 100% success,
	% including all optional tests.

	% Cowboy's Websocket implementation also includes the per message-deflate
	% and x-webkit-deflate-frame compression extensions.

	% Cowboy will automatically use compression when the "compress" option is
	% returned from the "init/2" function.