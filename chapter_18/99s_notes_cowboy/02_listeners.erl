% Listeners

	% A listener is a set of processes that listens on a port for new
	% connections. Incoming connections get handled by Cowboy. Depending on
	% the connection handshake, one or another protocol may be used.

	% This chapter is specific to Cowboy. Please refer to the Ranch User
	% Guide for more information about listeners.

	% Cowboy provides two types of listeners:
		% one listening for clear TCP connections
		% and one for listening for secure TLS connections. Both of them
		% support the HTTP/1.1 and HTTP/2 protocols.

% -------------------------------------------------------------------------

% Clear TCP Listener

	% The clear TCP listener will accept connections on the given port. A
	% typical HTTP server would listen on port 80. Port 80 requires special
	% permissions on most platforms so a common alternative is port
	% 8080.

	% The following snippet starts listening for connections on port 8080:

		start(_Type, _Args) ->
			Dispatch = cowboy_router:compile([
				{'_', [{"/", hello_handler, []}]}
			]),
			{ok, _} = cowboy:start_clear(my_http_listener, [{port, 8080}],
				#{env => #{dispatch => Dispatch}}),

			hello_erlang_sup:start_link().

	% The Getting Started chapter uses a clear TCP listener.

	% Clients connecting to cowboy on the clear listener port are expecte to
	% use either HTTP/1.1 or HTTP/2.

	% Cowboy supports both methods of initiating a clear HTTP/2 connection:
	% through the Upgrade mechanism (RFC 7540 3.2) or by sending the preface
	% directly (RFC 7540 3.4).

	% Compatibility with HTTP/1.0 is provided by Cowboy's HTTP/1.1
	% implementation.

% -------------------------------------------------------------------------

% Secure TLS Listener

	% The secure TLS listener will accept connections on the given port. A
	% typical HTTPS server would listen on port 443. Port 443 requires
	% special permissions on most platforms however so a common alternative
	% is port 8443

	% The function provided by Cowboy will ensure that the TLS options given
	% are following the HTTP/2 RFC with regards to security. For example,
	% some TLS extensions or ciphers may be disabled. This also applies to
	% HTTP/1.1 connections on this listener. If this is not desirable, Ranch
	% can be used directly to set up a custom listener.

		start(_Type, _Args) ->
			Dispatch = cowboy_router:compile([
				{'_', [{"/", hello_handler, []}]}
			]),

			% {ok, _} = cowboy:start_tls(my_https_listener, [
			% 	{port, 8443},
			% 	{certfile, "/path/to/certfile"},
			% 	{keyfile, "/path/to/keyfile"},
			% ], #{env => {dispatch, Dispatch}}),
			% rewriting the above to this

			ConnPort = {port, 8443},
			CertfilePath = {certfile, "/path/to/certfile"},
			KeyfilePath = {keyfile, "/path/to/keyfile"},
			{ok, _} = cowboy:start_tls(my_https_listener, [
				ConnPort,
				CertfilePath,
				KeyfilePath,
			], #{env => {dispatch, Dispatch}}),

			hello_erlang_sup:start_link().

	% Clients connecting to Cowboy on the secure listener are expected ot use
	% the ALPN TLS extension to indicate what protocols they understand.
	% Cowboy always prefers HTTP/2 over HTTP/1.1 when both are supported.
	% When neither are supported by the client, or when the ALPN extension
	% was missing, Cowboy expects HTTP/1.1 to be used.

	% Cowboy also advertises HTTP/2 support through the older NPN TLS
	% extension for compatibility. Note however that this support will likely
	% not be enabled by default when Cowboy 2.0 gets released.

	% Compatibility with HTTP/1.0 is provide by Cowboy's HTTP/1.1
	% implementation.

% -------------------------------------------------------------------------

% Stopping the Listener

	% When starting listeners along with the application, it is a good idea
	% to also stop the listener when the application stops. This can be done
	% by calling cowboy:stop_listener/1

		stop(_State) ->
			ok = cowboy:stop_listener(my_https_listener).

% -------------------------------------------------------------------------

% Protocol Configuration

	% The HTTP/1.1 and HTTP/2 protocols share the same semantics; only their
	% framing differs. The first is a text protocol and the second a binary
	% protocol.

	% Cowboy doesn't separate the configuration for HTTP/1.1 and HTTP/2.
	% Everything goes into the same map. Many options are shared.
	