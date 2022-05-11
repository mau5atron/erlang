-module(17_2_active_and_passive_sockets).
-export([]).

% 17.2 Active and Passive Sockets.

	% Erlang sockets can be opened in one of three modes:

		% {active, true | false | once}

	% in the Options argument to either gen_tcp:connect(Address, Port, 
	% Options or gen_tcp:listen(Port, Options).

	% If {active, true} is specified, then an active socket will be created; 
	% {active, false} specifies a passive socket. {active, once} creates a
	% socket that is active but only for the reception of one message; after
	% it has received this message, it must be reenabled before it can
	% receive the next message.

	% We'll go through how these different types of sockets are used in the
	% following sections.

	% The difference between active and passive sockets has to do with what
	% happens when messages are received by the socket.

		% Once an active socket has been created, the controlling process will
		% be sent {tcp, Socket, Data} messages as data is received. There is no
		% way the controlling process can control the flow of these messages. A
		% rogue client could send thousands of messages to the system, and
		% these would all be sent to the controlling process. The controlling
		% process cannot stop this flow of messages.

		% If the socket was opened in passive mode, then the controlling
		% process has to call gen_tcp:recv(Socket, N) to receive data from the
		% socket. It will then try to receive exactly N bytes from the socket.
		% If N = 0, then all available bytes are returned. In this case, the
		% server can control the flow of messages from the client by choosing
		% when to call gen_tcp:recv.

	% Passive sockets are used to control the flow of data to a server. To
	% illustrate this, we can write the message reception loop of a server in
	% three ways:

		% Active message reception (nonblocking)
		% Passive message reception (blocking)
		% Hybrid message reception (partial blocking)

% -------------------------------------------------------------------------

% Active Message Reception (Nonblocking)
	
	% Our first example opens a socket in active modeand then receives
	% messages from the socket.

		% {ok, Listen} = gen_tcp:listen(Port, [.., {active, true}...]),
		% {ok, Socket} = get_tcp:accept(Listen),
		% loop(Socket).

		% loop(Socket) ->
			% receive
				% {tcp, Socket, Data} ->
					% do something with the data
				% {tcp_closed, Socket} ->
					% ...
			% end

	% This process cannot control the flow of messages to the server loop. If
	% the client produces data faster than the server can consume this data,
	% then the system can be flooded with messages - the message buffers will
	% fill up, and the system might crash or behave strangely.

	% This type of server is called a nonblocking server because it cannot
	% block the client. We should write a nonblocking server only if we can
	% convince ourselves that it can keep up with the demands of the clients.

% -------------------------------------------------------------------------

% Passive Message Reception (Blocking)

	% In this section, we'll write a blocking server. The server opens the
	% socket in passive mode by setting the {active, false} option. This
	% server cannot be crashed by an overactive client that tries to flood it
	% with too much data.

	% The code in the server loop calls gen_tcp:recv every time it wants to
	% receive data. The client will block until the server has called recv.
	% Note that the OS does some buffering that allows the client to send a
	% small amount of data before it blocks even if recv has not been called.

		% {ok, Listen} = gen_tcp:listen(Port, [.., {active, false}...]),
		% {ok, Socket} = gen_tcp:accept(Listen),
		% loop(Socket).

		% loop(Socket) ->
			% case gen_tcp:recv(Socket, N) of
				% ... do something with the data
				% loop(Socket);
			% end.

% -------------------------------------------------------------------------

% The Hybrid Approach (Partial Blocking)

	% You might think that using passive mode for all servers is the correct
	% approach. Unfortunately, when we're in passive mode, we can wait for
	% the data from only the socket. This is useless for writing servers that
	% must wait for data from multiple sockets.

	% Fortunately, we can adopt a hybrid approach, neither blocking nor
	% non-blocking. We open the socket with the option {active, once. In this
	% mode, the socket is active but only for one message. After the
	% controlling processes has been sent a message, it must explicitly call
	% inet:setopts to reenable reception of the next message. The system will
	% block until this happens. This is the best of both worlds. Here's what
	% the code looks like:

		% {ok, Listen} = gen_tcp:listen(Port, [.., {active, once}...]),
		% {ok, Socket} = gen_tcp:accept(Listen),
		% loop(Socket).

		% loop(Socket) ->
			% receive
				% {tcp, Socket, Data} ->
					% ... do something with the data...
					% When you're ready, enable the next message
					% inet:setopts(Sock, [{active, once}]),
					% loop(Socket);

				% {tcp_closed, Socket} ->
					% ....
			% end.


	% Using the {active, once} option, the user can implement advanced forms
	% of flow control (sometimes called traffic shaping) and thus prevent a
	% server from being flooded by excessive messages.

% -------------------------------------------------------------------------

% Finding Out Where Connections Come From

	% Suppose we write some kind of online server and find that somebody
	% keeps spamming our site. To try to prevent this, we need to know where
	% the connection came from. To discover this, we can call inet:peername
	% (Socket).


		% @spec inet:peername(Socket) -> {ok, {IP_Address, Port}} | {error,
		% Why}

			% This returns the IP address and port of the other end of the
			% connection so the server can discover who initiated the connection.
			% IP_Address is a tuple of integers, with {N1, N2, N3, N4}
			% representing the IP address for IPv4 and {K1, K2, K3, K4, K5, K6,
			% K7, K8} representing it for IPv6. Here Ni integers in the range 0
			% to 255, and Ki are integers in the range 0 to 65535.