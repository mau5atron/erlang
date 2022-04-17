-module(nano_server).
-export([start_nano_server/0]).

start_nano_server() ->
	{ok, Listen} = gen_tcp:listen(2345, [binary, {packet, 4},
																			        {reuseaddr, true},
																			        {active, true}]),
	{ok, Socket} = gen_tcp:accept(Listen),
	gen_tcp:close(Listen),
	loop(Socket).

loop(Socket) ->
	receive
		{tcp, Socket, Bin} ->
			io:format("Server received binary = ~p~n", [Bin]),
			Str = binary_to_term(Bin),
			io:format("Server (unpacked) ~p~n", [Str]),
			Reply = lib_misc:string2value(Str),
			io:format("Server replying = ~p~n", [Reply]),
			gen_tcp:send(Socket, term_to_binary(Reply)),
			loop(Socket);
		{tcp_closed, Socket} ->
			io:format("Server socket closed~n")
	end.

% -------------------------------------------------------------------------

% Program Meaning:

	% 1:

		% First, we call gen_tcp:listen to listen for a connection on port 
		% 2345 and set up the message packaging conventions. {packet, 4} means 
		% that each application message will be preceded by a 4-byte length 
		% header. Then, gen_tcp:listen(...) returns {ok, Listen} or {error, 
		% Why}, but we're interested only in the return case where we were 
		% able to open a socket. Therefore, we write the following code:

			% {ok, Listen} = gen_tcp:listen(..),

		% This causes the program to raise a pattern matching exception if gen_tcp:listen returns {error, ...}. In the successful case, this 
		% statement binds Listen to the new listening socket. There's only one 
		% thing we can do with a listening socket, and that's to use it as an 
		% argument to gen_tcp:accept.

	% 2:

		% Now we call gen_tcp:accept(Listen). At this point, the program will 
		% suspend and wait for a connection. When we get a connection, this 
		% function returns with the variable Socket bound to a socket than can 
		% be used to talk to the client that performed the connection.

	% 3:

		% When accept returns, we immediately call gen_tcp:close(Listen). This 
		% closes down the listening socket, so the server will not accept any 
		% new connections. This does not affect the existing connection; it 
		% just prevents new connections.

	% 4:

		% We decode the input data (unmarshaling)

	% 5:

		% Then we evaluate the string.

	% 6:

		% Then we encode the reply data (marshaling) and send it back to the
		% socket.