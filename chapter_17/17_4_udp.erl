-module(17_4_udp).
-export([]).

% 17.4 UDP

% Now let's look at the User Datagram Protocol (UDP). Using UDP, machines
% on the Internet can send each other short messages called datagrams. UDP
% datagrams are unreliable. This means if a client sends a sequence of UDP
% datagrams to a server, then the datagrams might arrive out of order, not
% at all, or even more than once, but the individual datagrams, if they
% arrive, will be undamaged. Large datagrams can get split into smaller
% fragments, but the IP protocol will reassemble the fragments before
% delivering them to the application.

% UDP is a connectionless protocol, which means the client does not have to
% establish a conection to the server before sending it a message. This
% means that UDP is well suited for applications where large numbers of
% clients send small messages to a server.

% Writing a UDP client and server in Erlang is much easier than writing in
% the TCP case since we don't have to worry about maintaining connections
% to the server.

% -------------------------------------------------------------------------

% The Simplest UDP Server and Client

	% First let's discuss the server. The general form of a UDP server is as
	% follows:

		% server(Port) ->
			% {ok, Socket} = gen_udp:open(Port, [binary]),
			% loop(Socket).

		% loop(Socket) ->
			% receive
				% {udp, Socket, Host, Port, Bin} ->
					% BinReply = ....,
					% gen_udp:send(Socket, Host, Port, BinReply),
					% loop(Socket)
			% end.


	% This is somewhat easier than the TCP case since we don't need to worry
	% about our process receiving "socket closed" messages. Note that we 
	% opened the socket in a binary mode, which tells the driver to send all
	% message to the controlling process as binary data.

	% Now the client. Here's a very simple client. It merely opens a UDP
	% socket, sends a message to the server, waits for a reply (or timeout),
	% and then closes the socket and returns the value returned by the 
	% server.

		% client(Request) ->

			% {ok, Request} = gen_udp:open(0, [binary]),
			% ok = gen_udp:send(Socket, "localhost", 4000, Request),
			% Value = receive
			% 					{udp, Socket, _, _, Bin} ->
			% 						{ok, Bin}
			% 				after 2000 ->
			% 					error
			% 				end,

			% gen_udp:close(Socket),
			% Value

	% We must have a timeout since UDP is unreliable and we might not 
	% actually get a reply.

% -------------------------------------------------------------------------

% A UDP Factorial Server

	% We can easily build a UDP server that computes the good ol' factorial
	% of any number that is sent to it. The code is modeled on that in the
	% previous section.

		% look at udp_test.erls

	% Note that I have added a few print statements so we can see what's 
	% happening when we run the program. I always add a few print statements 
	% when I develop a program and then edit or comment them out when the 
	% program works.

	% Now let's run this example. First we start the server.

		% 1> udp_test:start_server().
		% server opened socket:#Port<0.106>
		% <0.34.0>

	% This runs in the background, so we can make a client request to request
	% the value of factorial 40.

		% 2> udp_test:client(40).
		% client opened socket=#Port<0.105>
		% server received:{udp, #Port<0.106>, {127,0,0,1}, 32785,
		% <<131,97,40>>}
		% client received:{udp, #Port<0.105>,
		% 								{127,0,0,1}, 4000,
		% <<131,110,20,0,0,0,0,0,64,37,5,255,100,222,15,8,126,242,199,132,27,23
		% 2,234,142>>}

		% 815915283247897734345611269596115894272000000000

	% And now we have built a little UDP factorial server. Just for fun you
	% might like to try writing the TCP equivalent of this program and
	% benchmarking the two against eachother.

% -------------------------------------------------------------------------

% UDP Packet Gotchas

	% We should note that because UDP is a connectionless protocol, the
	% server has no way to block the client by refusing to read data from it
	% -- the server has no idea who the clients are.

	% Large UDP packets might become fragmented as they pass through the
	% network. Fragmentation occurs when the UDP data size is greater than
	% the maximum transfer unit (MTU) size allowed by the routers that the
	% packet passes through when it travels over the network. The usual
	% advice given in tuning a UDP network is to start with a small packet
	% size (say, about 500 bytes) and then gradually increase it while
	% measuring throughput. If at some point the throughput drops
	% dramatically, then you know the packets are too large.

	% A UDP packet can be delivered twice (which surprises some people), so
	% you have to be careful writing code for remote procedure calls. It
	% might happen that the reply to a second query was in fact a duplicated
	% answer to the first query. To avoid this, we could modify the client
	% code to include a unique reference and check that this reference is
	% returned by the server. To generate a unique reference, we call the
	% Erlang BIF make_ref, which is guaranteed to return a globally unique
	% reference. The code for a remote procedure call now looks like this:

		% client(Request) ->
			% {ok, Socket} = gen_udp:open(0, [binary]),
			% Ref = make_ref(), % Make a unique reference
			% B1 = term_to_binary({Ref, Request}),
			% ok = gen_udp:send(Socket, "localhost", 4000, B1),
			% wait_for_ref(Socket, Ref).

		% wait_for_ref(Socket, Ref) -> 
			% receive
				% {udp, Socket, _, _, Bin} ->
					% case binary_to_term(Bin) of
						% {Ref, Val} ->
							% Val; % Got the correct value
						% {_SomeOtherRef, _} ->
							% wait_for_ref(Socket, Ref) % some other value throw it away
					% end; 
			% after 1000 ->
				% ....
			% end.

	% Now we're done with UDP. UDP is often used for online gaming where low
	% latency is required, and it doesn't matter if the odd packet is lost.