-module(17_6_a_shoutcast_server).

% 17.6 A Shoutcast Server

	% To finish off this chapter, we'll use our newly acquired skills in 
	% socker programming to write a SHOUTcast server. SHOUTcast is a 
	% protocol developed by the folks at Nullsoft for streaming audio data. 
	% SHOUTcast sends MP3 or AAC encoded audio data using HTTP as the 
	% transport protocol.
	

	% Skipping.......

	% The End

	% In this chapter, we looked at only the most commonly used functions for
	% manipulating sockets. You can find more information abiout the socket
	% APIs in the manual pages for gen_tcp, gen_udp, and inet.

	% The combination of a simple socket interface, together with BIFs
	% term_to_binary/1 and its inverse binary_to_term, makes networking
	% really easy. I recommend that you work through the following exercises
	% to experience this first hand.

	% In the next chapter, we'll look at websockets. Using websockets, an
	% Erlang process can communicate directly with a web browser without
	% following the HTTP protocol. This is ideal for implementing low-latency
	% web applications and provides an easy way to program web applications.

% -------------------------------------------------------------------------

% Exercises:

	% 1.

		% Modifu the code for nano_get_url/0 (in section Fetching Data from a
		% Server, page 264), adding appropriate headers where necessary and
		% performing redirects if needed, in order to fetch any web page. Test
		% this on several sites.

	% 2.

		% Enter the code for (A Simple TCP server, on page 267). Then modify
		% the code to receive a {Mod, Func, Args} tuple instead of a string.
		% Finally, compute reply = apply(Mod, Func, Args) and send the value
		% back to the socket.

		% Write a function nano_client_eval(Mod, Func, Args) that is similar
		% tot the version shown earlier in this chapter, which encodes Mod,
		% Func, and Arity in a form understood by the modified server code.

		% Test that the client and server code function correctly, first on the
		% same machine, then on two machines in the same LAN, and then on two
		% machines on the internet.

	% 3.

		% Repeat the previous exercise using UDP instead of TCP
	
	% 4.

		% Add a layer of cryptography by encoding the binary immediately before
		% sending it to the outgoing socket and decoding it immediately after
		% it is received on the incoming socket.

	% 5.

		% Make a simple "email-like" system. Use Erlang terms as messages and
		% store them in a directory ${HOME}/mbox