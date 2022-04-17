-module(socket_examples).

nano_get_url() ->
	nano_get_url("www.google.com").

nano_get_url(Host) ->
	{ok, Socket} = gen_tcp:connect(Host, 80, [binary, {packet, 0}]),
	ok = gen_tcp:send(Socket, "GET / HTTP/1.0/\r\n\r\n"),
	receive_data(Socket, []).

receive_data(Socket, SoFar) ->
	receive
		{tcp, Socket, Bin} ->
			receive_data(Socket, [Bin|SoFar]);
		{tcp_closed, Socket} ->
			list_to_binary(reverse(SoFar))
	end.

% This works as follows:

% 1.

	% We open a TCP socket to port 80 of http://www.google.com by calling 
	% get_tcp:connect. The argument binary in the connect call tells the 
	% system to open the socket in "binary" mode and deliver all data to the 
	% application as binaries. {packet, 0} means the TCP data is delivered 
	% directly to the application in an unmodified form.

% 2.

	% We call get_tcp:send and send the message GET / HTTP/1.0\r\n\r\n to 
	% the socket. Then we wait for a repl. The reply doesn't come all in one 
	% packet but comes fragmented, a bit at a time. These fragments will be 
	% received as a sequence of messsages that are sent to the process that 
	% opened (or controls) the socket.

% 3.

	% We receive a {tcp, Socket, Bin} message. The third argument in this 
	% tuple is a binary. This is because we opened the socket in binary 
	% mode. This message is one of the data fragments sent to use fomr the 
	% web server. We add it to the list of fragments we have received so far 
	% and wait for the next fragment.


% 4.

	% We receive a {tcp_closed, Socket} message. This happens when the server has finished sending us data.

% 5.

	% When all the fragments have come, we've stored them in the wrong 
	% order, so we reverse the order and concentrate all the fragments.

% The code that reassembled the fragments looked like this:

	% look at receive_data(Socket, SoFar) ->.

% So, as the fragments arrive, we just add them to the head of the list
% SoFar. When all the fragments have arrived and the socket is closed, we
% reverse the list and concatenate all the fragments.

% You might think that it would be better to write the code to accumulate
% the fragments like this:

		
	% receive_data(Socket, SoFar) ->
	% 	receive
	% 	{tcp,Socket,Bin} ->
	% 	receive_data(Socket, list_to_binary([SoFar,Bin]));
	% 	        {tcp_closed,Socket} ->
	% 	            SoFar
	% 	end.

% This code is correct but less efficient than the original version. The
% reason is that in the latter version we are continually appending a new
% binary to the end of the buffer, which involves a lot of copying of data.
% It's much better to accumulate all the fragments in a list (which will
% end up in the wrong order) and then reverse the entire list and
% concatenate all the fragments in one operation.

% Let's test the HTTP client works:

	% 1> B = socket_examples:nano_get_url().
	% <<"HTTP/1.0 302 Found\r\nLocation: http://www.google.se/\r\n Cache-Control: private\r\nSet-Cookie: PREF=ID=b57a2c:TM"...>>

% NOTE: When you run nano_get_url, the result is a binary, so you'll see
% what a binary looks like when pretty printed in the Erlang shell. When
% binaries are pretty printed, all control characters are displayed in an
% escaped format. And the binary is truncated, which is indicated by the
% three dots (...>>) at the end of the printout. If you want to see all of
% the binary, you can print it with io:format or break it into pieces with
% string:tokens.


% -------------------------------------------------------------------------

% Writing a Web Server:

% Writing something like a web client or a server is great fun. Sure, other
% people have already written these things, but if we really want to
% understand how they work, digging under the surface and finding out
% exactly how they work is very instructive. Who knows - maybe our web
% server will be better than the best.

% To build a web server, or for that matter any software that implements a
% standard internet protocol, we neeed to use the right tools and need to
% know exactly which protocols to implement.

% In our example, code that fetched a web page we opened port 80 and sent
% it a GET / HTTP/1.0\r\n\r\n command. We used the HTTP protocol defined in
% RFC 1945. All the major protocols for Internet services are defined in
% requests for comments (RFCs). The official website for all RFCs is
% http://www.ietf.org (home of the Internet Engineering Task Force).

% The other invaluable source of information is a packet sniffer. With a
% packet sniffer we can capture and analyze all the IP packets coming from
% and going to our application. Most packet sniffers included software that
% can decode and analyze the data in the packets and present the data in a
% meaningful manner. One of the most well-known and possibly the best is
% Wireshark (previously known as Ethereal), available from
% http:www.wireshakr.org

% Armed with a packet sniffer dump and the appropriate RFCs, we're ready to
% write our next killer application.

	% 2> io:format("~p~n",[B]).
	% <<"HTTP/1.0 302 Found\r\nLocation: http://www.google.se/\r\n
	% Cache-Control: private\r\nSet-Cookie: PREF=ID=b57a2c:TM" TM=176575171639526:LM=1175441639526:S=gkfTrK6AFkybT3; expires=Sun, 17-Jan-2038 19:14:07
	% ... several lines omitted ...
	% >>


	% 3> string:tokens(binary_to_list(B), "\r\n").
	% ["HTTP/1.0 302 Found",
	%  "Location: http://www.google.se/",
	%  "Cache-Control: private",
	%  "Set-Cookie: PREF=ID=ec7f0c7234b852dece4:TM=11713424639526:
	%  LM=1171234639526:S=gsdertTrK6AEybT3;
	%  expires=Sun, 17-Jan-2038 19:14:07 GMT; path=/; domain=.google.com",
	%  "Content-Type: text/html",
	%  "Server: GWS/2.1",
	%  "Content-Length: 218",
	%  "Date: Fri, 16 Jan 2009 15:25:26 GMT",
	%  "Connection: Keep-Alive",
	% ... lines omitted ...


% Note that the 302 response code is not an error; it's the expected
% response of this command, which is to redirect to a new address. Also,
% note that this example shows how socket communication works and does not
% strictly follow the HTTP protocol.

% This is more or less how a web client works (with the emphasis on less -
% we would have to do a lot of work to correctly render the resulting data
% in a web browser). The previous code is, however, a good starting point
% for your own experiments. You might like to try modifying this code to
% fetch and store an entire website or automatically go and read your
% email. The possibilities are boundless.

% -------------------------------------------------------------------------

% A Simple TCP Server:

	% In the previous section, we wrote a simple client. Now lets write a
	% server.

	% This server opens port 2345 and then waits for a single message. This
	% message is a binary that contains an Erlang term. The term is an Erlang
	% string that contains an expression. The server evaluates the expression
	% and sends the result to the client by writing the result to the socket.

	% To write this program (and indeed any program that runs over TCP/IP),
	% we have to answer a few simple questions.

		% How is the data organized? How do we know how much data makes up a
		% single request or response?

		% How is the data within a request or the response encoded and decoded?
		% (Encoding the data is sometimes called marshaling, and decoding the
		% data is sometimes called demarshaling.)

	% TCP socket data is just an undifferentiated stream of bytes. During
	% transmission, this data can be broken into arbitrary-sized fragments,
	% so we need some convention so that we know how much data represents a
	% single request or response.

	% In the Erlang case we use the simple convention that every logical
	% request or response will be preceded by an N (1, 2, or 4) byte length
	% count. This is the meaning of the {packet, N} argument in the
	% gen_tcp:connect and gen_tcp:listen functions. The word packet here
	% refers to the length of an application request or response message, not
	% the physical packet seen on the wire. Note that the arguments to packet
	% used by the client and the server must agree. If the server was opened
	% with {packet, 2} and the client with {packet, 4}, then nothing would
	% work.

	% Having opened a socket with the {packet, N} option, we don't need to
	% worry about data fragmentation. The Erlang drivers will make sure that
	% all fragmented data messages are reassembled to the correct lengths
	% before delivering them to the application.

	% The next concern is data encoding and decoding. We'll use the simplest
	% possible way of encoding and decoding messages using term_to_binary to
	% encode Erlang terms and using its inverse, binary_to_term, to decode
	% the data.

	% Note that the packaging convention and encoding rules need for the
	% client to talk to the server is achieved in two lines of code, by using
	% the {packet, 4} option when we open the socket and by using
	% term_to_binary and its inverse to encode and decode the data.

	% The ease with which we can package and encode Erlang terms gives us a
	% significant advantage over text-based methods such as HTTP or XML.
	% Using the Erlang BIF term_to_binary and its inverse binary_to_term is
	% typically more than XML terms and involves sending far less data. NOw
	% to the programs. First, here's a very simple server:

		% Look at nano_server.erl

	% Note that this program accepts only a single request; once the program
	% has run to completion, then no more connections will be accepted.

	% This is the simplest of servers that illustrates how to package and
	% encode the application data. It accepts a request, computes a reply,
	% sends the reply, and terminates.

	% To test the server, we need a corresponding client.

		% Look at nano_client.erl


	% To test our code, we'll run both the client and server on the same
	% machine, so the hostname in the gen_tcp:connect function is hardwired
	% to localhost.

	% NOTE: term_to_binary is called in the client to encode the message and
	% how binary_to_term is called in the server to reconstruct the message.

	% To run this, we neede to open two terminal windows and start an Erlang
	% shell in each of the windows.

	% First we start the server.

		% SERVER 1> socket_examples:start_nano_server().

	% We won't see any output in the server window, since nothing has
	% happened yet. Then we move to the client window and give the following
	% command:

		% CLIENT 1> socket_examples:nano_client_eval("list_to_tuple(
		% [2+3*4,10+20])").

	% In the server window, we should see the following:

		% Server received binary = <<131,107,0,28,108,105,115,116,95,116,
                           % 111,95,116,117,112,108,101,40,91,50,
                           % 43,51,42,52,44,49,48,43,50,48,93,41>>

    % Server (unpacked) "list_to_tuple([2+3*4,10+20])"

    % Server replying
    % ok

  % Finally, in the server window, we'll see this:

  	% Server socket closed