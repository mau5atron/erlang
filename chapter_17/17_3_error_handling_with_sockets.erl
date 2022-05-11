-module(17_3_error_handling_with_sockets).
-export([]).

% 17.3 Error Handling with Sockets

	% Error handling with sockets is extremely easy - basically you don't 
	% have to do anything. As we said earlier, each socket has a controlling 
	% process (that is, the process that created the socket). If the 
	% controlling process dies, then the socket will be automatically closed.

	% This means that if we have, for example, a client and a server and the
	% server dies because of a programming error, the socket owned by the
	% server will be automatically closed.

	% We can test this mechanism with the following small program:

		% socket_examples.erl

		% error_test() ->
			% spawn(fun() -> error_test_servere() end),
			% lib_misc:sleep(2000),
			% {ok, Socket} = gen_tcp:connect("localhost", 4321, [binary, 
			% {packet, 2}]),
			% io:format("connected to: ~p~n", [Socket]),
			% gen_tcp:send(Socket, <<"123">>),
			% receive
				% Any ->
					% io:format("Any: ~p~n", [Any])
			% end.

		% error_test_server() ->
			% {ok, Listen} = gen_tcp:listen(4321, [binary, {packet, 2}]),
			% {ok, Socket} = gen_tcp:accept(Listen),
			% error_test_server_loop(Socket).

		% error_test_server_loop(Socket) ->
			% receive
				% {tcp, Socket, Data} ->
					% io:format("received: ~p~n", [Data]),
					% _ = atom_to_list(Data),
					% error_test_server_loop(Socket)
			% end.

	% When we run it, we see the following:

		% 1> socket_examples:error_test().
		% connected to:#Port<0.152>
		% received: <<"123">>

		% =ERROR REPORT==== 30-Jan-2009::16:57:45 === Error in process <0.77.0> with exit value:
  	%  {badarg,[{erlang,atom_to_list,[<<3 bytes>>]},
  	%  {socket_examples,error_test_server_loop,1}]}
  	% Any={tcp_closed,#Port<0.152>}
  	% ok

  % We spawn a server, sleep for two seconds to give it a chance to start,
  % and then send it a message containing the binary <<"123">>. When this
  % message arrives at the server, the server tries to compute atom_to_list
  % (Data) where Data is a binary and immediately crashes. The system
  % monitor prints the diagnostic that you can see in the shell. Now that
  % the controlling process for the server side of the socket has crashed,
  % the (server-side) socket is automatically closed. The client is then
  % sent a {tcp_closed, Socket} message.