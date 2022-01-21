-module(chapter_notes_concurrent_programming).

% Writing concurrent programs is easy once we know sequential Erlang. All
% we need are three new primitives:

	% spawn
		% Creates a parallel process
	% send
		% Sends a message to a process
	% receive
		% receives messages

% Erlang concurrency is based on processes. These are small, self-contained
% virtual machines that can evaluate Erlang functions.

% I'm sure you've met processes before, but only in the context of
% operating systems. In Erlang, processes belong to the programming
% language and not the operating system. This means that Erlang processes
% will have the same logical behavior on any operating system, so we can
% write portable concurrent code that can run on any operating system that
% supports Erlang.

% In Erlang:

	% Creating and destroying processes is very fast.

	% Sending messages between processes is very fast.

	% Processes behave the same way on all operating systems.

	% We can have very large numbers of processes.

	% Processes share no memory and are completely independent.

	% The only way for processes to interact is through message passing.

% For these reasons Erlang is sometimes called a pure message passing
% language.

% If you haven't programmed with processes before, you might have heard
% rumors that it is rather difficult. You've probably heard horror stories
% of memory violations, race conditions, shared-memory corruption, and the
% like. In Erlang, programming with processes is easy.


% 12.1 The Concurrency Primitives:

concurrency_primitives() ->
	% Everything we've learned about sequential programming is still true for
	% concurrent programming. All we have to do is ad the following
	% primitivess:


		% Pid = spawn(Mod, Func, Args)
			% Create a new concurrent process that evaluates apply(Mod. Func,
			% Args). The new process runs in parallel with the caller. spawn
			% returns a Pid (short for process identifier). You can use a Pid to
			% send messsages to the process. Note that the function Func with
			% arity length(Args) must be exported from the module Mod.

		% Pid = spawn(Func)
			% Creates a new concurrent process that evaluates Func(). This form
			% of spawn always uses the current value of the fun being evaluated,
			% and this fun does not have to be exported from the module.

			% The essential difference between the two forms of spawn has to do
			% with dynamic code upgrade. How to choose between the two forms of
			% spawn is discussed in section 12.8, (Spawning with MFAs or Funs,
			% pg 197).

		% Pid ! Message
			% Sends messsage to the process wit identifier Pid. Message sending
			% is asynchronous. The sender does not wait but continues with what
			% it was doing. ! is called the <send> operator

		% receive ... end
			% Receives a message that has been sent to a process. It has the
			% following syntax:

			% Example:

				receive
					Pattern1 [when Guard1] ->
						Expressions1;
					Pattern2 [when Guard2] ->
						Expressions2;
				end

			% When a message arrives at the process, the system tris to match it
			% against Pattern1 (with possible guard Guard1); if this succeeds, it
			% evaluates Expressions1.

			% If the first pattern does not match, it tries Pattern2, and so on.
			% If no pattern matches, the message is saved for later processing,
			% and the process waits for the next message. This is described in
			% more detail in section 12.5 (Selective Receive) pg 193

			% The patterns and guards used in a receive statement have exactly
			% the same syntactic form and meaning as the patterns and guards that
			% we use when we define a function.
	.

% That's it. You don't need threading and locking and semaphores and
% artificial controls.

% When a spawn command is executed, the system creates a new process. Each
% process has an associated mailbox that is also created when the process
% is created.

% When you send a message to a process, the message is put into the mailbox
% of the process. The only time the mailbox is examined is when your
% program evaluates a receive statement.

% Using the three primitives, we can recast the area/1 function in section
% 4.1, pg 43 into a process.

% look at geometry.erl

% We can then create a process that evaluates loop/0 in the shell

% 1> Pid = spawn(area_server0, loop, []).
% <0.36.0
% 2> Pid ! {rectangle, 6, 10}.
% Area of rectangle is 60
% {rectangle, 6, 10}
% 3> Pid ! {square, 12}.
% Area of square is 144
% {square, 144}

% In line 1 we created a new parallel process. spawn(area_server, loop, [])
% creates a parallel process that evaluates area_server:loop(); it returns
% Pid, which is printed as <0.36.0>

% In line 2 we sent a message to the process. This message matches the
% first pattern in the receive statement in loop/0:

% Having received a message, the process prints the area of the rectangle.
% Finally, the shell prints {rectangle, 6, 10}. This is because the value
% of Pid ! Msg is defined to be Msg.

% 12.2 Introducing Client Server:
client_server() ->
	% Client-server architectures are central to Erlang. Traditionally,
	% client-server architectures have involved a network that separates a
	% client from a server.

	% Most often there are multiple instances of the client and a single
	% server. The word server often conjures up a mental image of some rather
	% heavyweight software running on a specialized machine.

	% In our case, a much lighter-weight mechanism is involved. The client
	% and server in a client-server architecture are separate processes, and
	% normal Erlang message passing is used for communication between the
	% client and the server. Both client and server can run on the same
	% machine or on two different machines.

	% The words client and server refer to the roles that these two processes
	% have:
		% The client alwasys initiates a computation by sending a request to
		% the server.

		% The server computes a reply and sends a response to the client

	% Let's write our first client-server application
	.


% In the previous program, all that we needed was to send a request to a
% process that received and printed that request. Now, what we want to do
% is send a response to the process that sent the original request. The
% trouble is we do not know to whom to send the response. To send a
% response, the client has to include an address to which the server can
% reply. This is like sending a letter to somebody -- if you want to get a
% reply, you had better include your address in the letter.

% So, the sender must include a reply address. This can be done by changing
% this:

	% Pid ! {rectangle, 6, 10}

% To the following
	
	% Pid ! {self(), {rectangle, 6, 10}}

% self() is the PID of the client process.

% To Respond to the request, we have to change the code that receives the
% request from this:

% look at area_server1.erl

% Note how we now send the result of our calculation back to the process
% identified by the From parameter. Because the client set this parameter
% to its own process ID, it will receive the result.

% The process that sends requests is usually called a client. The process
% that receives requests and replies to the client is called a server.

% In addition, it's good practice to make sure every message sent to a
% process is actually received. If we send a message to the process that
% doesn't match one of the two patterns in the original receive statement,
% then this message will end up in the mailbox of the process and never be
% received. To deal with this, we add a clause at the end of the receive
% statement that is guaranteed to match any message that is sent to the
% process.

% Finally, we add a small utility function called rpc (short for remote
% procedure call) that encapsulates sending a request to a server and
% waiting for a response.

	
	% 1> Pid = spawn(area_server1, loop, []). 
	% <0.36.0>
	% 2> area_server1:rpc(Pid, {rectangle,6,8}).
	% 48
	% 3> area_server1:rpc(Pid, {circle,6}). 
	% 113.097
	% 4> area_server1:rpc(Pid, socks). 
	% {error,socks}


% There's a slight problem with this code. In the function rpc/2, we send a
% request to the server and then wait for a response. But we do not wait
% for a response from the server, we wait for any message. If some other
% process sends the client a messsage while it is waiting for a response
% from the server, it will misinterpret this message as a response from the
% server. We can correct this by changing the form of the receive statement
% to this:

	% loop() ->
		% receive
			% {From, ....} ->
				% From ! {self(), ...}
				% loop()
				% ...
		% end

% and by changing rpc to the following:

	% rpc(Pid, Request) ->
		% Pid ! {self(), Request},
		% receive
			% {Pid, Response} -> Response
		% end.

% When we call the rpc function, Pid is bound to some value, so in the
% pattern {Pid, Response}, Pid is bound, and Response is unbound. This
% pattern will match only a message containing a two-element tuple where
% the first element is Pid. All other messages will be queued. (receive
% provides what is called selective receive)

% So now we've built a simple client-server module. All we needed were the
% three primitives, spawn, send, and receive. This pattern will repeat over
% and over again in major and minor variations, but the underlying ideas
% are always the same.


% 12.3 Processes are Cheap:
processes_are_cheap() ->
	% At this point, you might be worried about performance. After all, if
	% we're creating hundreds or thousands of Erlang processes, we must be
	% paying some kind of penalty. Let's find out how much.

	% We'll do a bunch of spawns, create loads of processes, and time how
	% long it takes. Here's the program; note that here we use spawn(Fun) and
	% that the function being spawned does not have to be exported from the
	% module:

	% Look at processes.erl
	% Note that we used the BIF erlang:system_info(process_limit) to find the
	% maximum allowed number of processes. Some of these processes are
	% reserved, so your program cannot actually use this number. When we
	% exceed the system limit, the system refuses to start any more processes
	% and produces an error report.

	% The system limit is set to 262,144; to exceed this limit, you have to
	% start the Erlang emulator with the +P flag

		% $ processes:max(300000).

	% In the previous example, the actual value chose is the next highest
	% power of two that is greater than the supplied argument. The actual
	% value can be obtained by calling erlang:system_info(process_limit). We
	% can see that the process spawn time increases as we increase the number
	% of processes. If we continue to increase the number of processes, we
	% will reach a point where we run out of physical memory, and the system
	% will start swapping physical memory to disk and run dramatically lower.

	% If you're writing a program that uses a large number of processes, it's
	% a good idea to find out how many processes can fit into a physical
	% memory before the system starts swapping memory to disk and to make
	% sure that your program will run in physical memory.

	% As you can see, creating large numbers of processes is pretty fast. If
	% you're a C or Java programmer, you might hesitate to use a large number
	% of processes, and you would have to take care managing them. In Erlang,
	% creating processes simplifies programming instead of complicating it.
	.

% 12.4 Receive with a Timeout:
receive_with_timeout() ->
	% Sometimes a receive statement might wait forever for a message that
	% never comes. This could be for a number of reasons. For example, there
	% might be a logical error in our program, or the process that was going
	% to send us a message might have crashed before it sent the message. To
	% avoid this problem, we can add a timeout to the receive statement. This
	% sets a maxmimum time that the process will wait to receive a message.
	% The syntax is as follows:

		% receive
			% Pattern1 [when Guard1] ->
				% Expressions1;
			% Pattern2 [when Guard2] ->
				% Expressions2;
			% ....
		% after Time ->
			% Expressions
		% end

	% If no matching message has arrived within Time milliseconds of entering
	% the receive expression, then the processwill stop waiting for a
	% message and evaluate Expressions.
	.

	% Receive with Just a Timeout:
	receive_with_just_timeout() ->
		% You can write a receive consisting of only a timeout. Using this, we
		% can define a function sleep(T), which suspends the current process
		% for T milliseconds.

		% Look at lib_misc.erl sleep(T)
		.

	% Receive with Timeout Value of Zero:
	receive_with_timeout_zero() ->
		% A timeout value of 0 causes the body of the timeout to occur
		% immediately, but before this happens, the system tries to match any
		% patterns in the mailbox.

		% We can use this to define a function flush_buffer, which entirely
		% empties all messages in the mailbox of a process.

		% Look at lib_misc.erl flush_buffer()

		% Without the timeout clause, flush_buffer would suspend forever and
		% not return when the mailbox was empty. We can also use a zero timeout
		% to implement a form of "priority receive," as follows:

		% Look at lib_misc.erl priority_receive()

		% If there is not a message matching {alarm, X} in the mailbox, then
		% priority_receive will receive the first message in the mailbox. If
		% there is no message at all, it will suspend in the innermost receive
		% and return the first message it receives. If there is a message
		% matching {alarm, X}, then this message will be returned immediately.
		% Remember that the after section is checked only after pattern
		% matching has been performed on all the entries in the mailbox.

		% Without the after 0 statement, the alarm message would not be matched
		% first.

		% NOTE: Using large mailboxes with priority receive is rather
		% innefficient, so if you're going to use this technique, make sure
		% your mailboxes are not too large.
		.

	% Receive with Timeout Value of Infinity
	receive_with_timeout_infinity() ->
		% If the timeout value in a receive statement is the atom infinity,
		% then the timeout will never trigger. This might be useful for
		% programs where the timeout value is calculated outside the receive
		% statement. Sometimes the calculation might want to return an actual
		% timeout value, and other times it might want to have the receive wait
		% forever.
		.

	% Implementing a Timer:
	implementing_timer() ->
		% We can implement a simple timer using receive timeouts.

		% The function stimer:start(Time, Fun) will evaluate Fun (a function
		% of zero arguments) after Time ms. It returns a handle (which is a
		% PID), which can be used to cancel the timer if required.

		% Look at stimer.erl

		% Usage:

		% 1> Pid = stimer:start(5000, fun() -> io:format("timer event~n") end).
		% <0.42.0>
		% timer event
		% - This waits for for 5 seconds so that the timer would trigger.

		% 2> Pid1 = stimer:start(25000, fun() ->  io:format(timer event) end).
		% <0.49.0>
		% 3> stimer:cancel(Pid).
		% cancel

		% Timeouts and timers are central to the implementation of many
		% communication protocols. When we wait for a message, we don't want to
		% wait forever, so we add a timeout as in the examples.
		.


% 12.5 Selective Receive:
selective_receive() ->
	% The receive primitive is used to extract messages from the process
	% mailbox, but it does more than simple pattern matching; it also queues
	% unmatched message for later processing and manages timeouts. The
	% following statement:

		% receive
		% 	Pattern1 [when Guard1] ->
		% 		Expressions1; 
		% 	Pattern2 [when Guard2] ->
  	%     Expressions2;
		%   ...
		% after
  	%   Time ->
  	%     ExpressionsTimeout
		% end

	% Works as follows:
		% 1.

			% When we enter a receive statement, we start a timer (but only if
			% an after section is present in the expression)

		% 2.

			% Take the first message in the mailbox and try to match it against
			% Pattern1, Pattern2, and so on. If the match succeeds, the message
			% is removed from the mailbox, and the expressions following the
			% pattern are evaluated.

		% 3.

			% If none of the patterns in the receive statement matches the first
			% message in the mailbox, then the first message is removed from the
			% mailbox and put into a "save queue." The second message in the
			% mailbox is then tried. This procedure is repeated until a matching
			% message is found or until all the messages in the mailbox have been
			% examined.

		% 4.

			% If none of the messages in the mailbox matches, then the process is
			% suspended and will be rescheduled for execution the next time a new
			% message is put in the mailbox. When a new message arrives, the
			% messages in the save queue are not rematched; only the new message
			% is matched.

		% 5.

			% As soon as a message has been matched, then all messages that have
			% been put into the save queue are reentered into the mailbox in the
			% order in which they arrived at the process

		% 6.

			% If the timer elapses when we are waiting for a message, then
			% evaluate the expressions ExpressionsTimeout and put any saved
			% messages back into the mailbox in the order in which they arrived
			% at the process.
	.

% 12.6 Registered Processes:
registered_processes() ->
	% If we want to send a message to a process, then we need to know its
	% PID, but when a process is created, only the parent process knows the
	% PID. No other process in the system knows about the process. This is
	% often inconvenient since the PID has to be sent to all processes in the
	% system that want to communicate with this process. On the other hand,
	% it's very secure; if you don't reveal the PID of a process, other
	% processes can't interact with it in any way.

	% Erlang has a method for publishing a process identifier so that any
	% process in the System can communicate with this process. Such a process
	% is called a registered process. There are four BIFs for managing
	% registered processes.

		% register(AnAtom, Pid)
			% Register the processs Pid with the name AnAtom. The registration
			% fails if AnAtom has already been used to register a process.

		% unregister(AnAtom)
			% Remove any registrations associated with AnAtom.
			% NOTE: If a registered process dies, it will be automatically
			% unregistered.

		% whereis(AnAtom) -> Pid | undefined
			% Find out whether AnAtom is registered. Return the process
			% identifier Pid, or return the atom undefined if no process is
			% associated with AnAtom.

		% registered() -> [AnAtom::atom()]
			% Return a list of all registered processes in the system

	% Using register, we can revise the example in the code on page 183, and
	% we can try to register the name of the process that we created.

		% 1> Pid = spawn(area_server0, loop, []).
		% <0.51.0>
		% 2> register(area, Pid).
		% true

	% Once the name has been registered, we can send it a message like this:
		% 3> area ! {rectangle, 4, 5}.
		% Area of rectangle is 20
		% {rectangle,4,5}

	% We can use register to make a registered process that represents a
	% clock:

		% look at clock.erl
	.

% 12.7 A Word About Tail Recursion:
word_about_tail_recursion() ->
	% Take a look at the receive loop in the area server that we wrote
	% earlier:

		% area_server_final.erl

		% loop() ->
		% receive
		%         {From, {rectangle, Width, Ht}} ->
		%             From ! {self(), Width * Ht},
		%             loop();
		%         {From, {circle, R}} ->
		%             From !  {self(), 3.14159 * R * R},
		%             loop();
		%         {From, Other} ->
		%             From ! {self(), {error,Other}},
		%             loop()
		% end.

	% If you look carefully, you'll see that every time we receive a message,
	% we process the message and then immediately call loop() again. Such a
	% procedure is called tail-recursive. A tail-recursive function can be
	% compiled so that the last function call in a sequence of statements can
	% be replaced by a simple jump to the start of the function being called.
	% This means that a tail-recursive function can loop forever without
	% consuming stack space.

	% Suppose we wrote the following (incorrect) code:

		% loop() ->
		% 	receive
		% 	        {From, {rectangle, Width, Ht}} ->
		% 	            From ! {self(), Width * Ht},
		% 	            loop(),
		% 	            someOtherFunc();
		% 	        {From, {circle, R}} ->
		% 	            From !  {self(), 3.14159 * R * R},
		% 	            loop();
		% 	            ...
		% 	end
		% end
	.

% A concurrent program template:
concurrent_program_template() ->
	% When I (Joe) write a concurrent program, I almost always start with
	% something like this:

		% look at concurrent_template.erl

	% The receive loop is just any empty loop that receives and prints any
	% message that I send to it. As I develop the program, I'll start sending
	% messages to the processes.

	% Because I(Joe) start with no patterns in the receive loop that matches
	% these messages, I'll get a printout from the code at the bottom of the
	% receive statement. When this happens, I add a matching pattern to the
	% receive loop and rerun the program. This technique largely determines
	% the order in which I wrote the program: I start with a small program
	% and slowly grow it, testing it as I go along.

	% 	In line 5, we call loop(), but the compiler must reason that “after I’ve called loop(), I have to return to here, since I have to call someOtherFunc() in line 6.” So, it pushes the address of someOtherFunc onto the stack and jumps to the start of loop. The problem with this is that loop() never returns; instead, it just loops forever. So, each time we pass line 5, another return address gets pushed onto the control stack, and eventually the system runs out of space.
	% Avoiding this is easy; if you write a function F that never returns (such as loop()), make sure you never call anything after calling F, and don’t use F in a list or tuple constructor.
	.

% 12.8 Spawning with MFAs or Funs:
spawning_with_mfa_or_funs() ->
	% Spawning a function with an explicit module, function name, and
	% argument list (called an MFA) is the proper way to ensure that our
	% running processes will be correctly updated with new versions of the
	% module code if it is compiled while it is being used. The dynamic code
	% upgrade mechanism does not work with spawned funs. It works only with
	% explicitly named MFAs. For more details, read section 8.10, pg 122 
	% (Dynamic code loading).

	% If you don't care about dynamic code upgrade or you are certain that
	% your program will never be changed in the future, use the spawn(Fun)
	% form of spawn. If in doubt, use spawn(MFA).

	% That's it - you can now write concurrent programs.

	% Next we'll look at error recovery and see how we can write
	% fault-tolerant concurrent programs using three more concepts: links,
	% signals, and trapping process exits.
	.

% Look at exercises.erl
