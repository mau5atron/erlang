-module(chapter_notes).

% Part IV & Chapter 15: Programming Libraries and Frameworks

	% This part of the book covers the major libraries for programming with files, sockets, and databases. We also cover debugging techniques and the OTP framework.

% Chapter 15: Interfacing Techniques
interfacing_techniques() ->
	% Building systems often involves interfacing applications written in 
	% different programming languages with our system. We might use C for 
	% efficiency or writing low-level hardware drivers, or we might want to 
	% integrate a library written in Java or Ruby or some other programming 
	% language. We can interface foreign language problems to Erlang in a 
	% number of ways.

		% By running the programs outside the Erlang virtual machine in an
		% external operating system process. This is the safe way of doing
		% things. If the foreign language code is incorrect, it will not crash
		% the Erlang system. Erlang controls the exernal process through a
		% device called a port and communication channel. Erlang is responsible
		% for starting and stopping the external program and can monitor and
		% restart it if it crashes. The external process is called a port
		% process since it is controlled through an Erlang port.

		% By running an OS command from within Erlang and capturing the result.

		% By running the foreign language code inside the Erlang virtual
		% machine. This involves linking the code with the code for the Erlang
		% virtual machine. This is the unsafe way of doing things. Errors in
		% the foreign language code might crash the Eralng system. Although it
		% is unsafe, it is useful since it is more efficient than using an
		% external process.

			% Linking the code into the Erlang kernel can be used only for
			% languages like C taht produce native object code and can't be used
			% with languages like Java that have their own virtual machines.

	% In this chapter we'll look at interfacing Erlang usin ports and OS
	% commands. In addition, there are a number of advanced interfacing
	% techniques using the linked-in drivers, natively-implemented functions 
	% (NIFs), and C-nodes. The advanced techniques are not covered in the
	% book, but at the end of the chapter, there is a short overview of these
	% techiques and some pointers to reference material.
	.

% 15.1 How Erlang Communicates with External Programs.
erlang_communication_with_external_programs() ->
	% Erlang communicates with external programs through objects called
	% "ports". If we send a message to a port, the message will be sent to
	% the external program connected to the port. Messages from the external
	% program will appear as Erlang messages that come form the ports.

	% As far as the programmer is concerned, the port behaves just like an
	% Erlang process. You can send messages to it, you can register it (just
	% like a process), and so on. If the external program crashes, then an
	% exit signal will be sent to the connected process, and if the connected
	% process dies, then the external program will be killed.

	% Note the difference between using a port to communicate with an
	% external process and a socket. If you use a port, the port will behave
	% like an Erlang process, so you can link to it, send messages to it from
	% a remote distributed Erlang node, and so on. If you use a socket, it
	% will not behave like a process.

	% The process that creates a port is called the "connected process" for
	% that port. The connected process has a special significance: all
	% messages to the port must be tagged with the PID of the connected
	% process, and all messages from the external program are sent to the
	% connected processes.

	% We can see the relationship between a connected process (C), a port 
	% (P), and an external operating system processes in the following
	% figure:


                                                            
                                                            
	 	% ┌────────────────────────────┐           ┌──────────────┐
	 	% │ ERTS                       │           │              │
	 	% │                            │           │              │
	 	% │                            │           │              │
	 	% │  .───.  ◀──────────   .───.│◀───────── │  External    │
	 	% │ (  C  )              (  P  )           │  program     │
	 	% │  `───'  ──────────▶   `───'│─────────▶ │              │
	 	% │                            │           │              │
	 	% │                            │           │              │
	 	% └────────────────────────────┘           └──────────────┘

	 		% ERTS = Erlang runtime system
	 		% C = An Erlang Process that is connected to the port
	 		% P = A port

	% To create a port, we call open_port, which is specified as follows:

		% -spec open_port(PortName, [Opt]) -> Port
			% PortName is one of the following

				% {spawn, Command}
					% Start an external program. Command is the name of an external
					% program. Command runs outside the Erlang workspace unless a
					% linked-in driver with the name Command is found.

				% {fd, In, Out}
					% Allow an Erlang process to access any currently opened file
					% descriptors used by Erlang. The file descriptor "In" can be
					% used for standard input, and the file descriptor "Out" can be
					% used for standard output.

				% Opt is one of the following:
					% {packet, N}
						% Packets are preceded by an N (1, 2, or 4) byte length count.

					% stream
						% Messages are sent without packet lengths. The application
						% must know how to handle these packets/

					% {line, Max}
						% Deliver messages on a one-per line basis. If the line is more
						% than Max bytes, then it is split at Max bytes.

					% {cd, Dir}
						% Valid only for the {spawn, Command} option. The external
						% program starts in Dir.

					% {env, Env}
						% Valid only for the {spawn, Command} option. The environment
						% of the external program is extended with the environment
						% variables in the list Env. Env is a list of {VarName, Value}
						% pairs, where VarName and Value are strings.


	% This is not a complete list of the arguments to open_port. You can find
	% the precise details of the arguments in the manual page for the module
	% erlang.

	% The following messages can be sent to a port; note that in all of these
	% messages, PidC is the PID of the connected process

		% Port ! {PidC, {command, Data}}
			% Send data (an I/O list) to the port.

		% Port ! {PidC, {connect, Pid1}}
			% Change the PID of the connected process from PidC to Pid1

		% Port ! {PidC, close}
			% Close the port

	% The connected process can receive a message from the external program
	% by writing this:

		% receive
			% {Port, {data, Data}} ->
				% ... Data comes from the external process

	% In the following sections, we'll interface Erlang to a simple C
	% program. The C program is deliberately short so as not to distract from
	% the details of how we do the interfacing.
	.

% 15.2 Interfacing an External C program with a Port

interfacing_an_external_c_program_with_port() ->
	% We'll start with some simple C code. example1.c contains two functions.
	% The first function computes the sum of two integers, and the second
	% computes twice its argument.

	% Look at example1.c

	% Our final goal is to call these routines from Erlang. We'd like to be
	% able to call them as follows:

		% X1 = example1:sum(12,23),
		% Y1 = example1:twice(10),

	% As far as the user is concerned, example1 is an Erlang module, and
	% therefore all details of the interface to the C program should be
	% hidden inside the module example1.

	% To implement this, we need to turn function calls such as sum(12,23)
	% and twice(10) into sequences of bytes that we send to the external
	% program by means of the port. The port adds a length count to the byte
	% sequence and sends the result to the external program. When the
	% external program replies, the port receives the reply and sends the
	% result to the connected process for the port.

	% The protocol we use is very simple.

		% All packets start with a 2-byte length code (Len) followed by Len
		% bytes of data. This header is automatically added by the port when we
		% open it with argument {packet,2}.

		% We encode the call sum(N, M) as the byte sequence [1, N, M]

		% We encode the call twice(N) as the byte sequence [2,N].

		% Arguments and return values are assumed to be a single byte long.

	% Both the external C program and the Erlang program must follow this
	% protocol. The following figure illustrates what happens after we have
	% called example1:sum(12,23). It shows how the port is wired up to the
	% external C program.


		% ┌ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─               
		%                Erlang                               │              
		% │                                                                   
		%                                                     │    .─────.   
		% │{sum, 12,23}         [1,12,23]       [0,3,1,12,23]     ,'       `. 
		% ───────────▶ .─────. ────────▶ .───. ─────────────▶ │ ;     C     :
		% │            (Driver )         (port )                 :  Program  ;
		%     35       `─────'   [35]    `───'  [0,1,35]      │  ╲         ╱ 
		% │◀───────────         ◀────────       ◀─────────────     `.     ,'  
		%                                                     │     `───'    
		% │                                                                   
		% ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ┘              


	% What happens is as follows:
		% 1.

			% The driver encodes the sum(12,23) function into the byte sequence 
			% [1,12,23] and sends the {self(), {command, [1,12,23]}} message to
			% the port.

		% 2.

			% The port driver adds a 2-byte length header to the message and
			% sends the byte sequence 0,3,1,12,23 to the external program.

		% 3.

			% The external program reads these five bytes from standard input,
			% calls the sum function, and then writes the byte sequence 0,1,35 to
			% standard output.

			% The first two bytes contains the packet length. This is followed by
			% the result, 35, which is 1-byte long.

		% 4.

			% The port driver removes the length header and sends a {Port, 
			% {data, [35]}} message to the connected process.

		% 5.

			% The connected process decodes this message and returns the result
			% to the calling program.

	% We now have to write programs on both sides of the interface that
	% follow this protocol.
	.

	% The C Program:

	c_program() ->
		% The C program has three files.

			% example1.c
				% Contains the functions that we want to call (we saw this earlier)

			% example1_driver.c
				% Manages the byte stream protocol and calls the routines in
				% example1.c

			% erl_comm.c
				% Has routines for reading and writing memory buffers.
		.

	% example1_driver.c
	example1_driver_c() ->
		% This code has a loop that reads commands from standard input, calls
		% the application routines, and writes the results to standard output.
		% Note that if you want to debug this program, you can write to stderr;
		% there is a commented out fprintf statement in the code that shows how
		% to do this.

		% look at ports/example1_driver.c
		.

	% erl_comm.c
	erl_comm() ->
		% Finally, here's the code to read and write data to and from standard
		% input and output. The code is written to allow for possible
		% fragmentation of the data.

		% Look at ports/erl_comm.c

		% This code is specialized for handling data with a 2-byte length
		% header, so it matches up with the {packet, 2} option given to the
		% port driver program.
		.

	% The Erlang Program
	the_erlang_program() ->
		% The Erlang side of the port is driven by the following program:

		% Look at ports/example1.erl

		% This code follows a fairly standard pattern. In start/0, we create a
		% registered process (server) called example1. call_port/1 implements a
		% remote procedure call toward the server. twice/1 and sum/2 are
		% interface routines that must be exported and that make remote
		% procedure calls to the server. In loop/1, we encode the requests to
		% the external program.

		% That completes the programs. All we now need is a makefile to build
		% the programs.
		.

	% Compiling and Linking the Port Program
	compiling_and_linking_the_port_program() ->
		% This makefile compiles and links the port driver and linked-in driver
		% programs described in this chapter with all associated Erlang Code.
		% The makefile has been tested only on Mac OS X Mountain Lion and will
		% need modifying for other operating systems. It also includes a small
		% test program, which is run each time the code is rebuilt.

			% Look at Makefile.mac
 		.

 		% Running the program
 		running_makefile() ->
 			% Now we can run the program

 			% 1> example1:start().
 			% true
 			% 2> example1:sum(45, 32).
 			% 77
 			% 3> example1:twice(10).
 			% 20

 		% This completes our first example port program. The port protocol
 		% that the program implements is the principal way in which Erlang
 		% communicates with the external world.

 		% Before passing to the next topic, note the following:
 			% The example program made no attempt to unify Erlang's and C's
 			% ideas of what an integer is. We just assumed that an integer in
 			% Erlang and C was a single byte and ignored all problems of
 			% precision and signedness. In a realistic application, we would
 			% have to think rather carefully about the exact types and
 			% precisions of the arguments concerned. This can in fact be rather
 			% difficult, whereas langauges such as C have fixed ideas about the
 			% precision of integers and so on.

 			% We couldn't just run the Erlang functions without first having
 			% started the driver that was responsible for the interface (that
 			% is, some program had to evaluate example1:start() before we were
 			% able to run the program). We would like to be able to do this
 			% automatically when the system is started. This is perfectly
 			% possible but needs some knowledge of how the system starts and
 			% stops. We'll deal with this later in section 23.7, The Application
 			% on page 403.
 			.

% 15.3 Calling a Shell Script from Erlang:
calling_shell_script_from_erlang() ->
	% Suppose we want to call a shell script from Erlang. To do this, we can
	% use the library function os:cmd(Str). This runs the command in the
	% string Str and captures the result. Here's an example of using the
	% ifconfig command:

		% 1> os:cmd("ifconfig").
		% "lo0: flags=8049<UP,LOOPBACK,RUNNING,MULTICAST> mtu 16384\n\t...
	.

% 15.4 Advanced Interfacing Techniques:
advanced_interfacing_techniques() ->
	% In addition to the techniques discussed earlier, there are a few
	% additional techniques available for interfacing Erlang to external
	% programs.

	% The techniques described next are being continually improved and tend
	% to change with time more rapidly than Erlang itself. For this reason,
	% they are not described here in detail. The descriptions have been moved
	% into online archives so that they can be updated more quickly.


		% Linked-in Drivers:
			% These programs obey the same protocol as the port drivers discussed
			% earlier. The only difference is that the driver code is linked into
			% the Erlang kernel and thus runs inside the Erlang OS main process.
			% To make a linked-in driver, a small amount of code must be added to
			% initialize the driver, and the driver must be compiled and linked
			% with the Erlang VM.

			% git://github.com/erlang/linked_in_drivers.git
			% has up-to-date examples of linked-in drivers and how to compile
			% them for various operating systems.

		% NIFS:
			% NIFs are natively implemented functions. These are functions that
			% are written in C (or some language that compiles to native code)
			% and that are linked into the Erlang VM. NIFs pass arguments
			% directly onto the Erlang processes' stacks and heaps and have
			% direct access to all the Erlang internal data structure.

			% Examples of up-to-date information about NIFS are available from
			% git://github.com/erlang/nifs.git

		% C-Nodes:
			% C nodes are nodes implemented in C that obey the Erlang
			% distribution protocol. A "real" distributed Erlang node can talk to
			% a C-node and will think that the C-node is an Erlang node 
			% (provided it doesn't try to do anything fancy on the C-node like
			% sending it Erlang code to execute).

			% C-nodes are described interoperability tutorial at
			% http://www.erlang.org/ doc/tutorial/introduction.html.

	% S, nowo we know how to interface Erlang to the external world. In the
	% next couple of chapters, we'll see how to access files and sockets
	% from within Erlang.x
	.