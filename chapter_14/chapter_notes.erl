-module(chapter_notes).
-export([]).

% Chapter 14: Distributed Programming

% Writing distributed programs in Erlang is only a small step from writing
% concurrent programs. In distributed Erlang, we can spawn processes on
% remote nodes and machines. Having spawned a remote process, we'll see
% that all the other primitives, send, receive, link, and so on, work
% transparently over a network in the same way as they worked on a single
% node.

% In this chapter, we'll introduce the libraries and Erlang primitives that
% we'll use to write distributed Erlang programs. Distributed programs are
% programs that are designed to run on networks of computers and that can
% coordinate their activities only be message passing.

% Here are some reasons we might want to write distributed applications:
	
	% Performance
		% We can make our programs go faster by arranging that different parts
		% of the program are run in parallel on different machines.

	% Reliability
		% We can make fault-tolerant systems by structuring the system to run
		% on several machines. If one machine fails, we can continue on another
		% machine.

	% Scalability
		% As we scale up an application, sooner or later we will exhaust the
		% capabilities of even the most powerful machine. At this stage, we
		% have to add more machines to add capacity. Adding a new machine
		% should be a simple operation that doesn't require large changes to
		% the application architecture.

	% Intriniscally distributed application
		% Many applications are inherently distributed. If we write a multiuser
		% game or chat system, different users will be scattered all over the
		% globe. If we have a large number of users in a particular geographic
		% location, we want to place the computation resources near the users.

	% Fun
		% Most of the fun programs that I want to write are distributed. Many
		% of these involve interaction with people and machines all over the
		% world.

% 4.1 Two Models for Distribution:
two_models_for_distribution() ->
	% In this book we'll talk about two main models of distribution.

		% Distributed Erlang:

			% In distributed Erlang, programs are written to run on Erlang nodes.
			% A node is a self-contained Erlang system containing a complete
			% virtual machine with its own address space and own set of
			% processes.

			% We can spawn a process on any node, and all the message passing and
			% error handling primitives we talked about in previous chapters work
			% as in the single node case.

			% Distributed Erlang applications run in a trusted environment --
			% since any node can perform any operation on any other Erlang node,
			% a high degree of trust is involved. Typically distributed Erlang
			% applications will be run on clusters on the same LAN and behind a
			% firewall, though they can run in an open network.

		% Socket-based distribution:
			% Using TCP/IP sockets, we can write distributed applications that
			% can run in an untrusted environment. The programming model is less
			% powerful than that use in distributed Erlang but more secure. In
			% section 14.6, Socket-based distribution, pg 224, we'll see how to
			% make applications using a simple socket-based distribution
			% mechanism

	% If you think back to the previous chapters, you'll recall that the
	% basic unit that we construct programs from is the process. Writing a
	% distribute Erlang program is easy; all we have to do is spawn our
	% processes on the correct machines, and then everything works as before.

	% We are all used to writing sequential programs. Writing distributed
	% programs is usually a lot more difficult. In this chapter, we'll look
	% at a number of techniques for writing simple distributed programs. Even
	% thoguh the programs are simple, they are very useful.

	% We'll start with a number of small examples. To do this, we'll need to
	% learn only two things; then we can make our first distributed program.
	% We'll learn how to start an Erlang node and how to perform a remote
	% procedure call on a remote Erlang node.
	.

% 14.2 Writing a Distributed Program
writing_a_distributed_program() ->
	% When I develop a distributed application, I always work on the program
	% in a specific order, which is as follows:

		% 1.
			% I write and test my program in a regular nondistributed Erlang
			% session. This is what we've been doing up to now, so it presents no
			% new challenges.

		% 2.
			% I test my program on two different Erlang nodes running on the same
			% computer.

		% 3.
			% I test my program on two different Erlang nodes running on two
			% physically separated computers either in the same local area
			% network or anywhere on the internet.

	% The final step can be problematic. If we run on machines within the
	% same administrative domain, this is rarely a problem. But when the
	% nodes involved belong to machines in different domains, we can run into
	% problems with connectivity, and we have to ensure that our system
	% firewalls and security settings are correctly configured.

	% To illustrate these steps, we'll make a simple name server.
	% Specifically, we will do the following:

		% Stage 1: Write and test the name server in a regular undistributed
		% Erlang system.

		% Stage 2: Test the name server on two nodes on the same machine.

		% Stage 3: Test the name server on two different nodes on two different
		% machines on the same local area network.

		% Stage 4: Test the name server on two different machines belonging to
		% two different domains in two different countries.
	.

% 4.3 Building the Name Server:
building_the_name_server() ->
	% A name server is a program that, given a name, returns a value
	% associated with that name. We can also change the value associated with
	% a particular name.

	% Our first name server is extremely simple. It is not fault tolerant, so
	% all the data it stores will be lost if it crashes. The point of this
	% exercise is not to make a fault-tolerant name server but to get started
	% with distributed programming techniques.

	% Stage 1: A simple Name Server
		% Our name server kvs is a simple Key -> Value, server. It has the
		% following interface:

			% -spec kvs:start() -> true
				% Start the server; this creates a server with the registered name
				% kvs

			% -spec kvs:store(Key,Value) -> true
				% Associate Key with Value

			% -spec kvs:lookup(Key) -> {ok,Value} | undefined
				% Look up the value of Key, and return {ok,Value} if there is a
				% value associated with Key; otherwise, return undefined.

	% The key-value server is implemented using the process dictionary get
	% and put primitives, as follows:

		% look at socket_dist/kvs.erl

	% Store messages are sent in store(Key,Value). The main server starts in
	% the loop function. The loop function receives and waits for a store or
	% lookup message ad then just saves or retrieves the requested data from
	% the local process dictionary and sends a reply back to the client.
	% We'll start by testing the server locally to see that it works
	% correctly:

	% Usage:

		% 1> kvs:start().
		% true
		% 2> kvs:store({location, joe}, "stockholm").
		% true
		% 3> kvs:store(weather, raining).
		% true
		% 4> kvs:lookup(weather).
		% {ok, raining}
		% 5> kvs:lookup({location, joe}).
		% {ok, "Stockholm"}
		% 6> kvs:lookup({location, jane}).
		% undefined

	% So far, we get no unpleasant surprises. On to step 2. Let's distribute
	% the application.
	.

	% Stage 1: A simple Name Server
	stage_1_simple_name_server() ->
		% Our name server kvs is a simple Key -> Value, server. It has the
		% following interface:

			% -spec kvs:start() -> true
				% Start the server; this creates a server with the registered name
				% kvs

			% -spec kvs:store(Key,Value) -> true
				% Associate Key with Value

			% -spec kvs:lookup(Key) -> {ok,Value} | undefined
				% Look up the value of Key, and return {ok,Value} if there is a
				% value associated with Key; otherwise, return undefined.

		% The key-value server is implemented using the process dictionary get
		% and put primitives, as follows:

			% look at socket_dist/kvs.erl

		% Store messages are sent in store(Key,Value). The main server starts
		% in the loop function. The loop function receives and waits for a
		% store or lookup message ad then just saves or retrieves the requested
		% data from the local process dictionary and sends a reply back to the
		% client.

		% We'll start by testing the server locally to see that it works
		% correctly:

		% Usage:

			% 1> kvs:start().
			% true
			% 2> kvs:store({location, joe}, "stockholm").
			% true
			% 3> kvs:store(weather, raining).
			% true
			% 4> kvs:lookup(weather).
			% {ok, raining}
			% 5> kvs:lookup({location, joe}).
			% {ok, "Stockholm"}
			% 6> kvs:lookup({location, jane}).
			% undefined

		% So far, we get no unpleasant surprises. On to step 2. Let's
		% distribute the application.
		.

	% Stage 2: Client on One Node, Server on Second Node but Same Host
	stage_2_client_on_one_node_server_on_second_but_same_host() ->
		% Now we'll start two Erlang nodes on the same computer. to do this, we
		% need to open two terminal windows and start two Erlang systems.

		% First, we fire up a terminal shell and start a distributed Erlang
		% node in this shell called gandalf; then, we start the server:

			% $ erl -sname gandalf
			% (gandalf@localhost) 1> kvs:start().
			% true

		% The argument -sname gandalf means "start an Erlang node with name
		% gandalf on the local host." Note how the Erlang shell prints the name
		% of the Erlang node before the command prompt. The node name is of the
		% form Name@Host. Name and Host are both atoms, so they will have to be
		% quoted if they contain any nonatomic characters.

		% Important Note: If you run the previous command on your system, the
		% node name might not be gandalf@localhost. It might be gandalf@H where
		% H is your local hostname. This will depend upon how your system is
		% configured. If this is the case, then you'll have to use the name H
		% instead of localhost in all the examples that follow.

		% Next we start a second terminal session and start an Erlang node
		% called bilbo. 


		% Then we can call the functions in kvs using the library module rpc().
		% (Note that rpc is a standard Erlang library module, which is not the
		% same as rpc function we wrote earlier within kvs:rpc())

			% $ erl -sname bilbo

			% (bilbo@localhost) 1> rpc:call(gandalf@localhost, kvs, store, 
			% [weather, fine].
			% true

			% (bilbo@localhost) 2> rpc:call(gandalf@localhost, kvs, lookup, 
			% [weather]).
			% {ok, fine}

		% Now it may not look like it, but we've actually performed our
		% first-ever distributed computation! The server ran on the first node
		% that we started, and the client ran on the second node.

		% The call to set the value of weather was made on the bilbo node; we
		% can swap back to gandalf and check the value of the weather.

			% (gandalf@localhost) 2> kvs:lookup(weather).
			% {ok, fine}

		% rpc:call(Node, Mod, Func, [Arg1, Arg2, ArgN]) performs a remote
		% procedure call on Node. The function to be called is Mod:Func(Arg1,
		% Arg2, ArgN).

		% As we can see, the program works as in the nondistributed Erlang
		% case; now the only difference is that the client is running on one
		% node and the server is running on a different node.

		% The next step is to run the client and the server on different
		% machines.
		.

	% Stage 3: Client and Server on Different Machines on the Same LAN
	client_server_on_different_machines_same_lan() ->
		% We're going to use two nodes. The first node is called gandalf on
		% doris.myerl.example.com, and the second is called bilbo on
		% george.myerl.example.com.

		% Before we do this, we start two terminals using something like SSH or
		% VNC on the two different machines. We'll call these two windows doris
		% and george.

		% Step 1 is to start an Erlang node on doris:

			% doris $ erl -name gandalf -setcookie abc
			% (gandalf@doris.myerl.example.com) 1> kvs:start().
			% true

		% Step 2 is to start an Erlang node on george and send some commands to
		% gandalf.

			% george $ erl -name bilbo -setcookie abc
			% (bilbo@george.myerl.example.com) 1> rpc:call
			% (gandalf@doris.myerl.example.com, kvs, store, [weather, cold])

			% true

			% (bilbo@george.myerl.example.com) 2> rpc:call
			% (gandalf@doris.myerl.example.com, kvs, lookup, [weather]).
			% {ok, cold}


		% Things behave exactly as in the case with two different nodes on the
		% same machine.

		% Now for this to work, things are slightly more complicated that in
		% the case where we ran two nodes on the same computer. We have to take
		% four steps.

			% 1. 
				% Start Erlang with the -name parameter. When we have two nodes on
				% the same machine, we use "short" names (as indicated by the
				% -sname % flag), but if they are on different networks, we use
				% -name

				% We can also use -sname on two different machines when they are on
				% the same subnet. Using -sname is also the only method that will
				% work if no DNS service is available.

			% 2.
				% Ensure that both nodes have the same cookie. This is why both
				% nodes were started with the command-line argument -setcookie abc.
				% We'll talk more about cookies later in the chapter (section 14.5
				%, the Cookie Protection System, page 222)

				% NOTE: when we ran two nodes on the same machine, both nodes could
				% access the same cookie file, $HOME/.erlang.cookie, which is why
				% we didn't have to add the cookie to the Erlang comomand line.

			% 3.
				% Make sure the fully qualified hostnames of the nodes concerned
				% are resolved by DNS. In my case, the domain name
				% myerl.example.com is purely local to my home network and is
				% resolved locally by adding an entry to /etc/hosts.

			% 4.
				% Make sure that both systems have the same version of the code and
				% the same version of Erlang. If you don't do this, you might get
				% serious and mysterious errors. The easiest way to avoid problems
				% is to have the same versions of Erlang running everywhere.
				% Different versions of Erlang can run together, but there is no
				% guarantee that this will work, so it's a good idea to check. In
				% our case, the same version of the code for kvs has to be
				% available on both systems. There are several ways of doing this.

					% In my home setup (Joe), I have two physically separated
					% computers with no shared file systems; here I physically copy
					% kvs.erl to both machines and compile it before starting the
					% programs.

					% On my work computer we use workstations with a shared NFS disk.
					% Here I merely start Erlang in the shared directory form two
					% different workstations.

					% Configure the code server to do this. I won't describe how to
					% do this here. Take a look at the manual page for the module
					% erl_prim_loader.

					% Use the shell command nl(Mod). This loads the module Mod on all
					% connected nodes.
						% NOTE: For this to work, you have to make sure that all the
						% nodes are connected. Nodes become connected when they try to
						% access each other. This happens the first time you evaluate
						% any expression involving a remote node. The easiest way to do
						% this is to evaluate net_adm:ping(Node)(see the manual page
						% for net_adm for more details).


		% Success! We are running on two servers, on the same local area
		% network. The next step is to move these two computers connected
		% through the internet.
		.

	% Stage 4: Client and Server on Different Hosts in the Internet
	client_server_different_hosts_in_internet() ->
		% In principle, this is the same as in stage 3, but now we have to be
		% much more concerned with security. When we run two nodes on the same
		% LAN, we probably don't have to worry too much about security. In most
		% organizations, the LAN is isolated fro mthe internet by a firewall.
		% Behind the firewall we are free to allocate IP addresses in a
		% haphazard manner and generally misconfigure our machines.

		% When we connect several machines in an Erlang cluster on the
		% internet, we can expect to run into problems with firewalls that do
		% not permit incoming connections. We will have to correctly configure
		% our firewalls to accept incoming connections. There is no way to do
		% this in a generic manner, since every firewall is different.

		% To prepare your system for distributed Erlang, you will have to take
		% the following steps:

			% 1.

				% Make sure that port 4369 is open for both TCP and UDP traffic.
				% this port is used by a program called "epmd" (short for Erlang
				% Port Mapper Daemon).

			% 2.

				% Choose a port or range of ports to be used for distributed
				% Erlang, and make sure these ports are open. If these ports are
				% Min and Max (use Min = Max if you want to use only one port),
				% then start Erlang with the following command:

					% $ erl -name ... -setcookie ... -kernel inet_dist_listen_min
					% inet_dist_listen_max Max

		% We've now seen how to run programs on sets of Erlang nodes and how to
		% run them on the same local area network or over the internet. Next
		% we'll look at primitives that deal with nodes.
		.

% 14.4 Libraries and BIFS for Distributed Programming:
libraries_and_bifs_for_distributed_programming() ->
	% When we write distributed programs, we very rarely start from scratch.
	% In the standard libraries, there are a number of modules that can be
	% used to write distributed programs. These modules are written using
	% distribution BIFs, but they hide a lot of the complexity from the
	% programmer.

	% Two modules in the standard distribution cover most needs.

		% "rpc" provides a number of remote procedure call services.

		% "global" has functions for the registration of names and locks in a
		% distributed system and for the maintenance of a fully connected
		% network.

	% The single most useful function in the module rpc is the following:

		% call(Node, Mod, Func, Args) -> Result | {badrpc, Reason}
			% This evaluates apply(Mod, Function, Args) on Node and returns the
			% result Result or {badrpc, Reason} if the call fails.

	% The primitives that are used for writing distributed programs are as
	% follows:

		% -spec spawn(Node, Fun) -> Pid
			% This works exactly like spawn(Fun), but the new process is spawned
			% on Node

		% -spec spawn(Node, Mod, Func, ArgList) -> Pid
			% This works exactly like spawn(Mod, Func, ArgList), but the new
			% process is spawned on Node. spawn(Mod, Func, Args) creates a new
			% process that evaluates apply(Mod, Func, Args). It returns the PID
			% of the new process.

			% NOTE: This form of spawn is more robust than spawn(Node, Fun).
			% spawn(Node, Fun) can break when the distributed nodes are not
			% running exactly the same version of a particular module.

		% -spec spawn_link(Node, Fun) -> Pid
			% This works exactly like spawn_link(Fun), but the new process is
			% spawned on Node.

		% -spec spawn_link(Node, Mod, Func, ArgList) -> Pid
			% This works like spawn(Node, Mod, Func, ArgList), but the new
			% process is linked to the current process.

		% -spec disconnect_node(Node) -> bool() | ignored
			% This forcibly disconnects a node.

		% -spec monitor_node(Node, Flag) -> true
			% If Flag is true, monitoring is turned on; if Flag is fals,
			% monitoring is turned off. If monitoring has been turned on, then
			% the process that evaluated this BIF will be sent {nodeup, Node} and
			% {nodedown, Node} messages if Node joins or leaves the set of
			% connected Erlang nodes.

		% -spec node() -> Node
			% This returns the name of the local node. nonode@nohost is returned
			% if the node is not distributed.

		% -spec node(Arg) -> Node
			% This returns the node where Arg is located. Arg can be a PID, a
			% reference, or a port. If the local node is not distributed,
			% nonode@nohost is returned.

		% -spec nodes() -> [Node]
			% This returns a list of all other nodes in the network to which we
			% are connected.

		% -spec is_alive() -> bool()
			% This returns true if the local node is alive and can be part of a
			% distributed system. Otherwise, it returns false.

	% In addition, send can be used to send messages to a locally registered
	% process in a set of distributed Erlang nodes. The following syntax:

		% {RegName, Node} ! Msg
			% Sends the message Msg to the registered process RegName on the node
			% Node.
	.

	% An Example of Remote Spawning
	example_of_remote_spawning() ->
		% As a simple example, we can show how to spawn a process on a remote
		% node.

			% look at dist_demo.erl

		% Then we start two nodes; both nodes have to be able to load this
		% code. If both nodes are on the same host, then this is not a problem.
		% We merely start two Erlang nodes from the same dirctory.

		% If the nodes are on two physically separated nodes with different
		% file systems, then the program must be copied to all nodes and
		% compiled before starting both the nodes (alternatively, the .beam
		% file can be copied to all nodes). In the example, I'll asume we've
		% done this.

			% On the host doris, we start a node named gandalf
				% doris $ erl -name gandalf -setcookie abc
				% (gandalf@doris.myerl.example.com) 1>

			% And on the host george, we start a node named bilbo, remembering to
			% use the same cookie.

				% george $ erl -name bilbo -setcookie abc
				% (bilbo@george.myerl.example.com) 1> Pid = dist_demo:start
				% ('gandalf@doris.myerl.example.com').
				% <5094.40.0>

			% Pid is now a process identifier of the process on the remote node,
			% and we can call dist_demo:rpc/4 to perform a remote procedure call
			% on the remote node.

				% (bilbo@george.myerl.example.com) 2> dist_demo:rpc(Pid, erlang,
				% node, []).
				% 'gandalf@doris.myerl.example.com'

			% This evaluates erlang:node() on the remote node and returns the
			% value.
		.

	% The File Server Revisited:
	file_server_revisited() ->
		% In The File Server Process, pg 15, we built a simple file server with
		% the promise that we would return to it later. Well, now is later. The
		% previous section in this chapter showed how to set up a simple remote
		% procedure call server, which we can use to transfer files between two
		% Erlang nodes.

		% The following continues the example of the previous section:

			% (bilbo@george.myerl.example.com) 1> Pid =
			%     dist_demo:start('gandalf@doris.myerl.example.com').
			% <6790.42.0>

			% (bilbo@george.myerl.example.com) 2>
			%     dist_demo:rpc(Pid, file, get_cwd, []).
			% {ok,"/home/joe/projects/book/jaerlang2/Book/code"}

			% (bilbo@george.myerl.example.com) 3>
			%     dist_demo:rpc(Pid, file, list_dir, ["."]).
			% {ok,["adapter_db1.erl","processes.erl",
			%      "counter.beam","attrs.erl","lib_find.erl",...]}

			% (bilbo@george.myerl.example.com) 4>
			%     dist_demo:rpc(Pid, file, read_file, ["dist_demo.erl"]).
			% {ok,<<"-module(dist_demo).\n-export([rpc/4, start/1]).\n\n...>>}
		
		% On gandalf I started a distributed Erlang node in the code directory
		% where I store the code examples for this book. On bilbo I'm maing
		% requests that result in remote procedure calls to the standard
		% libraries on gandalf. I'm using three functions in the file module to
		% access the file system on gandalf. file:get_cwd() returns the current
		% working directory of the file server, file:list_dir(Dir) returns a
		% list of the files in Dir, and file:read_file(File) reads the file
		% File.

		% If you reflect for a moment, you'll realize what we've just done is
		% pretty amazing. We've made a file server without writing any code;
		% we've just reused the library code in the module file and made it
		% available through a simple remote procedure call interface.


		% Implementing a File Transfer Program
			% A few years ago I had to transfer a number of files between two
			% networked machines with different operating systems. My first
			% thought was to use FTP, but I needed an FTP server on one machine
			% and an FTP client on the other. I couldn't find an FTP server for
			% my server machine, and I didn't have root priviledges
		.


% 14.5 The Cookie Protection System:
cookie_protection_system() ->
	% Access to a single node or set of nodes is secured by a cookie system.
	% Each node has a single cookie, and this cookie must be the same as the
	% cookies of any nodes to which the node talks. To ensure this, all nodes
	% in a distributed Erlang system must have been started with the same
	% magic cookie orhave their cookie changed to the same value by
	% evaluating erlang:set_cookie().

	% The set of connected nodes having the same cookie defines an Erlang
	% cluster.

	% For two distributed Erlang nodes to communicate, they must have the
	% same magic cookie. We can set the cookie in three ways:

		% Important: the cookie protection system was designed for building
		% distributed systems that run on a local area network (LAN) where the
		% LAN itself was protected from the internet by a firewall. Distributed
		% Erlang applications running across the internet should first setup
		% secure connections between hosts and then use the cookie protection
		% system.


			% Method 1: 
				% Store the same cookie in the file $HOME/.erlang.cookie.
				% This file contains a random string and is automatically created
				% the first time Erlang is run on your machine.

				% This file file can be copied to all machiens that we want to
				% participate in a distributed Erlang session. Alternatively, we
				% can explicitly set the value.

				% For example, on a linux system, we could give the following
				% commands:

					% $ cd
					% $ cat > .erlang.cookie
					% AFRTY12ESS3412735ASDF12378
					% chmod 400 .erlang.cookie

			% Method 2:
				% When Erlang is started, we can use the command-line argument
				% "-setcookie C" to set the magic cookie to "C". An example:

					% $ erl -setcookie AFRTY12ESS3412735ASDF12378

			% Method 3:
				% The BIF erlang:set_cookie(node(), C) sets the cookie of the local
				% node to the atom C.

		% Note:

			% If your environment is insecure, then method 1 or 3 is better than
			% method 2 since on a Unix system anybody can discover your cookie
			% using the ps command. Method 2 is useful only for testing.

			% In case you're wondering, cookies are never sent across the network
			% in the clear. Cookies are used only forthe initial authentication
			% of a session. Distributed Erlang sessions are not encrypted but can
			% be setup to run over encrypted channels. (Google the Erlang
			% mailing list for up-to-date information on this.)

		% Up to now we have looked at how to write distributed programs using
		% Erlang nodes and the distribution primitives. As alternative, we can
		% write distributed programs on top of a raw socket interface.
	.

% 14.6 Socket-Based Distribution:
socket_based_distribution() ->
	% In this seciton, we will write a simple program using socket-based
	% distribution. As we have seen, distributed Erlang is fine for writing
	% cluster applications where you can trust everybody involved but is less
	% suitable in an open environment where not everyone can be trusted.

	% The main problem with distributed Erlang is that the client can decide
	% to spawn any process on the server machine. So, to destroy your system,
	% all you'd have to do is evaluate the following:

		% rpc:multicall(nodes(), os, cmd, ["cd /; rm -rf *"])

	% Distributed Erlang is useful in the situation where you own all the
	% machines and want to control all of them from a single machine. But
	% this model of computation is not suited to the situation where
	% different people own the individual machines and want to control
	% exactly which software can be executed on their machines.

	% In these circumstances, we will use a restricted form of spawn where
	% the owner of a particular machine has explicit control over what gets
	% run on their machines.
	.

		% Controlling Processes with lib_chan
		controlling_processes_lib_chan() ->
			% lib_chan is a module that allows a user to explicitly control which
			% processes are spawned on their machines. The implementation of
			% lib_chan is rather complex, so I've taken it out of the normal
			% chapter flow; you can find it in Appendix 2, a socket application,
			% page 477.

			% The interface is as follows:

			% -spec start_server() -> true
				% This starts a server on the local host. The behaviour of the
				% server is determined by the file
				% $HOME/.erlang_config/lib_chan.conf

			% -spec start_server(Conf) -> true
				% This starts a server on the local host. The behavior of the
				% server is determined by the file Conf, which contains a list of
				% tuples of the following form:

					% {port, NNNN}
						% This starts listening to port number NNNN

					% {service, S, password, P, mfa, SomeMod, SomeFunc, SomeArgsS}
						% This defines a service S protected by password P. If the
						% service is started, then a process is created by spawning
						% SomeMod:SomeFunc(MM, ArgsC, SomeArgsS) to handle messages
						% from the client. Here MM is the PID of a proxy process that
						% can be used to send messages to the client, and the argument
						% ArgsC comes from the client connect call.

			% -spec connect(Host, Port, S, P, ArgsC) -> {ok, Pid} | {error, Why}
				% Try to open the port Port on the host Host, and then try to
				% activate the service S, which is protected with the password P.
				% If the password is correct, {ok, Pid} will be returned, where Pid
				% will be the process identifier of a proxy process thatn can be
				% usede to send messages to the server.

			% When a connection is established by the client calling connect/5,
			% two proxy processes are spawned: one on the client side and the
			% other on the server side. These proxy processes handle the
			% conversion of Erlang messages to TCP packet data, trapping exits
			% from the controlling processes, and socket closure.

			% This explanation might look complicated, but it will become a lot
			% clearer when weuse it. The following is a complete example of how
			% to use lib_chan together with the kvs service that we described
			% earlier.
			.

		% The Server Code:
		the_server_code() ->
			% First we write a configuration file.

				% {port, 1234}.
				% {service, nameServer, password, "ABXy45", mfa, mod_name_server,
				% start_me_up,  notUsed}.

			% This means we are going to offer a service called nameServer on
			% port 1234 of our machine. The service is protected by the password
			% ABXy45.

			% When a connection is created by the client calling the following:

				% connect(Host, 1234, nameServer, "ABXy45", nil)

			% the server will spawn mod_name_server:start_me_up(MM, nil,
			% notUsed). MM is the PID of a proxy process that is used to talk to
			% the client.

			% Important:

				% At this statge, you should study the previous line of code and
				% make sure you see where the arguments in the call come from.

					% mod_name_server, start_me_up, and notUsed come from the
					% configuration file.

					% nil is the last argument in the connect call.

			% mod_name_server is as follows:
				% look at socket_dist/mod_name_server.erl

			% mod_name_server follows this protocol:

				% If the client sends the server a message {send, X}, it will
				% appear in mod_name_server as a message of the form {chan, MM, X}
				% (MM is the PID of the server proxy process)

				% If the client terminates or the socket used in communication
				% closes for any reason, then a message of the form {chan_closed,
				% MM} will be received by the server.

				% If the server wants to explicitly close the connection, it can do
				% so by evaluating MM ! close.

			% This protocol is the middle-man protocol that is obeyed by both the
			% client code and the server code. The socket middle-man code is
			% explained in more detail in lib_chan_mm: The middle man, on page
			% 480.

			% To test this code, we will first make sure that everything works on
			% one machine.

			% Now we can start the name server (and the module kvs).

				% kvs:start().
				% true

				% lib_chan:start_server().
				% Starting a port server on 1234...
				% true

			% Now we can start a second Erlang session and test this from any
			% client.

				% 1> {ok, Pid} = lib_chan:connect("localhost", 1234, nameServer,
				% "ABXy45", "")

				% 2> lib_chan:cast(Pid, {store, joe, "writing a book"}).
				% {send, {store, joe, "writing a book"}}.

				% 3> lib_chan:rpc(Pid, {lookup, joe}).
				% {ok, "writing a book"}

				% 4> lib_chan:rpc(Pid, {lookup, jim}).
				% undefined


			% Having testing that this works on one machine, we go through the
			% same steps we described earlier and perform similar tests on two
			% physically separated machines.

			% Note that in this case, it is the owner of the remote machine who
			% decides the contents of the configuration file. The configuration
			% file specifies which applications are permitted on this machine and
			% which port is to be used to communicate with these applications.

			% We're now at the point where we can write distributed programs. A
			% whole new world opens up. If writign sequential programs is fun,
			% then writing distributed program is fun squared or fun cubed. I
			% really recommend you do the following YAFS exercise; this basic
			% code structure is central to many applications.

			% We have now covered sequential, concurrent, and distributed
			% programming. In the next part of the book, we'll look at how to
			% interface foreign language code, and we'll look at some of the
			% major Erlang libraries and how to debug code. Then we'll see how
			% complex Erlang systems can be built using the OTP structuring
			% principles and libraries.
			.