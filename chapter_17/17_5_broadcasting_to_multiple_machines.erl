-module(17_5_broadcasting_to_mutliple_machines).

% 17.5 Broadcasting to Multiple Machines

	% Finally we'll see how to setup a broadcast channel. This code is 
	% simple.
	
		% Look at broadcast.erl

	% Here we need two ports, one to send the broadcast and the other to
	% listen for answers. We've chosen port 5010 to send the broadcast
	% request and 6000 to listen for broadcasts (these two numbers have no
	% significance; just choose two free ports on your system)

	% Only the process performing a broadcast opens port 5010, but all
	% machines in the network call broadcast:listen(), which opens port 6000
	% and listens for broadcast messages.

	% broadcast:send(IoList) broadcasts IoList to all machines on the local
	% area network.

	% Note: For this to work, the name of the interface must be correct, and
	% broadcasting must be supported. On my iMac, for example, I use the name
	% "en0" instead of "eth0." Note also that if hosts running UDP listeners
	% are on different network subnets, the UDP broadcasts are unlikely to
	% reach them, because by default routers drop such UDP broadcasts