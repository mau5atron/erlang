% 20.1 Creating The Initial Database

	% Before we can do anything, we have to create an Mnesia database. You
	% need to do this only once.

		% $ erl
		% 1> mnesia:create_schema([node()]).
		% ok

		% 2> init:stop().
		% ok
		% $ ls
		% Mnesia.nonode@nohost

	% mnesia.create_schema(NodeList) initiates a new Mnesia database on all
	% the nodes in NodeList (which must be a list of valid Erlang nodes). In
	% our case, we have the node list as [node()], that is, the current
	% node. Mnesia is initialized and creates a directory structure called
	% Mnesia.nonode@nohost to store the database.

% -------------------------------------------------------------------------

% Why is the DBMS Called Mnesia
	
	% The original name was Amnesia. One of our bosses didn't like the name.
	% He said, "You can't possibly call it Amnesia -- you can't have a
	% database that forgets things!" So, we dropped the A, and the name
	% stuck.

% -------------------------------------------------------------------------

	% Then we exit from the Erlang shell and issue the operating system's
	% "ls" command to verify this.

	% If we repeat the exercise with a distributed node called joe, we get
	% the following:

		% $ erl -name joe
		% (joe@doris.myerl.example.com) 1> mnesia.create_schema([node()]).
		% ok

		% $ (joe@doris.myerl.example.com) 2> init:stop().
		% ok
		% $ ls
		% Mnesia.joe@doris.myerl.example.com

	% Or we can point ot a specific database when we start Erlang.

		% $ erl -mnesia dir '"/home/joe/some/path/to/Mnesia.company"'
		% 1> mnesia:create_schema([node()]).
		% ok

		% 2> init:stop().
		% ok

	% /home/joe/some/path/to/Mnesia.company is the name of the directory in
	% which the database will be stored.


