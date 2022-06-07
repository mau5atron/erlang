% 19.6 What Haven't We Talked About?

	% ETS and DETS tables support a number of operations that we haven't
	% talked about in this chapter. These operations fall into the following
	% categories:

		% Fetching and deleting objects based on a pattern

		% Converting between ETS and DETS tables and between ETS tables and
		% disk files

		% Finding resource usage for a table

		% Traversing all elements in a table

		% Repairing a broken DETS table

		% Visualizing a table

	% More information about ETS and DETS is available online.
	
	% ETS and DETS tables were designed for efficient low-level in-memory and
	% disk storage of Erlang terms, but this is not the end of the story. For
	% more sophisticated data storage, we need a database.

	% In the next chapter, we'll introduce Mnesia, which is a real-time
	% database written in Erlang and which is part of the standard Erlang
	% distribution. Mnesia uses ETS and DETS tables internally, and a lot of
	% the routines exported from the "ets" and "dets" modules are intended
	% for internal use from Mnesia. Mnesia can do all kinds of operations
	% that are not possible using single ETS and DETS tables. For example, we
	% can index on more than the primary key, so the kind of double insertion
	% trick that wee used in the filename2indedx example is not necessary.
	% Mnesia will actually create several ETS or DETS tables to do this, but
	% this is hidden from the user.