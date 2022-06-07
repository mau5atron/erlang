% Creating an ETS Table

	% You create ETS tables by calling "ets:new". The process that creates
	% the table is called the owner of the table. When you create the table,
	% it has a set of options that cannot be changed. If the owner process
	% dies, space for the table is automatically deallocated. You can delete
	% the table by calling "ets:delete".

	% The arguments to ets:new are as follows:

		-spec ets:new(Name, [Opt]) -> TableId % returns TableId
			% Name: an atom
			% [Opt]: list of options, take from the following

				% set | ordered_set | bag | duplicate_bag
					% This creates an ETS table of the given type

				% private
					% This creates a private table. Only the owner process can read
					% and write this table.

				% public
					% This creates a public table. Any process that knows the table
					% identifier can read nad write this table.

				% protected
					% This creates a protected table. Any process that knows the
					% table identifier can read this table, but only the owner
					% process can write to the table.

				% named_table
					% If this is present, then Name can be used for subsequent table
					% operations

				% {keypos, K}
					% Use K as the key position. Normally position 1 is used for the
					% key. Probably the only time when we would use this option is if
					% we store an Erlang record (which is actually a disguised
					% tuple), where the first element of the record contains the
					% record name.

	% Note:
		% Opening an ETS table with zero options is the same as opening it with
		% the options [set, protected, {keypos, 1}]

	% All the code in this chapter uses protected ETS tables. Protected
	% tables are particularly useful since they allow data sharing at
	% virtually zero cost. All local processes that know the table identifier
	% can read the data, but only one process can change the data in the
	% table.

% -------------------------------------------------------------------------

% ETS Tables As Blackboards
	
	% Protected tables provide a type of "blackboard system." You can think
	% of a protected ETS table as a kind of named blackboard. Anybody who
	% knows the name of the blackboard can read the blackboard, but only the
	% owner can write on the blackboard.

	% Note:
		% An ETS table that has been opened in "public" mode can be written and
		% read by any process that knows the table name. In this case, the user
		% must ensure that reads and writes to the table are performed in a
		% consistent manner.
