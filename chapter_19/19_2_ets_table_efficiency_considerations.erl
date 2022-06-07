% 19.2 ETS Table Efficiency Considerations

	% Internally, ETS tables are represented by hash tables (except ordered
	% sets, which are represented by balanced binary trees). This means there
	% is a slight space penalty for using sets and a time penalty for using
	% ordered sets. Inserting into sets takes palce in constant time, but
	% inserting into an ordered seet takes place in a time proportional to
	% the log of the number of entries in the table.

	% When you choose between a set and an ordered set, you should think
	% about what you want to do with the table after it has been constructed
	% --- if you want a sorted table, then use an ordered set.

	% Bags are more expensive to use than duplicate bags, since on each
	% insertion all elements with the same key have to be compared for
	% equality. If there are large numbers of tuples with the same key, this
	% can be rather inefficient.

	% ETS tables are stored in a separate storage area that is not associated
	% with normal process memory. An ETS table is said to be owned by the
	% process that created it --- when that process dies or when ets:delete
	% is called, then the table is deleted. ETS tables are not garbage
	% collected, which means that large amounts of data can be stored in the
	% table without incurring garbage collection penalties.

	% When a tuple is inserted into an ETS table, all the data structures
	% representing the tuple are copied from the process stack and heap into
	% the ETS table. When a lookup operation is performed on a table, the
	% resultant tuples are copied from the ETS table to the stack and heap of
	% the process.

	% This is true for all data structures except large binaries. Large
	% binaries are stored in their own off-heap storage area. This area can
	% be shared by several processes and ETS tables, and the individual
	% binaries are managed with a reference-counting garbage collector that
	% keeps track of how many different processes and ETS tables use the
	% binary. When the use coutn for the number of processes and tables that
	% use a particular binary goes down to zero, then the storage area for
	% the binary can be reclaimed.

	% All of this might sound rather complicated, but the upshot is that
	% sending messages between processes that contain large binaries is very
	% cheap, and inserting tuples into ETS tables that contain binaries is
	% also very cheap. A good rule is to use binaries as much as possible for
	% representing strings and large blocks of untyped memory.
