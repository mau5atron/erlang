% Storing Data with ETS and DETS

	% ets and dets are two system modules that you can use for the efficient
	% storage of large numbers of Erlang. ETS is short for Erlang term
	% storage, and DETS is short for disk ETS.

	% ETS and DETS perform basically the same task: they provide large
	% key-value lookup tables. ETS is memory resident, while DETS is disk
	% resident. ETS is highly efficient - using ETS, you can store colossal
	% amounts of data (if you have enough memory) and perform lookups in
	% constant (or some some cases logarithmic) time. DETS provides almost
	% the same interface as ETS but stores the tables on DISK. Because DETS
	% uses disk storage, it is far slower than ETS but will have a much
	% smaller memory footprint when running. In addition, ETS and DETS tables
	% can be shared by several processes, making interprocess access to
	% common data highly efficient.

	% ETS and DETS tables are data structures for associating keys with
	% values. The most commonly performed operations on tables are insertions
	% and lookups. An ETS or DETS table is just a collection of Erlang
	% tuples.

	% Data stored in an ETS table is stored in RAM and is "transient". The
	% data will be deleted when the ETS table is disposed of or the owning
	% Erlang process terminates. Data stored in DETS tables is persistent and
	% should survive an entire system crash. When a DETS table is opened, it
	% is checked for consistency. If it is found to be corrupt, then an
	% attempt is made to repair the table (which can take a long time since
	% all the data in the table is checked).

	% This should recover all data in the table, though the last entry in the
	% table might be lost if it was being made at the time of the system
	% crash.

	% ETS tables are widely used in applications that have to maniupulate
	% large amounts of data in an efficient manner and where it is too costly
	% to program with nondestructive assgiment and "pure" Erlang data
	% structures.

	% ETS tables look sd if they were implemented in Erlang, but in fact they
	% are implemented in the underlying runtime system and have different
	% performance characteristics than ordinary Erlang objects. In
	% particular, ETS tables are not garbage collected; this means there are
	% no garbage collection penalties invovled in using extremely large ETS
	% tables, though slight penalties are incurred when we create or access
	% ETS objects.