% Types of Table

	% ETS and DETS tables store tuples. One of the elements in the tuple (by
	% default, the first) is calle the key of the table. We insert tuples
	% into the table and extract tuples form the table based on the key. What
	% happens when we insert a tuple into a table depends upon the type of
	% the table and the value of the key. Some tables, called "sets", require
	% that all the keys in the table are unique. Others, called bags, allow
	% several tuples to have the same key.

	% Choosing the correct type of table has important consequences for the
	% performance of your applications.

	% Each of the basic set and bag table types has two variants, making for
	% a total of four types of table:

		% sets
		% ordered sets
		% bags
		% duplicate bags

	% In a "set", all the keys in the different tuples in the table must be
	% unique. In an ordered set, the tuples are sorted. In a bag, there can
	% be more than one tuple with the same key, but no two tuples in the bag
	% can be identical. In a duplicate bag several tuples can have the same
	% key, and the same tuple can occur many times in the same table.

	% There are four basic operations on ETS and DETS tables.


		% Create a new table or open an existing table.
			% This we do with ets:new or dets:open_file.

		% Insert a tuple or several tuples into a table.
			% Here we call insert(TableId, X), where X is a tuple or a list of
			% tuples. "insert" has the same arguments and works the same way in
			% ETS and DETS.

		% Look up a tuple in a table
			% Here we call lookup(TableID, Key). The result is list of tuples
			% matching key. "lookup" is defined for both ETS and DETS.

			% The return value of lookup is always a list of tuples. This is so
			% we can use the same lookup function on bags and sets. If the table
			% type is a bag, then several tuples can have the same key, but if
			% the table type is a set, then there will be only one element in the
			% list if the lookup succeeds. We'll look at the table types in the
			% next section.

			% If no tuples in the table have the required key, then an empty list
			% is returned.

		% Dispose of a table
			% When we've finished with a table, we can tell the system by calling
			% dets:closed(TableId) or ets:delete(TableId).

% -------------------------------------------------------------------------

% Example - We can illustrate how these work with the following little test
% program:

	% ets.erl

	-module(ets_test).
	-export([start/0]).

	start() ->
		lists:foreach(fun test_ets/1, [set, ordered_set, bag, duplicate_bag]).

	test_ets(Mode) ->
		TableId = ets:new(test, [Mode]), % creating table
		ets:insert(TableId, {a, 1}),
		ets:insert(TableId, {b, 2}),
		ets:insert(TableId, {a, 1}),
		ets:insert(TableId, {a, 3}),
		List = ets:table2list(TableId),
		io:format("~-13w => ~p~n", [Mode, List]),
		ets:delete(TableId).

	% This Program creates an ETS table in one of four modes and inserts the
	% tuples {a, 1}, {b, 2}, {a, 1}, and finally {a, 3} into the table. Then
	% we call tab2list, which converts the entire table into a list and
	% prints it.

	% When we run this, we get the following output:

		1> ets_test:start().
			set => [{b,2},{a,3}]
			ordered_set => [{a,3},{b,2}]
			bag => [{b,2},{a,1},{a,3}]
			duplicate_bag => [{b,2},{a,1},{a,1},{a,3}]

	% For the set table type, each key occurs only once. If we insert the
	% tuple {a, 1} in the table followed by {a, 3}, then the final value will
	% {a, 3}. The only difference between a set and an ordered set is that
	% the elements in an ordered set are ordered by the key. We can see the
	% order when we convert the table to a list by calling tab2list.

	% The bag table types can have multiple occurences of the key. So, for
	% example, when we insert {a, 1} followed by {a, 3}, then the bag will
	% contain both tuples, not just the last. In a duplicate bag, multiple
	% identical tuples are allowed in the bag, so when we insert {a, 1}
	% followed by {a, 1} into the bag, then the resulting table contains two
	% copies of the {a, 1} tuple; however, in a regular bag, there would be
	% only one copy of the tuple.
