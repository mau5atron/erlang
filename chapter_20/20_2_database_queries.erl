% 20.2 Database Queries
	
	% Once we've created our database, we can start playing with it. We'll
	% start by looking at Mnesia queries. As you look through this, you might
	% be surprised to see that Mnesia queries look a lot like both SQL and
	% list comprehensions, so there's actually very little you need to learn
	% to get started. In fact, it really isn't that surprising that list
	% comprehensions and SQL look a lot alike. Both are based on mathematical
	% set theory.

	% In all our examples, I'll assume that you have created a database with
	% two tables called shop and cost. These tables contain the data show in
	% "Table 8, The Shop Table, pg 323" and "Table 9, the Cost Table, pg
	% 323".

	% A table in Mnesia is a set or bag of rows, where each row is an Erlang
	% record. To represent these tables in Mnesia, we need record definitions
	% that define the columns in the tables. These are as follows:

% -------------------------------------------------------------------------
		% Shop
% -------------------------------------------------------------------------
		% Item          Quantity          Cost
		% apple         20 								2.3
		% orange				100 							3.8
		% pear 					200								3.6
		% banana 				420								4.5
		% potato 				2456							1.2

% -------------------------------------------------------------------------
		% Cost
% -------------------------------------------------------------------------
		% Name          Price
		% apple         1.5
		% orange        2.4
		% pear          2.2
		% banana        1.5
		% potato        0.6


	% test_mnesia.erl

		-record(shop, {item, quantity, cost}).
		-record(cost, {name, price}).

	% Before we can manipulate the database, we need to create a database
	% schema, start the database, add some table definitions and stop the
	% database, and restart it. We need to do this only once. Here's the
	% code:

	