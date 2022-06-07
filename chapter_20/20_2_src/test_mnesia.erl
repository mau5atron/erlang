-module(test_mnesia).
-record(shop, {item, quantity, cost}).
-record(cost, {name, price}).
-record(design, {id, plan}).
-include_lib("stdlib/include/qlc.hrl").
-import(lists, [foreach/2]).
-export([
	mnesia_setup/0, 
	select_all_from_shop/0, 
	reset_tables/0, 
	start/0,
	select_some/0
]).

% Before we can manipulate the database, we need to create a database
% schema, start the database, add some table definitions and stop the
% database, and restart it. We need to do this only once. Here's the
% code:

% How to run:

% 1. c(test_mnesia).
% 2. test_mnesia:mnesia_setup().
% 3. test_mnesia:start().
% 4. test_mnesia:reset_tables().
% 5. test_mnesia:select_all_from_shop().

	% returns
	% [{shop,potato,2456,1.2},
	% {shop,apple,20,2.3},
	% {shop,orange,100,3.8},
	% {shop,pear,200,3.6},
	% {shop,banana,420,4.5}]

mnesia_setup() ->
	mnesia:create_schema([node()]),
	mnesia:start(),
	mnesia:create_table(shop, [{attributes, record_info(fields, shop)}]),
	mnesia:create_table(cost, [{attributes, record_info(fields, cost)}]),
	mnesia:create_table(design, [{attributes, record_info(fields, design)}]),
	mnesia:stop().

table_data() ->
	[
		{shop, apple,   20,   2.3},
   	{shop, orange,  100,  3.8},
   	{shop, pear,    200,  3.6},
   	{shop, banana,  420,  4.5},
   	{shop, potato,  2456, 1.2},
    {cost, apple,   1.5},
    {cost, orange,  2.4},
    {cost, pear,    2.2},
    {cost, banana,  1.5},
    {cost, potato,  0.6}
	].

do(Query) ->
	F = fun() -> qlc:e(Query) end,
	{atomic, Val} = mnesia:transaction(F),
	Val.

select_all_from_shop() ->
	do(
		qlc:q([X || X <- mnesia:table(shop)])
	).

reset_table_data() ->
	F = fun() ->
		foreach(fun mnesia:write/1, table_data())
	end,
	mnesia:transaction(F).

reset_tables() ->
	% clears the tables
	mnesia:clear_table(shop),
	mnesia:clear_table(cost),
	% add the data again
	reset_table_data().

start() ->
  mnesia:start(),
  mnesia:wait_for_tables([shop,cost,design], 20000).


% Steps to query something:

	% 1. Query = qlc:q([X || X <- mnesia:table(shop)]).
	% 2. EvalQueryFun = fun() -> qlc:e(Query) end.
	% 3. {atomic, QueryResult} = mnesia:transaction(EvalQueryFun).
	% 4. QueryResult.
	% [{shop,potato,2456,1.2},
 	% {shop,apple,20,2.3},
 	% {shop,orange,100,3.8},
 	% {shop,pear,200,3.6},
 	% {shop,banana,420,4.5}]

select_some() ->
	Query = qlc:q([ {X#shop.item, X#shop.quantity} || X <- mnesia:table
		(shop)]),
	EvalQueryFun = fun() -> qlc:e(Query) end,
	{atomic, QResult} = mnesia:transaction(EvalQueryFun),
	QResult.
	% returns:
	% [{potato,2456},
	%  {apple,20},
	%  {orange,100},
	%  {pear,200},
	%  {banana,420}]

add_plans() ->
	% left off on page 326.