-module(list_comprehension).
-export([double_the_list/0,
	double_the_list_with_comprehension/0,
	double_the_g_list_with_comprehension/1,
	get_g_list_with_cost/1,
	compute_g_list_cost/1,
	sum_g_list_cost/1,
	calc_total/1,
	map/2,
	testing_shorter_map/0,
	list_comprehension_filter/0
]).
-import(shop2, [setup_grocery_list/0]). % importing module function

double_the_list() ->
	L = [1, 2, 3, 4, 5],
	lists:map(fun(X) -> X*2 end, L).

% easier way to double the list
double_the_list_with_comprehension() ->
	L = [1, 2, 3, 4, 5],
	[ 2*X || X <- L ].

% The notation [ F(X) || X <- L ]
% means "the ist of F(X) where X is taken from the list L."
% Thus, [ 2*X || X <- L ] means "the list of 2*X where X 
% is taken from the list L"

double_the_g_list_with_comprehension(GList) ->
	[ {Name, 2*Number} || {Name, Number} <- GList ].
	% returns: [{oranges,8},{newspaper,2},{apples,20},{pears,12},{milk,6}]
	% after setting 
	% GList = [{oranges,4},{newspaper,1},{apples,10},{pears,6},{milk,3}]

% Note that the tuple {Name, Number} to the right side of the (||) sign is Name
% pattern that matches each of the elements in the list Buy. The tuple to the
% left side, {Name, 2*Number}, is a constructor.

% Suppose we want to compute the total cost of all the elements in the original
% list; we could do this as follows. First replace the name of every item in
% the list with its price.

% 1.
get_g_list_with_cost(GList) ->
	[ {shop:cost(A), B} || {A, B} <- GList ].

% setting this up:
% GList = shop2:setup_grocery_list()
% list_comprehension:calc_g_list_total(GList).
% The grocery list is now:
% [{5, 4}, {8,1}, {2,10}, {9,6}, {7,3}]

% now try to compute the cost of the grocery list:
% 2.
compute_g_list_cost(GList) ->
	% [ shop:cost(A) * B || {A, B} <- GList ].
	[ A * B || {A, B} <- GList ].
	% returns: [20, 8, 20, 54, 21]

% 3.
sum_g_list_cost(GList) ->
	lists:sum(GList).
	% return: 123

% using list comprehensions with 
calc_total(GList) ->
% calc_total() ->
	TotalCost = fun(L) ->
		GListWithCost = [{shop:cost(A), B} || {A, B} <- L],
		% 1
		fun() ->
			MultipliedGlistWithCost = [ A * B || {A, B} <- GListWithCost ],
			% 2
			fun() ->
				lists:sum(MultipliedGlistWithCost)
				% 3
			end
		end
	end,
	TotalCostWithList = TotalCost(GList), % 1
	GListWithCost = TotalCostWithList(), % 2
	GListWithCost(). % 3

	% L = [1, 2, 3, 4],
	% DoubleTheList = fun(X) ->
	% 	Doubled = [ A*2 || A <- X], % pattern match against the list items
	% 	% pass in doubled in order to triple the list items
	% 	fun() ->
	% 		[ A * 3 || A <- Doubled ]
	% 	end
	% end,
	% DList = DoubleTheList(L), % Double the list
	% DList(). % triple the list, call the second fun

% List comprehensions will make your code really short and easy to read.
% For example, we can define an even shorter version of map.
map(F, L) ->
	[F(X) || X <- L].

testing_shorter_map() ->
	SomeList = [1, 2, 3, 4],
	map(fun(X) -> X*2 end, SomeList).


% The most general form of a list comprehension is an expression of the
% following form:
% [ X || Qualifier1, Qualifier2, ....]

% X is an arbitrary expression, and each qualifier is either a generator
% a bitstring generator, or a filter
% - Generators are written as Pattern <- ListExpr where ListExpr must be an
% 	an expression that evaluates to a list of terms

% - Bitstring generators are written as BitStringPattern <= BitStringExpr
% 	where BitStringExpr must be an expression that evaluates to a bitstring.
% 	More information about bitstring patterns and generators can be found in
% 	The Erlang reference manual

% - Filters are either predicates (functions that return true or false) or 
% 	boolean expressions

% Note that the generator part of a list comprehension works like a filter
% here's an example

list_comprehension_filter() ->
	% pattern matches {a, 1} & {a, 4} and returns [1, 4]
	[ X || {a, X} <- [{a, 1}, {b, 2}, {c, 3}, {a, 4}, hello, "wow"]].