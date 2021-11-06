-module(mylists).
-export([
	sum/1, 
	testing_sum/0, 
	map/2, 
	double_the_list/0, 
	square_the_list/0, 
	map_with_empty_list/0
]).

% Simple List Processing
% Now that we've introduced funs, we can get back to writing sum and map, which
% we'll need for our improved version of total()

% starting with sum to compute the sum of the elements in the list
sum([H|T]) -> H + sum(T);
sum([]) -> 0.

% Note that the order of the two clauses in sum is unimportant. This is because
% the first clause matches a nonempty list and the second an empty list, and
% these two cases are mutually exclusive. We can test sum as follows.

testing_sum() ->
	L = [1, 3, 10],
	sum(L). % sum all items in the list together

% Execution trace:
% 1. sum([1,3,10])
% 2. sum([1,3,10]) = 1 + sum([3,10])
% 3. = 1 + 3 + sum([10])
% 4. = 1 + 3 + 10 + sum([])
% 5. = 1 + 3 + 10 + 0
% 6. = 14

map(_, [])			-> [];
map(F, [H|T]) 	-> [F(H)|map(F, T)].

% The first clause says what to do with an empty list. Mapping any function
% over the element of an empty list (there are none!) just produces an empty
% list

% The second clause is a rule for what to do with a list with a head H and
% tail T. That's easy. Just build a new list whose head is F(H) and whose tail
% is map(F, T).

% We can run 'map' using a couple of functions that double and square
% the elements in a list, as follows:

% doubles every list item
double_the_list() ->
	L = [1, 2, 3, 4, 5],
	map(fun(X) -> 2*X end, L).

% squares every list item
square_the_list() ->
	L = [1, 2, 3, 4, 5],
	map(fun(X) -> X*X end, L).

% this just returns an empty list
map_with_empty_list() ->
	L = [],
	map(fun(X) -> X+5 end, L).

