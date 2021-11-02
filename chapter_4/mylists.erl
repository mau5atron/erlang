-module(mylists).
-export([sum/1, testing_sum/0]).

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
map()