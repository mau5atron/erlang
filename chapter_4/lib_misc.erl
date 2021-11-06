-module(lib_misc).
-export([]).

% Defining Your Own Control Abstractions
% 	- So far, we haven't seen any 'if' statements, 'switch' statements,
% 		'for' statements, or 'while' statements, and yet this doesn't seem to
% 		matter. Everything is written using pattern matching and high-order
% 		functions
% 	- If we want additional control structures, we can make our own. Here's
% 		an example: Erlang has no 'for' loop, so let's make one

for(Max, Max, F) -> [F(Max)];
for(I, Max, F)	 -> [F(I)|for(I+1, Max, F)].

% Example: evaluating for(1,10,F) creates the list [F(1), F(2),...F(10)].
% Now we have a simple for loop. We can use it to make a list of the integers
% from 1 to 10

% lib_misc:for(1, 10, fun(I) -> I end).
% shell response: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

% Or we can compute the squares of the integers from 1 to 10
% lib_misc:for(1,10,fun(I) -> I*I end).
% shell: [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]


% Quick Sort
% Here's how to write a sort algorithm using two list comprehensions
qsort([]) -> [];
qsort([Pivot|T]) ->
	qsort([ X || X <- T, X < Pivot ])
	++ [Pivot] ++
	qsort([ X || X <- T, X >= Pivot ]).
	% Note that ++ is the infix append operator. This code is shown for its
	% elegance rather than its efficiency. Using ++ in this way is not generally
	% considered good programming practice.
	% See sect 4.9 (Building lists in natural order for more info)

	% To see how this works, we'll step through the execution