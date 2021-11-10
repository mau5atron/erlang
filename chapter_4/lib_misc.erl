-module(lib_misc).
-export([
	for/3, 
	qsort/1, 
	pythag/1, 
	perms/1, 
	my_tuple_to_list/1,
	my_time_func/1,
	my_date_string/0
]).

% -include(httpd_util).

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
	% try quicksort example with list: 
	% L = [23, 6, 2, 9, 27, 400, 78, 45, 61, 82, 14]

	% Note that ++ is the infix append operator. This code is shown for its
	% elegance rather than its efficiency. Using ++ in this way is not generally
	% considered good programming practice.
	% See sect 4.9 (Building lists in natural order for more info)

	% To see how this works, we'll step through the execution. We start with
	% a list L and call qsort(L). The following matches the second clause of
	% qsort with bindings Pivot -> 23 and T -> [rest of the list ie 6, 2, 9, 5]

	% [Pivot|T] = L
	% returns: [23, 6, 2, 9, 27, 400, 78, 45, 61, 82, 14]

	% Now we split T into two lists, one with all the elements in T that are less
	% than Pivot, and the other with all the elements greater than or equal to
	% Pivot (pivot is basically the H (head) so it gets assigned the first
	% element in the list)

	% Smaller = [ X || X <- T, X < Pivot].
	% returns: [6, 2, 9, 14]

	% Bigger = [ X || X <- T, X >= Pivot ].
	% returns: [27, 400, 78, 45, 61, 82]

	% Now we sort Smaller and Bigger and combine them with Pivot

	% qsort( [6,2,9,14] ) ++ [23] ++ qsort( [27, 400, 78, 45, 61, 82] )
	% = [2, 6, 9, 14] ++ [23] ++ [27, 45, 61, 78, 82, 400]
	% = [2, 6, 9, 14, 23, 27, 45, 61, 78, 82, 400]




% Pythagorean Triplets:
% Pyhtagorean Triplets are sets of integers {A, B, C} where A^2, + B^2 = C^2

% The function pythag(N) generates a list of all integers {A, B, C} where 
% A^2, + B^2 = C^2 and where the sum of the sides is less than or equal to N

pythag(N) ->
	% lists:seq(From, To) -> Seq
	% returns a sequence of integers that starts with From and contains
	% the successive results of adding Incre to the previous element
	% until, To is reached or passed (in the latter case, To is not an element
	% of the sequence). Incr defaults to 1
	[ {A, B, C} ||
			A <- lists:seq(1,N),
			B <- lists:seq(1,N),
			C <- lists:seq(1,N),
			A + B + C =< N,
			A*A + B*B =:= C*C
	].
	% [1 ... 25]

	% lists:seq(1, N) returns a list of all the integers from 1 to N
	% Thus, A <- lists:seq(1, N) means that A takes all possible values from
	% 1 to N. So, our program reads, "Take all values of A from 1 to N,
	% all values of B from 1 to N, and all values of C from 1 to N such that 
	% A + B + C is less than or equal to N and A*A + B*B = C*C"
	
	% lib_misc:pythang(16)
	% returns: [{3, 4, 5}, {4, 3, 5}]

	% lib_misc:pythang(30)
	% returns: [{3, 4, 5}, {4, 3, 5}, {5, 12, 13}, {6, 8, 10}, {12,5,13}]

	%  Basically all values from 1 to N, (for A, B, C) when added together amount
	% to N or less than N AND when A, B (squared) are equal to C (squared)


% Anagrams: Finding Permutations of a String
perms([]) -> [[]];
perms(L)	-> [ [H|T] || H <- L, T <- perms(L--[H]) ].
% take head from list, then assign perms() call to tail
% libs_misc:perms("123").
% returns: ["123", "132", "213", "231", "312", "321"]

% X -- Y is the list subtraction operator. It subtracts the elements from X
% There's a more precise definition in Section 8.16

% perms works as followed:
% Assume we want to compute all permutations of the string "cats"
% First we isolate the first character of the string, which is C, and
% compute all permutations of the string with the character C removed.
% "cats" with c removeed is th strign "ats", and all the permutations of "ats"
% are the strings ["ats", "ast", "tas", "tsa", "sat", "sta"]. Next we append
% the C to the beginning of each of these strings, formatting
% ["cats", "cast", "ctas", "ctsa", "scat", "scta"]
% Then we repeat the algorithm isolating the second character, and so on

% This is exactly what the permss function does
% [ [H|T] || H <- L, T <- perms( L -- [H] ) ]
% 'This means take H from L in all possible ways and then take T from
% perms (L -- [H]) (that is, all permutations of the list L with H removed)
% in all possible ways and return [H|T]




% 2. Write the equivalent of tuple_to_list without using the BIF
% my_tuple_to_list({}) -> [];
my_tuple_to_list(T) ->
	% get tuple items
	% lists:seq(1, 0) gives an empty list
	% written like this, we don't need the base case

	% calc size of tuple
	% element: returns the nth element (numbering from 1) of Tuple

	% lists:seq/2 => lists:seq(From, To) => Returns a sequence of integers that 
	% starts with From and contains the successive results of adding Incr to the 
	% previous element, until To is reached or passed (in the latter case, To is 
	% not an element the sequence). Incr defaults to 1

	% Returns an integer that is the number of elements in Tuple, for example:
	[ element(I, T) || I <- lists:seq(1, tuple_size(T)) ].


% 3. Look up erlang:now/0, erlang:date/0, erlang:time/0
% - Write a function called my_time_func(F), which evaluates the fun F and 
% 	times how long it takes.
% - Write a function called my_date_string() that neatly formats the current
% 	date and time of day

% calling this function from erl as 
% - lib_misc:my_time_func(erlang:now())
% - lib_misc:my_time_func(erlang:date())
% - lib_misc:my_time_func(erlang:time())

% erlang:now()
% Returns the tuple {Megasecs, Secs, Microsecs}, which is the elapsed time since
% 00:00 GMT, Jan 1, 1970 (zero hour), if provided by the underlying OS.
% Otherwise some other point in time is chosen. It is also guaranteed that
% the following calls to this BIF return continously increasing values. Hence,
% the return value from erlang:now/0 can be used to generate unique timestamps
% If it is called in a tight loop on a fast machine, the time of the node can
% become skewed
% - Can only be used to check the local time of day if the time-zoneeweewdw

% erlang:date()
% Returns the current date as {Year, Month, Day}
% The time zone and Daylight Saving Time correction depond on the underlying
% OS. The return value is based on the OS System Time.

% erlang:time()
% Returns the current time as {Hour, Minute, Second}
% The time zone and Daylight saving time correction depend on the underlying OS
% The return value is based on the OS System Time.
% Example: {5, 51, 42} 5 AM, 51 MIN, 52 SEC
my_time_func(F) ->
	F.

my_date_string() ->
	CurrentDate = erlang:date(),
	{Year, Month, Day} = CurrentDate,
	CurrentTime = erlang:time(),
	{Hour, Min, Sec} = CurrentTime,
	% io:fwrite("The date is ~p ~p, ~p~n", 
	io:fwrite("The date is ~s ~p, ~p @ ~p:~p:~p ~n",
		[
			httpd_util:month(Month),
			calendar:day_of_the_week(CurrentDate),
			Year,
			Hour,
			Min,
			Sec
		]
	).
	% will return: The date is Nov 3, 2021 @ 6:30:36
