-module(accumulators).
-export([odd_and_evens1/1]).

% Accumulators:
% Often we want to return two lists from a function. For example, we might want
% to write a function that splits a list of integers into two lists
% that contain the even and odd integers in the original list:

odd_and_evens1(L) ->
	Odds = [ X || X <- L, (X rem 2) =:= 1],
	Evens = [ X || X <- L, (X rem 2) =:= 0 ],
	{Odds, Evens}.

% The problem with this code is that we traverse the list twice -- this does
% not matter when the list is short, but if the list is very long, it might be
% a problem.

% To avoid traversing the list twice, we can recode this as follows:
odds_and_evens2(L) ->
	odds_and_evens_acc(L, [], []).
odds_and_evens_acc([Head|Tail], Odds, Evens) ->
	case (Head rem 2) of
		1 -> odds_and_evens_acc(Tail, [Head|Odds], Evens);
		0 -> odds_and_evens_acc(Tail, Odds, [Head|Evens])
	end.

odds_and_evens_acc([], Odds, Evens) ->
	{Odds, Evens}.

% The above code traverses the list only once, adding the odd and even arguments
% onto the appropriate output lists (which are called accumulators).
% This code also has an additional benefit, which is less obvious; the version
% with an accumulator is more space efficient than the version with the
% [H||filter(H)] type construction

% If we run the above, we get almost the same result as before
% The difference is that the order of the elements in the odd and even lists is
% reversed

% This is a consequence of the way that the list was constructed
% If we want the list elements in the same order as they were in the original
% , all we have to do is reverse the list in the final clause of the function
% by changing the second clause of odds_and_evens2 to the following:
% odds_and_evens_acc([], Odds, Evens) ->
% 	{ lists:reverse(Odds), lists:reverse(Evens) }.


% Exercises:

% Find the manual page for the "erlang" module. You'll see it lists a large
% number of BIFs (far more than we've covered) You'll need this information to
% solve some of the following problems:

% 1. Extend geometry.erl Add clauses to compute the areas of circles and right
% 	 angled triangles. Add clauses for computing the perimeters of different
% 	 geometric objects

% 2. The BIF tuple_to_list(T) converts the elements of the tuple T to a list
% 	 Write the same thing only not using the BIF that does this

% 3. Look up the definitions of erlang:now/0, erlang:date/0, and erlang:time/0
% 	 Write a function called my_time_func(F), which evaluates the fun F and
% 	 and fun F and times how long it takes. Write a function called
% 	 my_date_string() that neatly formats the current date and time of day

% 4. Advanced: Look up the manual pages for the Python datetime module. Find
% 	 out how many of methods in the Python datetime class can be implemented
% 	 using the time-related BIFs in the erlang module. Search the erlang
% 	 manual pages for equivalent routines. Implement any glaring omissions.

% 5. Write a module called math_functions.erl, exporting the functions
% 	 even/1 and odd/1. The function even(X) should return true if X is an
% 	 even integer and otherwise false. odd(X) should return true if X is an odd
% 	 integer.

% 6. Add a higher-order function to math_functions.erl called filter(F, L),
% 	 which returns all the elements X in L for which F(X) is true.

% 7. Add a function split(L) to math_functions.erl, which returns {Even, Odd}
% 	 where Even is a list of all the even numbers in Land Odd is a list of all
% 	 the Odd numbers in L. Write this function in two different ways using
% 	 accumulators and using the function filter() you wrote in the previous
% 	 exercise