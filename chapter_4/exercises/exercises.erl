% Exercises:

% Find the manual page for the "erlang" module. You'll see it lists a large
% number of BIFs (far more than we've covered) You'll need this information to
% solve some of the following problems:

% 1. Extend geometry.erl Add clauses to compute the areas of circles and right
% 	 angled triangles. Add clauses for computing the perimeters of different
% 	 geometric objects 
% -DONE

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