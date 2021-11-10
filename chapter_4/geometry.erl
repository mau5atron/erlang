-module(geometry).
-export([create_rec/0, create_square/0, area/1, perimeter/1]).
-import(math, [sqrt/1]).
-define(PI, 3.14159).

create_rec() ->
	Rectangle = {rectangle, 10, 5},
	{rectangle, Width, Height} = Rectangle, % this is called "unpacking fields"
	% using pattern matching

	io:format("Width: ~p, Height: ~p\n", [Width, Height]),
	% erlang:display() can also use this to print
	% erlang:display("Hello\n"),
	Rectangle.

create_square() ->
	Square = {square, 3},
	{square, Side} = Square,
	io:format("Side: ~p\n", [Side]),
	Square.

% instead of creating separate functions like above, we can instead use
% pattern matching

area({rectangle, Width, Height}) -> Width * Height;
area({square, Side})						 -> Side * Side;

% The function area() consists of two clauses.
% The clauses are separated by a semicolon, and the final clause is terminated
% by dot whitespace

% Each clause has a head and a body separated by an array (->).
% The head consists of a function name folloed by zer0 or more patterns,
% and the body consists of a sequence of expressions (expressions are defined)
% in 8.13

% Now suppose we want to extend our program by adding a cirlce to our
% geometric shapes. We could write this:

area({circle, Radius})					 ->
	?PI * Radius * Radius;

% Note that in this example, the order of the clauses does not matter; the
% program means the same no matter how the clauses are ordered. This is
% because the patters in the clause are mutually exclusive. This makes writing
% and extending programs very easy ---- we just add more patterns.
% In general, though, clause order does matter. When a function is entered,
% the clauses are pattern matched against the calling arguments in the order
% they are presented in the file

% Note the following about the way the area function is written:
% - The function area consists of several different clauses. When we call the
% 	function, execution starts in the first clause that matches the call
% 	arguments.
% - Our function does not handle the cases where no pattern matches --- our
% 	program will fail with a runtime error. This is deliberate.
% 	This is the way we program in Erlang




% Where to Put Semicolons:
% - Commas(,) separate arguments in function calls, data constructors, and
% 	patterns
% - Semicolons(;) separate clauses. We find clauses in several contexts, namely
% 	in function definitions and in 'case', 'if', 'try..catch', and 'receive'
% 	expressions
% - Periods(.)(followed by whitespace) separate entire functions and expressions
% 	in the shell

% semi_colons_as_separators() ->
% 	case f(Something) of
% 		Pattern1 ->
% 			Expressions1;
% 		Pattern2 ->
% 			Expressions2;
% 		LastPattern ->
% 			LastExpression
% 	end.

% Note that the last expression (that which immediately precedes the 'end' 
% key-word) has no semicolon

% That's enough theory for now. Let's continue with some code; we'll get back
% to control structures later.


% finding area of right-angled triangle: 1/2 * base * height or A*B/2
area({right_triangle, Base, Height}) ->
	(Base*Height)/2.

% Perimeter of rectangle 2(L + W)
perimeter({rectangle, Length, Width}) ->
	2*(Length+Width);

% Perimeter (circumference) of circle 2*PI*R
% call global constants with ?PI
perimeter({circle, R}) ->
	2*?PI*R;

perimeter({triangle, A, Base, C}) ->
	A+Base+C;

perimeter({right_triangle, A, B}) ->
	A+B+sqrt((A*A)+(B*B)).

