-module(funexamples).
-export([list_map/0, list_filter/1, find_in_list/1, nested_fun/2]).


% Functions that Have Funs As Their Arguments
% The module lists, which is in the standard libraries, exports several
% functions whose arguments are funs. The most useful of all these is
% lists:map(F, L). This is a function that returns a list made by applying
% the fun F to every element in the list L.
list_map() ->
	L = [1, 2, 3, 4],
	lists:map(fun(X) -> 2*X end, L).

% Another ueful function is lists:filter(P, L), which returns a new list of all
% the elements E in L such that P(E) is true

% Let's define a function Even(X) that is true if X is an even number.

list_filter(FindEven) ->
	Even = fun(X) -> (X rem 2) =:= 0 end,
	Even(FindEven).
	% equivalent of (X % 2) == 2
	% Here X rem 2 computes the remainder after X has been divided by 2, and
	% =:= is a test for equality. Now we can test Even, and then we can use
	% it as an argument to map and filter.


% Functions That Return Funs
find_in_list(FruitItem) ->
	Fruit = [apple, pear, orange],
	MakeTest = fun(L) -> 
		(
			fun(X) -> 
				lists:member(X, L) 
			end
		) 
	end,

	IsFruit = MakeTest(Fruit),
	IsFruit(FruitItem),
	% erl: funexamples:find_in_list(pear) % true
	% erl: funexamples:find_in_list(strawb) % false
	% We can also use IsFruit as an argument for lists:filter()
	lists:filter(IsFruit, [dog, orange, cat, apple, bear]).

% Putting a fun inside another fun

nested_fun(Multiple, Val) ->
	% Times -> what to multiply by
	% X -> value to multiply after we specified what to multiply by
	Mult = fun(Times) ->
	 (
	  fun(X) -> 
	  	X * Times 
	  end 
	 ) 
	end,
	% Mult is a generalization of (Double, or just multiplying a number)
	% Instead of computing a value, it returns a function, which when called
	% will compute the required value
	Triple = Mult(Multiple),
	Triple(Val).








