-module(lib_misc).
-export([sqrt/1]).

sqrt(X) when X < 0 -> 
	error({squareRootNegativeArgument, X});

sqrt(X) ->
	math:sqrt(X).

% now if we enter:
% lib_misc:sqrt(-1)
% we get
% exception error: {squareRootNegativeArgument,-1}