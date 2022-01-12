-module(guards).
-export().

% Guards:
% Guards are constructs that we can use to increase the power of pattern
% matching

% Using guards, we can perform simple tests and comparisons on the variables in
% a pattern

% Suppose we want to write a function max(X, Y) that computes the max of X and
% Y. We can write this using a guard as follows:

% max(X, Y) when X > Y -> X;
% max(X, Y) -> Y.

% Guard Sequences:
% A guard sequence is either a single guard or a series of guards, separated
% by semicolons (;). The guard sequence G1; G2; ...; Gn is true if at least one
% of the guards ---- G1, G2, ... -- evalutes to true

% A guard is a series of guard expressions, separated by commas (,). The guard
% GuardExpr1, GuardExpr2, GuardExprN is true if all the guard expressions
% GuardExpr1, GuardExpr2, GuardExprN evaluate to true

% The set of valid guard expressions is a subset of all valid Erlang expressions
% The reason for restricting guard expressiosn to a subset of Erlang
% expressions is that we want to guarantee that evaluating a guard expression
% is free from side effects

% Guards are an extension of pattern matching, and since pattern matching has
% no side effects, we don't want guard evaluation to have side effects



% Guards cannot call user-define functions, since we want to guarantee that
% they are side effect free and terminate

% The following syntactic forms are legal in a guard expression
% - The atom true
% - Other constants (terms and bound variables): these all evaluate to false
% 	in a guard expression
% - Calls to the guard predicates and to the BIFS
% - Arithmetic expressions
% - Boolean expressions
% - Short-circuit boolean expressions

% Guard Examples:
% f(X, Y) when is_integer(X), X > Y, Y < 6 -> ....

% This means "When X is an integer, X is greater than Y, and Y is less than 
% 6." The comma, which separates the test in the guard, means "and"

% Predicates:		Meaning
% is_atom(X): 						X is an atom
% is_binary(X): 					X is a binary
% is_constant(X): 				X is a constant
% is_float(X): 						X is a float
% is_function(X): 				X is a fun
% is_function(X, N): 			X is a fun with N arguments
% is_integer(X):					X is an integer
% is_list(X):							X is a list
% is_map(X):							X is a map
% is_number(X):						X is an integer or a float
% is_pid(X): 							X is a process identifier
% is_pmod(X): 						X is an instance of a parameterized module
% is_port(X):							X is a port
% is_reference(X):				X is a reference
% is_tuple:								X is a tuple
% is_record(X, Tag): 			X is a record of type Tag
% is_record(X, Tag, N): 	X is a record of type Tag and size N

% Guard Predicates
% Examples (Look at page 66)
% abs(X):	Absolute value of X

% Term comparisons:
% == 		Equal to
% /= 		Not equal to
% =<		Less than or equal to
% <			Less than
% >= 		Greater than or equal to
% > 		Greater than
% =:= 	Exactly equal to
% =/=		Exactly not equal to
