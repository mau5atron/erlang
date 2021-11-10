-module(bifs).
-export().

% A BIF is a Built-in-function
% BIFS provide interfaces to the operating system or perform operations that
% are impossible or very inefficient to program in Erlang

% For example, turning a list into a tuple or to find the current time
% and date.

% Examples
% list_to_tuple([12, cat, "hello"]).
% returns: {12, cat "Hello"}

% time().
% returns: {20,0,3}