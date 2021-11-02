-module(shop).
-export([cost/1]).

cost(oranges)		-> 5;
cost(newspaper)	-> 8;
cost(apples)		-> 2;
cost(pears)			-> 9;
cost(milk)			-> 7.

% The function cost/1 is made up from 5 clauses. The head of each clause
% contains a pattern (in this case a very simple pattern that is just an atom).

% When we evaluate shop:cost(X), then the system will try to match X against
% each of the patterns in these clauses.

% If a match is found, the code to the right of the -> is evaluated\
% trying to use:
% shop:cost(socks).
% exception error* no function clause matching shop:cost(socks) 
% (shop.erl, line 4)

% We asked what socks cost but no clause matched, so we got a pattern
% matching error

% Suppose we have a shopping list:
% Buy = [{oranges, 4}, {newspaper, 1}, {apples, 10}, {pears, 10}, {milk, 3}].

