-module(shop1).
-export([total/1]).

total([{What, N}|T]) -> shop:cost(What) * N + total(T);
total([]) 					 -> 0.

% when we compile and run shop1:total([]). we get back 0
% second pattern match will return 0

% more complex query
% shop:total([{milk,3}]). returns 21
% evaluating the body of the function, we evaluate the expression
% shop:cost(milk) * 3 + total([]);

% shop:cost(milk) is 7 and total([]) is 0, so the final return value is 21

% testing with even more complex argument
% shop1:total([{pears,6}, {milk,3}]). returns 75 (54 + 21)
% total([{What,N}|T]) -> shop:cost(What) * N + total(T);

