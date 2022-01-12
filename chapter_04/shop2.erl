-module(shop2).
-export([total/1, setup_grocery_list/0]).
-import(lists, [map/2, sum/1]).

total(L) ->
	sum(map(fun({What, N}) -> shop:cost(What) * N end, L)).

setup_grocery_list() ->
	Buy = [{oranges,4},{newspaper,1},{apples,10},{pears,6},{milk,3}],
	Buy.

% from the erl shell, we can setup the list with:
% GList = setup_grocery_list().
% then
% shop2:total(GList).
% we should then get the total: 123

% How this works in chained execution:
% 1: Buy = [{oranges,4},{newspaper,1},{apples,10},{pears,6},{milk,3}],
% 2: L1 = lists:map(fun({What, N}) -> shop:cost(What) * N end, Buy)
% 3: L1 is then equal to [20, 8, 20, 54, 21]
% 4: lists:sum(L1)
% 5: 123

% -import and -export declarations
% The declaration -import(lists, [map/2, sum/1]). means the function map/2
% is imported fro mthe module lists, and so on.
% This means we can write map(Fun, ....) instead of lists:map(Fun, ...)
% cost/1 was not declared in an import declaration, so we had to use the
% "fully qualified" name shop:cost

% The declaration -export([total/1]) means the function total/1 can be called
% from outside the module shop2. Only functions that are exported from a
% module can be called form outside the module
