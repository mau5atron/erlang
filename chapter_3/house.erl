-module(house).
-export([init/0, print_house/1, create_house/2]).

% Exercise
% Try representing a house using a tuple and a street using a list of houses.
% Make sure you can pack and unpack the data in the representations

init() ->
	io:format("Initializing house \n"),
	House = {house, {street, 'oak creek rd'}, {color, white}},
	print_house(House).

print_house(House) ->
	House.

create_house(Street, Color) ->
	{house, {street, Street}, {color, Color}}.