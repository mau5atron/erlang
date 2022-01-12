-module(street).
-export([init/0, list_street/0]).
-import(house, [create_house/2]).

init() ->
	io:format("Initializing street\n"),
	list_street().

list_street() ->
	House1 = create_house('oak creek rd', 'white'),
	House2 = create_house('don pedro rd', 'beige'),
	% House3 = create_house('some street', 'brown'),
	Street = [House1, House2],
	Street.
