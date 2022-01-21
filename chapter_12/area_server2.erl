-module(area_server2).
-export([loop/0, area/2, start/0]).

start() -> spawn(area_server2, loop, []).

area(Pid, What) -> rpc(Pid, What).

rpc(Pid, Request) ->
	Pid ! {self(), Request},
	receive
		{Pid, Response} -> Response
	end.

loop() ->
	receive
		{From, {rectangle, Width, Height}} ->
			CalculatedRec = Width*Height,
			% io:format("Area of rectangle is ~p~n", [CalculatedRec]),
			% From ! CalculatedRec, % this replies to the requestor with calculated value
			From ! {self(), CalculatedRec},
			loop();

		{From, {square, Side}} ->
			CalculatedSquare = Side*Side,
			% io:format("Area of square is ~p~n", [CalculatedSquare]),
			% From ! CalculatedSquare,
			From ! {self(), CalculatedSquare},
			loop();

		{From, Other} ->
			% From ! {error, Other},
			From ! {self(), {error, Other}},
			loop()
	end.


% Usage: 
% 4> Pid = area_server2:start().
% <0.171.0>
% 5> area_server2:area(Pid, {rectangle, 10, 20}).
% 200
% 6> area_server2:area(Pid, {square, 20}).       
% 400
