-module(area_server1).
-export([loop/0, rpc/2]).

loop() ->
	receive
		{From, {rectangle, Width, Height}} ->
			CalculatedRec = Width*Height,
			io:format("Area of rectangle is ~p~n", [CalculatedRec]),
			From ! CalculatedRec, % this replies to the requestor with calculated value
			loop();

		{From, {square, Side}} ->
			CalculatedSquare = Side*Side,
			io:format("Area of square is ~p~n", [CalculatedSquare]),
			From ! CalculatedSquare,
			loop();

		{From, Other} ->
			From ! {error, Other},
			loop()
	end.

rpc(Pid, Request) ->
	Pid ! {self(), Request},
	receive
		{Pid, Response} -> Response
	end.