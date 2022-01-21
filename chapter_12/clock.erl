-module(clock).
-export([start/2, stop/0]).

start(Time, Fun) ->
	register(clock, 
		spawn(
			fun() ->
				tick(Time, Fun)
			end
		)
	).

% sending clock with registered Pid to stop
stop() -> clock ! stop.

% this is the function that is run as part of the new spawned process and
% registration
tick(Time, Fun) ->
	% pattern match "stop"
	receive stop ->
		void
	after Time ->
		Fun(),
		tick(Time, Fun)
	end.

%1> clock:start(3000, fun() -> io:format("TICK: ~p~n", [erlang:now()])
% end).
% This clock will tick every 3 seconds

% To stop the clock
% 2> clock:stop().