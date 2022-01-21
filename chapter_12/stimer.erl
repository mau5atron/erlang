-module(stimer).
-export([start/2, cancel/1]).

start(Time, Fun) -> spawn( fun() -> timer(Time, Fun) end ).

cancel(Pid) -> Pid ! cancel.

timer(Time, Fun) ->
	receive
		cancel -> void
	after Time ->
		Fun()
	end.

% Usage:

% 1> Pid = stimer:start(5000, fun() -> io:format("timer event~n") end).
% <0.42.0>
% timer event
% - This waits for for 5 seconds so that the timer would trigger.

% 2> Pid1 = stimer:start(25000, fun() ->  io:format(timer event) end).
% <0.49.0>
% 3> stimer:cancel(Pid).
% cancel


% Things that can be improved:
	% Better pattern matching
	% Better function passing for pattern match