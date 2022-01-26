-module(chap13_exercises).
-export([my_spawn/3, loop/0, on_exit/2, setup/0]).

setup() ->
	% Pid = my_spawn(?MODULE, loop, []),
	MainPid = my_spawn(?MODULE, loop, []),
	register(mainpid, MainPid),
	register(monitorpid, MonitorPid = on_exit(MainPid, fun(Why) ->
		io:format("~p died with: ~p~n", [MainPid, Why])
	end)).

% in order to view the registered pids
% registered() to list registered pids
% whereis() to show registered pid addresses
% whereis(monitorpid)
% whereis(mainpid)

% Pid = chap13_exercises:my_spawn(chap13_exercises, loop, []).
my_spawn(Mod, Func, Args) ->
	% Module, Func, Args
	% Module is supposed to be the name of the current module

	% We can omit this by saying ?MODULE as that would then be the current 
	% module
	spawn(?MODULE, Func, Args).

% This should be called as:
% chap13_exercises:on_exit(Pid, fun(Why) ->
	% io:format("~p died with: ~p~n", [Pid, Why])
% end)
on_exit(Pid, Fun) ->
	spawn(fun() -> 
					Ref = monitor(process, Pid),
					% io:format("MonitorPid: ~p~n started", [Ref]),
					receive {'DOWN', Ref, process, Pid, Why} ->
						Fun(Why)
					end
				end).

loop() ->
	% 1. calculate how long the spawned process is running for

	% 2. when the process dies, display why the process died and how long it 
	% was runing before it died

	% a. Make process die
	% b. Display message when process dies

	% InitialTime = erlang:time(), % will print {hour, min, sec}
	% InitialTime keeps changing on recursion

	% When I call self(), it shows the current process spawned
	io:format("Current pid: ~p~n", [self()]),
	receive
		{message, X} ->
			io:format("Received: ~p~n", [X]),
			% io:format("InitialTime: ~p~n", [InitialTime]),
			loop();
		{trykill, X} ->
			% before we try and kill the process, we have to setup a monitor
			% process that will send a message once the first process dies due to
			% an error
			list_to_atom(X)
	end.


	% 1.
	
	% Write a function my_spawn(Mod, Func, Args) that behaves like spawn(
	% Mod, Func, Args) but with one difference. If the spawned process dies, 
	% a message should be printed saying why the process died and how long 
	% the process lived for before it died.


	% Steps:

	% 1. Pid = chap13_exercises:my_spawn(chap13_exercises, loop, []).
		% <0.114.0>
	% 2. chap13_exercises:on_exit(Pid, fun(Why) -> io:format("~p died with: 
	% ~p~n", [Pid, Why]) end).
		% <0.116.0>
	% 3. Pid ! {trykill, hello}.

		% <0.114.0> died with: {badarg,
		%                          [{erlang,list_to_atom,[hello],[]},
		%                           {chap13_exercises,loop,0,
		%                               [{file,"chap13_exercises.erl"},{line,47}]}]}
		% =ERROR REPORT==== 24-Jan-2022::09:06:19.828103 ===
		% Error in process <0.114.0> with exit value:
		% {badarg,[{erlang,list_to_atom,[hello],[]},
		%          {chap13_exercises,loop,0,[{file,"chap13_exercises.erl"},{line,47}]}]}

		% {trykill,hello}

	% For gathering when the main process started and died, I will be writing
	% to a logfile
	% then just need to grab the first and last additions and

% 2.

	% Solve the previous exercise using the on_exit function shown earlier in
	% this chapter.

% 3.

	% Write a function my_spawn(Mod, Func, Args, Time) that behaves like
	% spawn(Mod, Func, Args) but with one difference If the spawned process
	% lives for more than Time seconds, it should be killed.

% 4.

	% Write a function that creates a registered process that writes out
	% "I'm still running" every five seconds. Write a function that monitors
	% this process and restarts it if it dies. Start the global process and
	% the monitor process.

% 5. 

	% Write a function that starts and monitors several worker processes. If
	% any of the worker processes dies abnormally, restart it.

% 6.

	% Write a function that starts and monitors several worker processes. If
	% any of the worker processes dies abnormally, kill all the worker
	% processes and restart them all.