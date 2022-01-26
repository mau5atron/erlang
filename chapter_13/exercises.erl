-module(exercises).
-export([]).

% 1.
	
	% Write a function my_spawn(Mod, Func, Args) that behaves like spawn(
	% Mod, Func, Args) but with one difference. If the spawned process dies, 
	% a message should be printed saying why the process died and how long 
	% the process lived for before it died.

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