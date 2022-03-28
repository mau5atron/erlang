-module(lib_misc).
-export([]).

on_exit(Pid, Fun) ->
	spawn(
		fun() ->

			Ref = monitor(process, Pid),
			receive {'DOWN', Ref, process, Pid, Why} ->
				Fun(Why)
			end

		end).

keep_alive(Name, Fun) ->
	register(Name, Pid = spawn(Fun)),
	on_exit(Pid, fun(_Why) -> keep_alive(Name, Fun) end).


consult(File) ->
	case file:open(File, read) of
		{ok, S} ->
			Val = consult1(S),
			file:close(S),
			{ok, Val};

		{error, Why} ->
			{error, Why}
	end.

consult1(S) ->
	case io:read(S, '') of
		{ok, Term} -> [Term|consult1(S)];
		eof 			 -> [];
		Error 		 -> Error
	end.


% writes a list of terms to a file
unconsult(File, L) ->
	{ok, S} = file:open(File, write),
	lists:foreach(fun(X) -> io:format(S, "~p.~n", [X]) end, L),
	file:close(S).