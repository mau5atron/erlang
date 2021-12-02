-module(sec_encrypted).
-export([hash/0, secure/0]).

hash() ->
	io:fwrite("Hi, Hater👋~n").

secure() ->
	io:fwrite("Bye, Hater👍~n").