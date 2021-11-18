-module(try_test).
-export([demo1/0, demo2/0, demo3/0, generate_exception/1]).

generate_exception(1) -> a;
generate_exception(2) -> throw(a);
generate_exception(3) -> exit(a);
generate_exception(4) -> {'EXIT', a};
generate_exception(5) -> error(a).

% now we'll write a wrapper function to call generate_exception in a try catch expression:

demo1() ->
	[catcher(I) || I <- [1, 2, 3, 4, 5]].

% try_test:demo1().
% [{1,normal,a},
% {2,caught,thrown,a},
% {3,caught,exited,a},
% {4,normal,{'EXIT',a}},
% {5,caught,error,a}]

% this shows that we can trap and distinguish all the forms of exception that a function can raise

demo2() ->
	[ {I, (catch generate_exception(I))} || I <- [1, 2, 3, 4, 5] ].

% try_test:demo2().
% [{1,a},
% {2,a},
% {3,{'EXIT',a}},
% {4,{'EXIT',a}},
% {5,
%  {'EXIT',{a,[{try_test,generate_exception,1,
%                        [{file,"try_test.erl"},{line,8}]},
%              {try_test,'-demo2/0-lc$^0/1-0-',1,
%                        [{file,"try_test.erl"},{line,25}]},
%              {try_test,'-demo2/0-lc$^0/1-0-',1,
%                        [{file,"try_test.erl"},{line,25}]},
%              {erl_eval,do_apply,6,[{file,"erl_eval.erl"},{line,684}]},
%              {shell,exprs,7,[{file,"shell.erl"},{line,686}]},
%              {shell,eval_exprs,7,[{file,"shell.erl"},{line,642}]},
%              {shell,eval_loop,3,[{file,"shell.erl"},{line,627}]}]}}}]

% If you compare this with the output from the try catch block section, you'll see that the two methods provide differing amounts of debug information. The first method summarized the information. The second provided a detailed stack trace.

catcher(N) ->
	try generate_exception(N) of
		Val -> {N, normal, Val}
	catch
		throw:X -> {N, caught, thrown, X};
		exit:X 	-> {N, caught, exited, X};
		error:X -> {N, caught, error, X}
	end.


% demo3 for printing out stacktrace

demo3() -> 
	try generate_exception(5)
	catch
		% error:X ->
			% { X, erlang:get_stacktrace() } this is deprecated, new way of
			% writing stack trace is:
		error:X:Stk -> { X, Stk }
		% Stk can be anything, even StackTrace
		% pull request for the change can be found here
		% https://github.com/erlang/otp/pull/1634

		% using the deprecated erlang:get_stacktrace() method will just return
		% an empty list: {a,[]}

		% whereas the new implementation will return:

		% {a,[{try_test,generate_exception,1,
    %           [{file,"try_test.erl"},{line,8}]},
    % {try_test,demo3,0,[{file,"try_test.erl"},{line,59}]},
    % {erl_eval,do_apply,6,[{file,"erl_eval.erl"},{line,684}]},
    % {shell,exprs,7,[{file,"shell.erl"},{line,686}]},
    % {shell,eval_exprs,7,[{file,"shell.erl"},{line,642}]},
    % {shell,eval_loop,3,[{file,"shell.erl"},{line,627}]}]}
	end.