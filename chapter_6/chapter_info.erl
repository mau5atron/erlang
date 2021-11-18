% Error Handling in Sequential Programs:

% 6.2 Trapping an Exception with try .... catch

% If you're familiar with Java, then you'll have no difficulties
% understanding the try catch expression. Java can trap an exception with
% the following syntax

% try {
% 	block
% } catch(exception type identifier) {
% 	block
% } catch(exception type identifier) {
% 	block
% } finally {
% 	block	
% }

% Erlang has a similar try catch construct:block

% try FuncOrExpressionSeq of
% 	Pattern1 [when guard1] -> Expression1;
% 	Pattern2 [when guard1] -> Expression2;
% catch
% 	ExceptionType1: ExPattern1 [when ExGuard1] -> ExExpressions1;
% 	ExceptionType2: ExPattern2 [when ExGuard2] -> ExExpressions2;
% after
% 	AfterExpressions
% end

% try catch is like a case expression on steroids. It's basically a case
% expression with catch and after blocks at the end.

% -------------------------------------------------------------------------

% Try-Catch Shortcuts:

% We can omit several of the parts of a try catch expression. This:

try F
catch
end

% means the same as this:

try F of
	Val -> Val
catch
	...
end

% also the after section can be omitted

% -------------------------------------------------------------------------

% Programming Idioms with Try Catch

% When we design applications, we often make sure that the code taht
% catches an error can catch all the errors that a function can produce.

% Here's a pair of functions illustrating this. The first function
% generates three different types of an exception and has two ordinary
% return values


% -------------------------------------------------------------------------

% 6.3: Trapping an Exception with catch

% The other way to trap an exception is to use the primitive catch. The
% catch primitive is not the same as the catch block in the try catch
% statement (this is because the catch statement was part of the language
% long before try catch was introduced)

% When an exception occurs within a catch statement, it is converted into
% an {'Exit', ...} tuple that describes the error. To demonstrate this, we
% can call:

% generate_exception within a catch expression

% -------------------------------------------------------------------------

% 6.4 Programming Style with Exceptions

% Handling exceptions is not rocket science; the following sections
% contains some frequently occurring code patterns that we can reuse in
% our programs.

% Improving error Messages:

% one use of the error/1 BIF is to improve the quality of error messages.
% If we call math:sqrt(X) with negative argument, we'll see the following:

% 1> math:sqrt(-1).
% exception error: bad argument in an arithmetic expression in function
% math:sqrt/1 called as math:sqrt(-1)

% We can write a wrapper for this, which improves the error message.
% look at lib_misc.erl

% -------------------------------------------------------------------------

% Code Where Error Returns are Common

% If your function does not really have a "common case," you should
% probably return something like {ok, Value} or {error, Reason}, but
% remember that this forces all callers to do something with the return
% value. You then have to choose between two alternatives; you either write
% this:

case f(X) of
	{ok, Val} ->
		do_some_thing_with(Val);
	{error, Why} ->
		% do something with the error
end.
% which takes care of both return values, or write this:

{ok, Val} = f(X) ->
	do_some_thing_with(Val);

% Which raises an exception if f(X) returns {error, ...}

% -------------------------------------------------------------------------

% Code Where Errors are Possible But rare

% Typically you should write code that is expected to handle errors, as in
% this example:

try my_func(X)
catch
	throw:{thisError, X} -> someErr;
		% some error.

	throw:{someOtherError, X} -> someOtherErr
end.

% And the code that detects the errors should have matching throws as
% follows:

my_func(X) ->
	case ... of
		... ->
			throw({thisError, ...});
		... ->
			throw({someOtherError, ...})
	end.


% -------------------------------------------------------------------------

% Catching Every Possible Exception

% if we want to catch every possible error, we can use the following idiom 
% (which uses the fact that _ matches anything)

try Expr
catch
	_:_ -> % Code to handle all exceptions
end.

% if we omit the tag and write this:

try Expr
catch
	_ -> % Code to handle all exceptions
end.

% then we won't catch all errors, since in this case the default tag throw
% is assumed

% -------------------------------------------------------------------------

% 6.5 Stack Traces:

% When an exception is caught, we can find the latest stack trace by
% calling:

erlang:get_stacktrace().
% use this in try_test.erl

% When running demo3 inside try_test.erl
% The previous stack trace shows what happened when we tried to evaluate
% try_test:demo3(). It shows that our program crashed in the function
% generate_exception/1, which was defined in line 9 (in my case line 8) of
% the file try_test.erl

% The stack trace contains information about where the current function 
% (which crashed) would have returned to had it succeeded. The individual
% tuples in the stack trace are of the form { Mod, Func, Arity, Info }.
% Mod, Func, and Arity denote a function, and ifno contains the filename
% and line number of the item in the stack trace.

% So, try_test:generate_exception/1 would have returned to try_test:demo3
% (), which would have returned to erl_eval:do_apply/6, and so on. Ifa
% function was called from the middle of a sequence of expressions, then
% the site of the call and the palce to which the function will return are
% almost the same. If the function that was called was the last function in
%  sequence of expressions, then information about where the function was
% called from is not retained on the stack. Erlang applies a last-call
% optimization to such code, so the stack trace will not record where the
% function was called form, only where it will return to.

% Examining the stack trace gives us a good indication of where the program
% was executing at the time when the error occurred. Normally the top two
% entries on the stack trace give you enough information to locate the
% place where the error occurred.

% Now we know about handling errors in sequential programs. The important
% thign to remember is to let it crash. Never return a value when a
% function is called with an incorrect argument; raise an exception. Assume
% that the caller will fix the error.

% -------------------------------------------------------------------------

% 6.6 Fail Fast and Noisily, Fail Politely:

% We need to consider two key principles when coding for errors.

% First, we should fail as soon as soon as an error occurrrs, and we should
% fail noisily. Several programming languages adopt the principle of
% failing silently, tryign to fix up the error and continuing; this results
% in code that is a nightmare to debug. In Erlang, when an error is
% detected internally by the system or is detected by program logic, the
% correct approach is to crash immediately and generate a meaningful error
% message. We crash immdediately so as to not make matters worse. The error
% message shoudl be written to a permanent error log and be sufficiently
% detailed so tht we can figure out what went wrong later.

% Second, fail politely means that only the programmer should see the
% detailed error messages produced when a program crashes. A user of the
% program should never see these messages. On the other hand, the user
% should be alerted to the fact that an error has occurred and be told what
% action they can take to remedy the error.

% Error messages are gold dust to programmers. They should never scroll up
% the screen to vanish forever. They should go to a permanent log file that
% can be read later.

% -------------------------------------------------------------------------

% End of Chapter:

% At this point, we have covered errors only in sequential programs.
% In chapter 13, Errors in Concurrent Programs, pg 199, we'll look at how
% errors can be managed in concurrent programs, and in Section 23.2, The
% Error Logger, on page 384, we'll see how to log errors permanently so we
% never lose them.

% In the next chapter we'll look at binaries and the bit syntax. The bit
% syntax is unique to Erlang and extends pattern matching over bit fields,
% which simplifies writing programs that manipulate binary data.


% -------------------------------------------------------------------------

% Exercises

% 1. file:read_file(File) returns {ok, Bin} or {error, Why}, where file is
% 	 the filename and Bin contains the contents of the file. Write a
% 	 function myfile:read(File) that returns Bin if the file can be read
% 	 and raises an exception if the file cannot be read
% - An example I can think about for this is to try to read a zip file 
% (which should fail because it's compressed (apparently it reads all the
% bytes from it and does not fail) ) for this exercise, then add
% appropriate messages

% for the fail part, I should just git it the zip file name just drop off a
% character (basically it won't be able to find the file)


% 2. Rewrite the code in try_test.erl so that it produces two error
% 	 messages: a polite message for the user, and a detailed message for
% 	 the developer

