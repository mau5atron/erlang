-module(lib_misc).
-export([
	eval_expression/1
]).

% -spec eval_expression(string()) -> any().
% eval_expression(Expression) ->
% 	{ok, Tokens, _} = erl_scan:string(Expression),
% 	{ok, Parsed} = erl_parse:parse_exprs(Tokens),
% 	{value, Result, _} = erl_eval:exprs(Parsed, []),
% 	Result.

% Usage when passing in string to evaluate
% NOTE: Make sure to start the server again after each successful response
% because the socket is closed right after
 
 	% 1. nano_client:nano_client_eval("4 > 3.").

 	% 2. nano_client:nano_client_eval("list_to_tuple([20+30, 40+50]).").

% Below the code is updated to reflect better security and avoid users
% being able to call internal functions

% -------------------------------------------------------------------------

% Best Practices:

	% Intercepting Local Function Calls

		% We can supply a function to erl_eval:exprs through which all calls to
		% local functions will be passed, and that's where we can take
		% additional actions.

		% Local functions are those in the same module, which can be called
		% without specifying the module name. (Though some BIFs don't require
		% a module name, like list_to_binary, it's because they're
		% auto-imported by the system - they're still considered non-local)

		% Example in function called handle_local_function and a local function
		% called get_random_number. The handler function outputs an
		% informational message and then handles the passed-in function name.



-spec eval_expression(string) -> any().
eval_expression(Expression) ->
	{ok, Tokens, _} = erl_scan:string(Expression),
	{ok, Parsed} = erl_parse:parse_exprs(Tokens),

	% Allowing only certain local and non local functions to be called when
	% evaluating user input erlang expressions.
	{value, Result, _} = erl_eval:exprs(Parsed, [], 
		{value, fun handle_local_function/2},
		{value, fun handle_non_local_function/2}),
	Result.

% This handles local function calls within the module
-spec handle_local_function(atom(), list()) -> any().
handle_local_function(FunctionName, Arguments) ->
	io:format("Local call to ~p with ~p~n", [FunctionName, Arguments]),

	case FunctionName of
		% These are examples of functions that are permitted when being handled
		% and allowed by a separate function
		get_random_number -> get_random_number();
		what_time_is_it -> calendar:universal_time();
		are_we_there_yet -> "no";

		_ -> "Can't do that :p"
	end.

-spec get_random_number() -> integer().
get_random_number() ->
	4. % Chosen by fair dice roll; guaranteed to be random

% Usage of the new handler function

	% > c(lib_misc).

	% > lib_misc:evaluate_expression("get_random_number().").
		% Local call to get_random_number with []
		% 4

	% > lib_misc:evaluate_expression("what_time_is_it().").
		% Local call to what_time_is_it with []
		% {{2017,3,5},{15,21,53}}

	% > lib_misc:evaluate_expression("are_we_there_yet().").
		% "no"

	% > lib_misc:evaluate_expression("break_the_system().").
		% "Can't do that :p"

% -------------------------------------------------------------------------

% This handles only non-local function calls from outside modules or system
% modules
-spec handle_non_local_function(atom(), list()) -> any().
handle_non_local_function(
	{ModuleName, FunctionName}, Arguments) ->
	io:format("Non-local or local call to ~p with ~p~n", [{ModuleName,
		FunctionName}, Arguments]),
	% below we use apply(Module, Function, Arguments)
	% Module is the module that holds the function
	% Function is the function within the module to use
	% Arguments are the arguments for the function that will be passed
	case ModuleName of
		erlang ->
			case FunctionName of
				'>' -> apply(ModuleName, FunctionName, Arguments);
				'<' -> apply(ModuleName, FunctionName, Arguments);
				list_to_binary -> apply(ModuleName, FunctionName, Arguments);
				_ -> "nothing else to see for erlang"
			end;
		_ -> "dont try anything little stinker...."
	end.
