-module(intercepting_non_local_function_calls).
-export([]).

% Intercepting Non-Local Function Calls:

	% Similarly we can supply a function to erl_val:exprs through which all calls to non-local functions will be passed. (Anything outside of the current module, including BIFs and even the operators used in comparisons.)

	% Similar to what we did before, we have extended to handle non-local functions. Notice how we have to explicitly handle the > and < comparison operators that are part of the Erlang module, howwe can redirect non-existent functions to existing ones, and how we can display a message if a function is unsupported.

-spec eval_expression(string) -> any().
eval_expression(Expression) ->
	{ok, Tokens, _} = erl_scan:string(Expression),
	{ok, Parsed} = erl_parse:parse_exprs(Tokens),
	{value, Result, _} = erl_eval:exprs(Parsed, [], 
			{value, fun handle_local_function/2},
			{value, fun handle_non_local_function/2}),
	Result.

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

-spec handle_non_local_function(atom(), list()) -> any().
handle_non_local_function({ModuleName, FunctionName}, Arguments) ->
	io:format("Non-local call to ~p with ~p~n", [FunctionName, Arguments]),
	case ModuleName of
		% only non local erlang standard library functions that are permitted
		erlang ->
			case FunctionName of
				'>' -> apply(ModuleName, FunctionName, Arguments);
				'<' -> apply(ModuleName, FunctionName, Arguments);
				list_to_binary -> apply(ModuleName, FunctionName, Arguments);
				_ -> "uuhuhuefhuehfefeh"
			end;

		calendar ->
			case FunctionName of
				universal_time -> calendar:universal_time();
				lets_pretend_this_returns_four -> 4;
				something_something -> "what calendar are you using?";
				_ -> "dont worry boutit"
			end;

		% can also add our own local modules here

		_ -> "dont try anything little stinker...." % this is a catch all I think
	end.

-spec get_random_number() -> integer().
get_random_number() -> 4.

% Writing the functions above to be used as local and non-local module and
% function usage.

-spec handle_local_and_non_local_function(atom(), list()) -> any().
% {ModuleName, FunctionName} pair is a tuple where FunctionName is part of
% a module (first item in {ModuleName, ....} tuple)

% Then Arguments function argument is the list
handle_local_and_non_local_function(
	{ModuleName, FunctionName}, Arguments
) ->
	io:format("Non-local or local call to ~p with ~p~n", [FunctionName, Arguments]),
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

		lib_misc ->
			case FunctionName of
				eval_expression -> apply(ModuleName, FunctionName, Arguments);
				_ -> "nothing to see in lib_misc"
			end;
	end.
