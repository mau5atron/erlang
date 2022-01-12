-module(chapter_notes).

% Chapter 9 Notes: Types

% Erlang has a type notation that we can use to define new data types and 
% add type annotations to our code. The type annotations make the code 
% easier to understand and maintain and can be used to detect errors at 
% compile time

% In this chapter we'll introduce the type notation and talk about two
% programs that can be used to find errors in our code.

% The programs we'll discuss are called "dialyzer" and "typer" and are
% included in the standard Erlang distribution. Dialyzer stands for
% "DIscrepancy AnaLYZer for ERlang programs," and it does precisely that
% which is implied by its name: 
	% it finds discrepencies in Erlang code. "typer" provides information
	% about the types used in your programs. Both the dialyzer and typer work
	% perfectly well with not type annotations at all, but if you add type
	% annotations to your program, the quality of the analysis performed by
	% these tools will be improved.


% 9.1 Specifying Data and Function Types
specifying_data_and_function_types() ->
	% We are going on a walking tour and are lucky enough to have a module that we can use to plan our walks. The module starts like this:

	% walks.erl
	% -module(walks).
	% -export([plan_route/2]).

	% -spec plan_route(point(), point()) -> route().
	% -type direction() :: north | south | east | west.
	% -type point() 		:: {integer(), integer()}.
	% -type route()			:: [{ go,direction(), integer() }].
	% ...

	% This module exports a function called plan_route/2. The input and 
	% return types for the function are specified in a type specification, 
	% and three new types are defined using type declarations. The are
	% interpreted as follows:

	% -spec plan_route(point(), point()) -> route()
		% Means that if the function plan_route/2 is called with two input
		% arguments, both of type point(), then it will return an object of
		% type route().

	% -type direction() :: north | south | east | west
		% Introduces a new type called direction() whose value is one of the
		% atoms north, south, east, or west.

	% -type point() :: {integer(), integer()}.
		% Means that the type point() is a tuple of two integers (integer() is
		% a predefined type)

	% -type route() :: [{go, direction(),integer()}].
		% Defines the type route() to be a list of 3 tuples, where each tuple
		% contains the atom go, an object of type direction(), and an integer.
		% The notation [X] means a list of type X.

	% from the type annotations alone we can imagine evaluating plan_route
	% and seeing something like this:

	% 1> walks:plan_route({1,10},{25,57}).
	% [{go, east, 24}, {go, north, 47}, ....]

	% -------------------------------------------------------------------------
	% bunch of irrelevant stuff here...
	% To add expressive power to the types, we can annotate them with
	% descriptive variables. For example, we could change the specification
	% of plan_route to the following:

	% -spec plan_route(From:: point(), To:: point()) -> ...
	% The names From and To in the type annotation give the user some idea as
	% to the role these assignments play in the function. They are also used
	% to link the names in the documentation to the variables in the type
	% annotations. The official Erlang documentation uses strict rules for
	% writing type annotations so that the names in the type annotations
	% correspond to the names used in the corresponding documentation.

	% Saying that our route starts at From and that From is a pair of
	% integers ma or may not be sufficient to document the function; it
	% depends upon the context. We could easily refine the type definitions
	% by adding more information. For example, by writing this:

	% -type angle() 		:: {Degrees::0..360, Minutes::0..60, Seconds::0..60}.
	% -type position() 	:: {latitude | longitude, angle()}.
	% -type plane_route1(From::position(), To::position()) -> ...

	% The new form gives a lot more information but again invites guesswork.
	% We might guess that units of the angles are in degrees, since the range
	% of allowed values is 0 to 360, but they might just be in radians, and
	% we would have guessed wrongly.

	% As the type annotations become longer, we might end up being more
	% precise at the expsense of increased verbosity. The increased size of
	% the annotations might make the code more difficult to read. Writing
	% good type annotations is as much of an art as writing good clear code
	% -- something that is very difficult and takes years of practice. It's a
	% form of zen meditation: the more you do it, the easier it becomes and
	% the better you get :)
	.

% 9.2 Erlang Type Notation
erlang_type_notation() ->
	% To make full use of the type system, we need to understand the type
	% grammar so we can read and write more precise type descriptions.

	% The grammar of Types
		% T1 :: A | B | C....

		% This means that T1 is defined to be one of A, B, or c.
		% Using this notation, we can define a subset of Erlang types as
		% follows:

			% Type :: any() | none() | pid() | port() | reference | [] | Atom |
			% binary() | float() | Fun | Integer | [Type] | Tuple | Union |
			% UserDefined

			% Union :: Type1 | Type2 | ...
			% Atom :: atom() | Erlang_Atom
			% Integer :: integer() | Min .. Max
			% Fun :: fun() | fun((...) -> Type)
			% Tuple :: tuple() | {T1, T2, .... Tn}

		% In the previous example, any() means any Erlang term, X() means an
		% Erlang object of type X, and the token none() is used to denote thte
		% type of a function that never returns.

		% The notation [X] denotes a list of type X, and {T1, T2, Tn..} denotes
		% a tuple of size n whose arguments are the type T1, T2, Tn.

		% New types can be defined with the following syntax:
		% -type NewTypeName(TVar1, TVar2, ... TVarN) :: Type

		% TVar1 to TVarN are optional type variables, and Type is a type
		% expression

		% Some examples:
			% -type onOff() 					:: on | off.
			% -type person() 					:: {person, name(), age()}.
			% -type people() 					:: [person()].
			% -type name() 						:: {firstname, string()}.
			% -type age() 						:: integer().
			% -type dict(Key, Val)		:: [{Key, Val}].

		% These rules say that, for example, {firstname, "dave"} is of type
		% name(), and [{person, {firstname, "john"}, 35}, {person, 
		% {firstname, "mary"}, 26}] is of type people(), and so on. The type
		% dict(Key,Val) shows the use of type variables and defines a
		% dictionary type to be a list of {Key, Val} tuples.

		% - Predefined Types
			% In addition to the type grammar, the following type aliases are
			% predefined:

			% -type term() :: any().
			% -type boolean() :: true | false.
			% -type byte() :: 0..255.
			% -type char() :: 0..16#10ffff.
			% -type number() :: integer() | float().
			% -type list() :: [any()].
			% -type maybe_improper_list() :: maybe_improper_list(any(), any()).
			% -type maybe_improper_list(T) :: maybe_improper_list(T, any()).
			% -type string() :: [char()].
			% -type nonempty_string() :: [char(), ...].
			% -type iolist() :: maybe_improper_list(byte() | binary() | iolist
			% (), binary() | []).

			% -type module() :: atom().
			% -type mfa() :: {atom(), atom(), atom()}.
			% -type node() :: atom().
			% -type timeout() :: infinity | non_neg_integer().
			% -type no_return() :: none().

			% maybe_improper_list is used to specify the types of lists whose
			% ultimate tail is non-nil. Such lists are rarely used, but it is
			% possible to specify their types!

			% There are also a small number of predefined types. 
			% non_neg_integer() is a non-negative integer, pos_integer() is a
			% positive integer, and neg_integer() is a negative integer. Finally,
			% the notation [X, ...] means a non-empty list of type X.

			% Now that we can define types, let's move on to function
			% specifications.
	.

	% Specifying the input and output Types of a Function
	specifying_the_input_and_output_types_of_a_function() ->
		% Function specifications say what the types of arguments to a function
		% are and what the type of the return value of the function is. A
		% function specification is written like this:

			% -spec functionName(T1, T2, Tn) -> Tret when
			% 	Ti :: Typei,
			% 	Tj :: Typej

		% Here T1, T2 ... Tn describe the types of the arguments to a
		% function, and Tret describes the type of the return value of the
		% function. Additional type variables can be introduced if necessary
		% after the optional when keyword.

		% We'll start with an example. The following type specification:

			% -spec file:open(FileName, Modes) -> {ok, Handle} | {error, Why}
			% when
			% FileName :: string(),
			% Modes	 	 :: [Mode],
			% Mode 		 :: read | write | ...
			% Handle 	 :: file_handle(),
			% Why 		 :: error_term().

		% says that if we open the file FileName, we should get a return value
		% that is either {ok, Handle} or {error, Why}. FileName is a string,
		% Modes is a list of Mode, and Mode is one of read, write, and so on.

		% The previous function specification could been written in a number of
		% equivalent ways; for example, we might have written the following and
		% not used a when qualifier:

		% -spec file:open(string(), [read|write|...] -> {ok, Handle} | 
		% {error, Why})

		% The problems with this are, first, that we lose the descriptive
		% variables FileName and Modes, and so on, and, second, that the type
		% specification becomes a lot longer and consequently more difficult to
		% read and format in printed documentation. In the documentation that
		% ideally follows the program we have no way to refer to the arguments
		% of the function if they are not named.

		% In the first way of writing the specification, we wrote the
		% following:
		% -spec file:open(FileName, Modes) -> {ok, Handle} | {error, Why} when
		% FileName :: string() ...

		% So, any documentation of this function could unambiguously refer to
		% the file that was being opened by using the name FileName. If we said
		% this:

		% -spec file:open(string(), [read|write|...]) -> {ok, Handle | 
		% {error, Why}}.

		% and dropped the when qualifier, then the documentation would have to
		% refer to the file that was being opened as "the first argument of
		% the open function" a circumlocution that is unnecessary in the first
		% way of writing the function specification.

		% Type variables can be used in arguments, as in the following
		% examples:

		% -spec lists:map(fun((A) -> B), [A]) -> [B].
		% -spec lists:filter(fun((X) -> bool()), [X]) -> [X].

		% This means that map takes a function from type A to B and list of
		% objects of type A and returns a list of type B objects, and so on.
		.


	% Exported and Local Types:
	exported_and_local_types() ->
		% Sometimes we want the definition of a type to be local to the module
		% where the type is defined; in other circumstances, we want to export
		% the type to another module. Imagine two modules a and b. Module a
		% produces objects of type rich_text, and module b manipulates these
		% objects. In module a, we make the following annotations.

			% -module(a).
			% -type rich_text() :: [{font(), char()}].
			% -type font()			:: integer().
			% -export_type([rich_text/0, font/0]).

		% Not only do we declare a rich text and font type, we also export them
		% using an -export_type(...) annotation.

		% Suppose module b manipulates instances of rich text; there might be
		% some function rich_text_length that computes the length of a
		% rich-text object. We could write the type specification for this
		% function as follows:

		% -module(b).
		% -spec rich_text_length(a:rich_text()) -> integer().

		% The input argument to rich_text_length uses the fully qualified type
		% name, a:rich_text(), which means the type rich_text() exported from
		% the module a.
		.

	% Opaque Types:
	opaque_types() ->
		% In the previous section, two modules, a and b, cooperate by
		% manipulating the internal structure of the object that represents
		% rich text. We may, however, want to hide the internal details of the
		% rich-text data structure so that only the module that creates the
		% data structure knows the details of the type. This is best explained
		% with an example.

		% Assume module a starts like this:

		% -module(a).
		% -opaque rich_text() :: [{font(), char()}].
		% -export_type([rich_text/0]).
		% -export([make_text/1, bounding_box/1]).
		% -spec make_text(string()) -> rich_text().
		% -spec bounding_box(rich_text()) -> {Height::integer(),
		% Width::integer()}.
		% ...

		% The following statement
		% -opaque rich_text() :: [{font(), char()}].

		% Creates an opaque types called rich_text(). Now let's look at some
		% code that tries to manipulate rich-text objects:
		% -module(b).

			% do_this() ->
				% X = a:make_text("Hello world"),
				% {W, H} = a:bounding_box(X)

		% The module b never needs to know anything about the internal
		% structure of the variable X. X is created inside the module a and is
		% passed back into a when we call bounding_box(X).

		% Now suppose we write code that makes use of some knowledge about the
		% shape of the rich_text object. For exmple, suppose we create a
		% rich-text object and then ask what fonts are needed to render the
		% object. We might write this:

			% -module(c).
			% fonts_in(Str) ->
				% X = a:make_text(Str),
				% [F || {F,_} <- X]

		% In the list comprehension we "know" that X is a list of 2-tuples. In
		% the module a we declared the return type of make_text to be an opaque
		% type, which means we are not supposed to know anything about the
		% internal structure of the type. Making use of the internal structure
		% of the type is called an abstraction violation and called be detected
		% by the dialyzer if we correctly declare the visibility of the types
		% in the functions involved.
 		.

% 9.3 A Session with the Dialyzer
session_with_dialyzer() ->
	% The first time you run the dialyzer you need to build a cache of all
	% the types in the standard libraries that you intend to use. This is a
	% once-onyl operation. If you launch the dialyzer, it tells you what to
	% do.

	% $ dialyzer

	% Kinds of errors we can expect to be reported by the dialyzer:
		% - Incorrect Use of a BIF Return Value
		% - Incorrect Arguments to a BIF
		% - Incorrect Program Logic
	.

	% Working with the Dialyzer:
	working_with_the_dialyzer() ->
		% Using the dialyzer to check your programs for type errors involves a
		% particular workflow. What you should not do is write the entire
		% program with no type annotations and then, when you think that it is
		% ready, go back and add type annotations to everything and then run
		% the dialyzer. If you do this, you will probably get a large number of
		% confusing errors and not know where to start looking to fix the
		% errors.

		% The best way to work with the dialyzer is to use it at every stage of
		% development. When you start writing a new module, think about the
		% types first nad declare them before you write your code. Write type
		% specifications for all the exported functions in your module. Do this
		% first before you start writing the code. You can comment out the type
		% specs of the functions that you have not yet implemented and then
		% uncomment them as you implement the functions.

		% Now write your functions, one at a time, and check after you have
		% written each new function to see whether the dialyzer can find any
		% errors in your program. Add type specifications if the function is
		% exported. If the function is not exported, then add type
		% specifications if you think this will help the type analysis or help
		% us understand the program (remember, type annotations provide good
		% documentation of the program). If the dialyzer finds any errors, then
		% stop and think and find out exactly what the error means.
		.

		% Things that confuse the Dialyzer:
			% Avoid using -compile(export_all).
				% If you export all functions in the module, the dialyzer might not
				% be able to reason about some of the arguments to some of your
				% exported functions; they could be called form anywhere and have 
				% any type. The values of these arguments can propagate to other
				% functions in the module and give confusing errors.

			% Provide detailed type specifications for all the arguments to the
			% exported functions in the module. 
				% Try to tightly constrain the arguments to exported functions as
				% much as possible. For example, at first sight you might reason
				% that an argument to a function is an integer, but after a little
				% more thought, you might decide that the argument is a positive
				% integer or even a bounded integer. The more precise you can be
				% about your types, the better results you will get with the
				% dialyzer. Also, add precise guard tests to your code if possible.
				% This will help with the program analysis and will often help the
				% compiler generate better-quality code.

			% Provide default arguments to all elements in a record definition.
			% If you don't provide a default, the atom undefined is taken as the
			% default, and this type will start propagating through the program
			% and might produce strange type errors.

			% Using anonymous variables in arguments to a function often results
			% in types that are far less specific than you had intended; try to
			% constrain variables as much as possible.

% 9.4 Type Inference and Success Typing:

% 9.5 Limitations of the Type System
limitations_of_the_type_system() ->
	% Let's look at what happens when we add type specifications to code.
	% We'll start with the well-known boolean and function.

	% and is true if both its arguments are true, and it is false if any of
	% its arguments are false. We'll define a function myand1 (which is
	% supposed to work like and) as follows:
	.
