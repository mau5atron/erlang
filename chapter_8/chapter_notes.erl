-module(chapter_notes).

the_rest_of_sequential_programming() ->
	% functions:

	% Apply:
		% This computes the value of a function fro its name and arguments 
		% when the function and module name are computed dynamically.

	% Arithmetic expressions:
		% All legal arithmetic expressions are defined here

	% Arity:
		% The arity of a function is a number of arguments that a function accepts.

	% Attributes:
		% This section covers the syntax and interpretation of the Erlang module attributes

	% Block expressions:
		% These are expressions using begin and end.

	% Booleans:
		% These are things represented by the atoms true or false

	% Boolean Expressions:
		% This section covers all the the boolean expressions.

	% Character set:
		% This is the character set that Erlang uses

	% Comments:
		% This section covers the syntax of comments

	% Dynamic Code Loading:
		% This section covers how dynamic code loading works.

	% The Erlang Preprocessor:
		% This section covers what happens before Erlang is compiled.

	% Escape Sequences:
		% This section covers the syntax of the escape sequences used in 
		% strings and atoms

	% Expressions and expression sequences:
		% This section covers how expressions and expression sequences are
		% defined

	% Function references:
		% This section covers hwo to refer to functions

	% Include files:
		% This section covers who to include files at compile time

	% List addition and subtraction operators:
		% These are ++ and --

	% Macros:
		% This section covers the Erlang macro processor.

	% Match Operator in Patterns:
		% This section covers how the match operator = can be used in patterns

	% Numbers:
		% This section covers the syntax of numbers

	% Operator precedence:
		% This section covers the priority and associativity of all the Erlang operators

	% The process dictionary:
		% Each erlang process has a local area o destructive storage, which can be useful sometimes

	% References:
		% References are unique symbols

	% Short-circuit boolean expressions:
		% These are boolean expressions that are not fully evaluated

	% Term comparisons:
		% This section covers all the term comparison operators and the lexical ordering of terms

	% Tuple modules:
		% These provide a method of creating "stateful" modules

	% Underscore variables:
		% These are variables that the compiler treats in a special way.
	.

apply_function() ->
	% apply(Mod, Func, [Arg1, Arg2, ...ArgN]) applies the function Func in
	% the
	% module Mod ot the arguments Arg1, Arg2, ArgN.
	% It is equivalent to calling:
	% Mod:Func(Arg1, Arg2, ArgN)

	% apply lets you call a function in a module, passing it arguments. What
	% makes it different from calling the function directly is that the
	% module name and / or the function name can be computed dynamically

	% All the Erlang BIFs can also be calle using apply by assuming that they
	% belong to the module erlang. So, to build a dynamic call to a BIF, we
	% might write the following:

	% 1> apply(erlang, atom_to_list, [hello]).
	% "hello"

	% WARNING: The use of apply should be avoided if possible. When the
	% number of arguments to a function is known in advance, it is much
	% better to use a call of the form M:F(Arg1, Arg2, ArgN) than apply. When
	% calls to functions are built using apply, many analysis tools cannot
	% work out what is happening, and certain compiler optimizations cannot
	% be made. So, use apply sparingly and only when absolutely needed.

	% The Mod argument to apply does not have to be an atom; it can also be a
	% tuple.

	% If we call this:

	% {Mod, P1, P2, ..., Pn}:Func(A1, A2, An)

	% then what actually gets called is the following function:

	% Mod:Func(A1, A2, An, {Mod, P1, P2, Pn})
	.

arithmetic_expressions() ->
	% need to look at the notes on page 116, not writing all that lol

	% Note: associated with each operator is a priority. The order of
	% evaluation of a complex arithmmetic expression depends upon the
	% priority of the operator: all operations with priority 1 operators are
	% evaluated first, then all operators with priority 2, and so on.

	% You can use parentheses to change the default order of evaluation - any
	% paranthesized expressions are evaluated first. Operators with equal
	% priorities are treated as left associative and are evaluated from left
	% to right.
	.

arity_of_a_function() ->
	% 8.3 Arity:

	% The arity of a function is the number of arguments that the function
	% has. In Erlang, two functions with the same name and different arity in
	% the same module represent entirely different functions. They have
	% nothing to do with each other apart from the coincidental use of the
	% same name.

	% by convention Erlang programmers often use functions with the same name
	% and different arities as auxiliary function

	% For example:

	% sum(L) -> sum(L, 0).
	% sum([], N) -> N;
	% sum([H|T], N) -> sum(T, H+N).

	% What you see here are two different functions, one with arity 1 and the
	% second with arity 2.

	% The function sum(L) sums the elements of a list L. It makes use of an
	% auxiliary routine called sum/2, but this could have been called
	% anything. You could have called the auxiliary routine hedgehog/2, and
	% the meaning of the program would be the same. sum/2 is a better choice
	% of name, though since it gives the reader of your program a clue as to
	% what's going on and since you don't have to invent a new name (which
	% is always difficult).

	% Often, we hide auxiliary functions by not exporting them. So, a module
	% defining the sum(L) would export only sum/1 and not sum/2
	.

attributes() ->
	% Module attributes have the syntax -AtomTag(...) and are used to define
	% certain properties of a file. (Note: -record() and -include(...) have
	% a similar syntax but are not considered module attributes.) There are
	% two types of module attributes: predefined and user-defined.
	.

	predefined_module_attributes() ->
		% The following module attributes have predefined meanings and must be
		% placed before any function definitions:

		% -module(modname)
			% The module declaration. modname must be an atom. This attribute
			% must be the first attribute in the file. Conventionally the code
			% for modname should be stored i na file called modname.erl. If you
			% do not do this, then automatic code loading will not work
			% correctly;

		% -import(Mod. [Name1/Arity1, Name2/Arity2,...]).
			% The import declaration specifies which functions are to be imported
			% into a module. The previous declaration means that the functions
			% Name1 with Arity1 arguments, Name2, with Arity2 arguments, and so
			% on, are to be imported from the module Mod.

			% Once a function has been imported from a module, then calling the
			% function can be achieved without specifying the module name. Here's
			% an example:

			-module(abc).
			-import(lists, [map/2]).

			f(L) ->
				L1 = map(fun(X) -> 2*X end, L),
				lists:sum(L1).

			% The call to map needs no qualifying module name, whereas to call
			% sum we need to include the module name in the function call.

		% -export([Name1/Arity1, Name2/Arity2,...]).
			% Export the functions Name1/Arity1, Name2/Arity2, and so on, from
			% the current module. only exported functions can be called from
			% outside a module.

			% Here's an example:

			abc.erl
			-module(abc).
			-export([a/2, b/1]).

			a(X, Y) -> c(X) + a(Y).
			a(X) -> 2 * X.
			b(X) -> X * X.
			c(X) -> 3 * X.

			% The export declaration means that only a/2 and b/1 can be called
			% from outside the module abc. So, for example, calling abc:a(5) from
			% the shell (which is outside the module) will result in error
			% because a/1 is not exported from the module.

			1> abc:a(1,2).
			7
			2> abc:b(12).
			144
			3> abc:a(5).
			% ** exception error: undefined function abc:a/1

		% -compile(Options).
			% Add Options to the list of compiler options. Options is a single
			% compiler option or a list of compiler options (these are described
			% in the manual page for the module compile)

			% NOTE: the compiler option -compile(export_all) is often used while
			% debugging programs. This exports all functions from the module
			% without having to explicitly use the -export annotation.

		% -vsn(Version)
			% Specify a module version. Version is any literal term. The value of
			% Version has no particular syntax or meaning, but it can be used by
			% analysis programs or for documentation purposes.
		.

	user_defined_attributes() ->
		% The syntax of a user-defined attribute is as follows:
		% -SomeTag(Value).
			% SomeTag must be an atom, and Value must be a literal term. The
			% values of the module attributes are compiled into the module and
			% can be extracted at runtime. Here's an example of a module
			% containing some user-defined attributes:

			% attrs.erl
			% -module(attrs).
			% -vsn(1234).
			% -author({joe,armstrong}).
			% -purpose("example of attributes").
			% -export([fac/1]).

			% fac(1) -> 1;
			% fac(N) -> N * fac(N-1).

			% We can extract the attributes as follows:

			1> attrs:module_info().
			[{exports,[{fac,1},{module_info,0},{module_info,1}]},
			{imports,[]},
			{attributes,[{vsn,[1234]},
									 {author, [{joe,armostrong}]},
									 {purpose,"example of attributes"}
									 etc.....
									]}
			]

			% The user-defined attributes contained in the source code file
			% reappear as a subterm of {attributes,....}. The tuple 
			% {compile,....} contains information that was added by the compiler.
			% The value {version, "4.5.5"} is the version of the compiler and
			% should not be confused with the vsn tag defined in the module
			% attributes. In the previous example, attrs:module_info() returns a
			% property list of all the metadata associated with a compiled
			% module. attrs:module_info(X), where X is one of exports, imports,
			% attributes, or compile, returns the individual attribute associated
			% with the module.

			% Note that the functions module_info/0 and module_info/1 are
			% automatically created every time a module is compiled.

			% To run attrs:module_info, we have to load the beam code for the
			% module attrs into the Erlang VM. We can extract the same
			% information without loading the module by using the module
			% beam_lib.

			3> beam_lib:chunks("attrs.beam", [attributes]).
			{ok,{attrs,[{attributes,[{author,[{joe,armstrong}]},
															 {purpose, "example of attributes"},
															 {vsn,[1234]}
															]}]}}

			% beam_lib:chunks extracts the attribute data from a module without
			% loading the code for the module.
		.

% 8.5 Block Expressions:

block_expressions() ->
	% Block expressions are used when the Erlang syntax requires a single
	% expression, but we want to have a sequence of expressions at this point
	% in the code. For example, in a list comprehension of the form [E ||
	% ...], the syntax requires E to be a single expression, but we might
	% want to do several things in E

	begin
		Expr1,
		....,
		ExprN
	end

	% You can use block expressions to group a sequence of expressions,
	% similar to a clause body. The value of a begin ... end block is the
	% value of the last expression in the block.
	.

% 8.6 Booleans:

booleans() ->
	% There is no distinct boolean type in Erlang; instead, the atoms true
	% and false are given a special interpretation and are used to represent 
	% boolean literals.

	% Sometimes we write functions that return one of two possible atomic
	% values. When this happens, it's good practice to make sure they return
	% a boolean. It's also a good idea to name your functions to make it
	% clear that they return a boolean.
	.

% 8.7 Boolean Expressions:
boolean_expressions() ->
	% There are four possible boolean expressions.
		% not B1: Logical not
		% B1 and B2: Logical and
		% B1 or B2: Logical or
		% B1 xor B2: Logical xor

	% In all of these, B1 and B2 must be boolean literals or expressions that
	% evaluate to booleans. Here are some examples:

	1> not true.
	false
	2> true and false.
	false
	3> true or false.
	true
	4> (2 > 1) or (3 > 4)
	true
	.

% 8.8 Character Set
character_set() ->
	% Since Erlang version R16B, Erlang source code files are assumed to be
	% encoded in the UTF-8 character set. Prior to this, the ISO-8859-1 
	% (Latin-1) character set was used. This means all UTF-8 printable
	% characters can be used in source code files without using any escape
	% sequences.

	% Internally Erlang has no character data type. Strings don't really
	% exist but instead are represented by lists of integers. Unicode strings
	% can be represented by lists of integers without any problems.
	.

% 8.9 Comments

comments() ->
	% Comments in Erlang start with a percent character (%) and extend to the
	% end of line. There are no block comments.

	% Note: You'll often see double percent characters (%%) in code examples.
	% Double percent marks are recognized in the Emacs erlang-mode and enable
	% automatic indentation of commented lines.
	.

% 8.10 Dynamic Code Loading

dynamic_code_loading() ->
	% Dynamic code loading is one of the most surprising features built into
	% the heart of Erlang. The nice part is that it just works without you
	% really being aware of what's happening in the background.

	% The idea is simple: every time we call someModule:someFunction(), we'll
	% always call the latest version of the function in the latest version of
	% the module, even if we recompile the module while code is running in
	% this module.

	% If a calls b in a loop and we recompile b, then a will automatically
	% call the new version of b the next time b is called. If many different
	% processes are running and all of them call b, then all of them will
	% call the new version of b if b is recompiled.

	% Look at code examples.

	% Erlang can have two versions of a module running at any one time, the
	% current version and an old version. When you recompile a module, any
	% process running code in the old version is killed, the current version
	% becomes the old version, and the newly compiled module becomes the
	% current version. Think of this shift register with two versions of the
	% code. As we add new code, the oldest version is junked. Some processes
	% can run old versions of the code while other processes can
	% simultaneously run new versions of the code.

	% Read the purge_module documentation for more details.
	.

% 8.11 Erlang Preprocessor

erlang_preprocessor() ->
	% Before an Erlang module is compiled, it is automatically processed by
	% the Erlang preprocessor. The preprocessor expands any macros that might
	% be in the source file and inserts any necessary include files.

	% Ordinarily, you won't need to look at the output of the preprocessor,
	% but in exceptional circumstances (for example, when debugging a faulty
	% macro), you might want to save the output of the preprocessor. To see
	% the result of preprocessing the module some_module.erl, give the OS
	% shell command.

	% $ erlc -P some_module.erl

	% This produces a listing file called some_module.P
	.

% 8.12 Escape Sequences:

escape_sequences() ->
	% Within strings and quoted atoms, you can use escape sequences to enter
	% any nonprintable characters. All the possible escape sequences are
	% shown in table 4, Escape sequences, page 127

	% ~Bunch of examples of character escaping~
	.

% 8.13 Expressions and Expression Sequences
expression_and_expression_sequences() ->
	% In Erlang, anything that can be evaluated to produce a value is called
	% an expression. This means things such as catch, if, and try ... catch
	% are expressions. Things such as record declarations and module
	% attributes cannot be evaluated, so they are not expressions.

	% Expression sequences are sequences of expressions separated by commas.
	% They are found all over the place immediately following an -> arrow.
	% The value of the expression sequence E1, E2, En is defined by the value
	% of the last expression in the sequence. This is computed using any
	% bindings created when computing the values of E1, E2, and so on. This
	% is equivalent to progn in LISP.
	.

% 8.14 Function References:
function_references() ->
	% Often we want to refer to a function that is defined in the current
	% module or in some external module. You can use the following notation
	% for this:

	% fun LocalFunc/Arity
		% This is used to refer to the local function called LocalFunc with
		% Arity arguments in the current module.

	% fun Mod:RemoteFunc/Arity
		% This is used to refer to an external function called RemoteFunc with
		% Arity arguments in the module Mod.

	% Here's an example of a function reference in the current module.
	% -module(x1).
	% -export([square/1, ....]).

	% square(X) -> X * X.
	% double(L) -> lists:map(fun square/1, L).

	% If we wanted to calll a function in a remote module, we could refer to
	% the function as in the following example:

	% -module(x2).
	% double(L) -> lists:map(fun x1:square/1, L).

	% fun x1:square/1 means the function square/1 in the module x1.

	% Note that function references that include the module name provide
	% switch-over points for dynamic code upgrade

	% Look at section 8.10 for dynamic code loading
	.

% 8.15 Include Files

include_files() ->
	% Files can be included with the following syntax:
	% -include(Filename).

	% In Erlang, the convention is that include files have the extension
	% .hrl. The FileName should contain an absolute or relative path so that
	% the preprocessor can locate the appropriate file. Library header files
	% can be included with the following syntax:

	% -include_lib(Name).
	% Here's an example:
	% -include_lib("kernel/include/file.hrl").

	% In this case, the Erlang compiler will find the appropriate include
	% files. (kernel, in the previous example, refers to the application
	% that defines this header file).

	% Include files usually contain record definitions. If many modules need
	% to share common record definitions, then the common record definitions
	% are put into include files that are included by all the modules that
	% need these definitions
	.


% 8.16 List Operations ++ and --

list_operations() ->
	% ++ and -- are infix operators for list addition and subtraction.
	% A ++ B adds (that is, appends) A and B

	% A -- B subtracts the list B from the list A. Subtraction means that
	% every element in B is removed from A.

	% NOTE: if some symbol X occurs only K times in B, then only the first K
	% occurences of X in A will be removed.

	% Here are some examples:

	% 1> [1,2,3] ++ [4,5,6].
	% [1,2,3,4,5,6]

	% 2> [a,b,c,1,d,e,1,x,y,1] -- [1].
	% [a,b,c,d,e,1,x,y,1

	% 3> [a,b,c,1,d,e,1,x,y,1] -- [1,1].
	% [a,b,c,d,e,x,y,1]

	% 4> [a,b,c,1,d,e,1,x,y,1] -- [1,1,1].
	% [a,b,c,d,e,x,y]

	% 5> [a,b,c,1,d,e,1,x,y,1] -- [1,1,1,1].
	% [a,b,c,d,e,x,y]

	% ++ can also be used in patterns. When matching strings, we can write
	% patterns such as the following:

	% f("begin" ++ T) -> ...
	% f("end" ++ T) -> ...

	% The pattern in the first clause is expanded into [$b,$e,$g,$i,$n|T].
	.


% 8.17 Macros:

macros() ->
	% Erlang macros are written as shown here:
	% -define(Constant,Replacement).
	% -define(Func(Var1, Var2, VarN), Replacement).

	% Macros are expanded by the Erlang preprocessor epp when an expression
	% of the form ?MacroName is encountered. Variables occurring in the macro
	% definition match complete forms in the corresponding site of the macro
	% call.

	% -define(macro1(X,Y), {a, X, Y}).

	% The expands into this:

	% foo(A) ->
	% 	{a, A + 10, b}.

	% In addition, a number of predefined macros provide information about
	% the current module. They are as follows:

		% ?FILE expands to the current filename
		% ?MODULE expands to the current module name
		% ?LINE expands to the current line number

	% Control flow in Macros
		% Inside a module, the following directives are supported; you can use
		% them to control macro expansion:


		% -undef(Macro).
			% Undefines the macro; after this you cannot call the macro.

		% -ifdef(Macro).
			% Evaluates the following lines only if Macro has been defined.

		% ifndef(Macro).
			% Evaluates the following lines only if Macro is undefined.

		% -else.
			% Allowed after an ifdef or ifndef statement. If the condition was
			% false, the statements following else are evaluated.

		% -endif.
			% Marks the end of an ifdef or ifndef statement

		% Conditional macros must be properly nested. They are conventionally
		% grouped as follows:

		% -ifdef(<FlagName>).
		% -define(...).
		% -else.
		% -define(...).
		% -endif.

		% We can use these macros to define a DEBUG macro. Here's an example:

		% m1.erl
		% -module(m1).
		% export([loop/1]).

		% -ifdef(debug_flag).
		% -define(DEBUG(X), io:format("DEBUG ~p:~p ~p~n", [?MODULE, ?LINE, X])).
		% -else.
		% -define(DEBUG(X), void).
		% -endif.

		% loop(0) ->
				% done;

		% loop(N) ->
				% ?DEBUG(N),
				% loop(N-1).

	% NOTE: io:format(String, [Args]) prints out the variables in [Args] in
	% the Erlang shell according to the formatting information in String. The
	% formatting codes are preceded by a ~ symbol. ~p is short for pretty
	% print, and ~n produces a newline. io:format understands an extremely
	% large number of formatting options; for more information, see page 251
	% on "Writing a List of Terms to a File"

	% To enable the macro, we set the debug_flag when we compile the code.
	% This is done with an additional argument to c/2 as follows:

	1> c(m1, {d, debug_flag}).
	{ok, m1}

	2> m1:loop(4).
	DEBUG m1:13 4
	DEBUG m1:13 3
	DEBUG m1:13 2
	DEBUG m1:13 1
	done

	% If debug_flag is not set, the macro just expands to the atom void. This
	% choice of name has no significance; it's just a reminder to you that
	% nobody is interested in the value of the macro.
	.

% 8.18 Match Operator in Patterns
match_operator_in_patterns() ->
	% Let's suppose we have some code like this:
	% func1([{tag1, A, B} | T]) ->
		% ...
		% ... f(..., {tag1, A, B}, ...)
		% ...

	% In line 1, we pattern match the term {tag1, A, B}, and in line 3, we
	% call f with an argument that is {tag1, A, B}. When we do this, the
	% system rebuilds the term {tag1, A, B}. A much more efficient and less
	% error-prone way to do this is to assign the pattern to a temporary
	% variable, Z, and pass this into f, like this:

	% func1([{tag1, A, B}=Z|T]) ->
		% ... f(... Z, ...)

	% The match operator can be used at any point in the pattern, so if we
	% have two terms that need rebuilding, such as in this code:

	% func1([{tag, {one, A}, B} | T]) ->
		% f(..., {tag, {one, A}, B}, ...),
		% g(..., {one, A}), ...)

	% then we could introduce two new variables, Z1 and Z2, and write the
	% following:

	% func1([{tag, {one, A}=Z1, B}=Z2|T]) ->
		% ... f(..., Z2, ....),
		% ... g(..., Z1, ....),
		% ...
	.


% 8.19 Numbers:
numbers() ->
	% Numbers in Erlang are either integers or floats.

	% Integers
		% Integer arithmetic is exact, and the number of digits that can be
		% represented in an integer is limited only by available memory.

		% Integers are written with one of three different syntaxes.

	% Conventional Syntax:
		% Here are integers are written as you expect. For example, 12, 123456,
		% and -23452 are all integers.

	% Base K integers:
		% Integers in a number base other than ten written with the syntax
		% K#Digits; thus, we can write a number in binary as 2#00101010 or a
		% number in hexadecimal as 16#af6bfa23. For bases greater than ten, the
		% characters abc... (or ABC...) represent the numbers 10,11,12, and so
		% on. The highest number base is 36.

	% $ syntax:
		% The syntax $C represents the integer code for the ASCII characte C.
		% Thus, $a is short for 97, $1 is short for 49, and so on.

		% Immediately after the $ we can also use any of the escape sequences
		% describe on page 127. Thus, $\n is 10, $\^c is 3, and so on.

	% Here are some examples of integers:
		% 0 -65 2#010001110 -8#377 16#fe34 36#wow

		% Their values are 0, -65, 142, -255, 65076, 65076, and 42368,
		% respectively
	.

% floats:
floats() ->
	% A floating-point number has five parts: an optional sign, a whole
	% number part, a decimal point, a fractional part, and an exponent part.

	% Here are some examples of floats:
	% 1.0 3.14159 -2.3e+6 23.56-27

	% After parsing, the floating point numbers are represented internally in
	% IEEE 754 64-bit format. Real numbers with absolute value in the range
	% 10^-323 ti 10^308 can be represented by an Erlang float.
	.

% 8.20 Operator Precedence:
operator_precedence() ->
	% Table for more info on page 133
	% Operator precedence and associativity are used to determine the
	% evaluation order in unparanthesized expressions.
	.

% 8.21 The process Dictionary:
the_process_dictionary() -> % USE SPARINGLY
	% Each process in Erlang has its own private data store called the
	% process dictionary. The process dictionary is an associative array (in
	% other languages this might be called a map, hashmap, or hash table)
	% composed of a collection of keys and values. Each key has only one
	% value.

	% The dictionary can be manipulated using the following BIFs:

	% put(Key, Value) -> OldValue
		% Add a Key, Value association to the process dictionary. The value of
		% put is OldValue, which is the previous value associated with Key. If
		% there was no previous value, the atom undefined is returned.

	% get(Key) -> Value
		% Look up the value of key. If there is a Key, Value association in the
		% dictionary, return Value; otherwise, return the atom undefined.

	% get() -> [{Key, Value}]
		% Return the entire dictionary as a list of {Key, Value} tuples.

	% get_keys(Value) -> [Key]
		% Return a list of keys that have the values Value in the dictionary

	% erase(Key) -> Value
		% Return the value associated with Key or the atom undefined if there
		% is no value associated with Key. Finally, erase the value associated
		% with Key.

	% erase() -> [{Key, Value}]
		% Erase the entire process dictionary. The return value is a list of 
		% {Key, Value} tuples representing the state of the dictionary before
		% it was erased.

	% Example:
	1> erase()
	[]
	2> put(x, 20)
	undefined
	3> get(x)
	20
	4> get(y)
	undefined
	5> put(y, 40)
	undefined
	6> get(y)
	40
	7> get()
	[{y,40}, {x,20}]
	8> erase(x)
	20
	9> get()
	[{y,40}]

	% Variables in the process dictionary behave pretty much like
	% conventional mutable variables in imperitive languages. If you use the
	% process dictionary, your code will no longer be side effect free, and
	% all the benefits of using nondestructive variables discussed previously
	% do not apply. For this reason, you should use the process dictionary
	% sparingly.

	% NOTE:
	% I rarely use the process dictionary. Using the process dictionary can
	% introduce subtle bugs into your program and make it difficult to debug.
	% One form of usage that I do approve of is to use the process dictionary
	% to store "write-once" variables. If a key acquires a value exactly once
	% and does not change the value, then storing it in the process
	% dictionary is sometimes acceptable.
	.

% 8.22 References:
references() ->
	% References are globally unique Erlang terms. They are created with the
	% BIF erlang:make_ref(). References are useful for creating unique tags
	% that can be included in data and then at a later stage compared for
	% equality. For example, a bug-tracking system might add a reference to
	% each new bug report in order to give it a unique identity.
	.

% 8.23 Short-Circuit Boolean Expressions
short_circuit_boolean_expressions() ->
	% Short-Circuit Boolean expressions are boolean expressions whose
	% arguments are evaluated only when necessary.

	% There are two "short-circuit" boolean expressions

	% Expr1 orelse Expr2
		% This first evaluates Expr1. If Expr1 evaluates to true, Expr2 is not
		% evaluated. If Expr1 evalutes to false, Expr2 is evaluated

	% Expr1 andalso Expr2
		% The first evaluates Expr1. If Expr1 evaluates to true, Expr2 is
		% evaluated. If Expr1 evaluates to false, Expr2 is not evaluated.

	% NOTE:
	% In the corresponding boolean expressions (A or B; A and B), both the
	% arguments are always evaluated, even if the truth value of the
	% expression can be determined by evaluating only the first expression
	.

% 8.24 Term Comparisons:
term_comparisons() ->
	% There are 8 possible term comparison operations

	% For the purposes of comparisons, a total ordering is defined over all
	% terms. This is defined so that the following is true:

	% number < atom < reference < fun < port < pid, tuple (and record) < map
	% < list < binary

	% This means that, for example, a number (any number) is defined to be
	% smaller than an atom (any atom), that a single tuple is greater than an
	% atom, and so on. (Note that for the purposes of ordering, ports and
	% PIDs are included in this list)

	% Having a total order over all terms means we can sort lists of any type
	% and build efficient data access routines based on the sort order of the
	% keys.

	% All the term comparison operators, with the exception of =:= and =/=,
	% behave in the following way if their arguments are numbers:

		% If one argument is an integer and the other is a float, then the
		% integer is converted to a float before the comparison is performed.

		% If both arguments are integers or if both arguments are floats, then
		% the arguments are used "as is", that is, without conversion


	% NOTE:
	% You should also be really careful about using == (especially if you're
	% a C or Java programmer). In 99 out of 100 cases, you should be using
	% =:=. == is useful only when comparing floats with integers. =:= is for
	% testing whether two terms are identical.

	% Identical means having the same value (like the Common Lisp EQUAL).
	% Since values are immutable, this does not imply any notion of pointer
	% identity. If in doubt, use =:=, and be suspicious if you see ==. 

	% Note that a similar comment applies to using /= and =/=, where /= means
	% "not equal to" and =/= means "not identical"

	% NOTE:
	% In a lot of library and published code, you'll see == used when the
	% operator should have been =:=. Fortunately, this kind of error does not
	% often result in an incorrect program, since if the arguments to == do
	% not contain any floats, then the behaviors of the two operators are the
	% same.

	% You should also be aware that function clause matching always implies
	% exact pattern matching, so if you define a fun F = fun(12) -> ... end,
	% then trying to evaluate F(12.0) will fail.

	% OPERATORS AND MEANINGS

	% X > Y -> X is greater than Y
	% X < Y -> X is less than Y
	% X =< Y -> X is equal to or less than Y
	% X >= Y -> X is greater than or equal to Y
	% X == Y -> X is equal to Y
	% X /= Y -> X is not equal to Y
	% X =:= Y -> X is identical to Y
	% X =/= Y -> X is not identical to Y
	.

% 8.25 Tuple Modules:
tuple_modules() ->
	% When we call M:f(Arg1, Arg2, ArgN), we have assumed that M is a module
	% name. But M can also be a tuple of the form {Mod1, X1, X2, Xn}, in
	% which case the function Mod1:f(Arg1, Arg2, Arg3, M) is called.

	% This mechanism can be used to create "stateful modules," which is
	% discussed on page 418, and create "adapter patterns," discussed on page
	% 419
	.


% 8.26 Underscore Variables:
underscore_variables() ->
	% There's one more thing to say about variables. The special syntax
	% _Varname is used for a normal variable, not an anonymous variable.
	% Normally the compiler will generate a warning if a variable is used
	% only once in a clause since this is usually the sign of an error. If
	% the variable is used only once but starts with an underscore, the
	% warning message will not be generated.

	% Since _Var is a normal variable, very subtle bugs can be caused by
	% forgetting this and using it as a "don't care" pattern. In a
	% complicated pattern match, it can be difficult to spot that, for
	% example, _Int is repeated when it shouldn't have been, causing the
	% pattern match to fail.

	% There are two main uses of underscore variables.
		% To name a variable that we don't intend to use. That is, writing open
		% (File, _Mode) makes the program more readable that writing open
		% (File, _).

		% For debugging purposes. For example, suppose we write this:
		% some_func(X) ->
			% {P, Q} = some_other_func(X),
			% io:format("Q = ~p~n", [Q]),
			% P.

	  % This compiles without an error message.
		% Now comment out the following format statement:
		% some_func(X) ->
			% {P, Q} = some_other_func(X),
			% %%io:format("Q = ~p~n", [Q]),
			% P.

		% If we compile this, the compiler will issue a warning that the
		% variable Q is not used.
		% If we rewrite the function like this
		% some_func(X) ->
			% {P, Q} = some_other_func(X),
			% io:format("_Q = ~p~n", [_Q]),
			% P.

		% Then we can comment out the format statement, and the compiler will
		% not complain

	% no we're actually through with sequential Erlang.

	% In the next two chapters we'll round off Part 2 of the book. We'll
	% start with the type notation that is used to describe the types of
	% Erlang functions and talk about a number of tools that can be used to
	% type check Erlang code. In the final chapter of Part 2, we'll look at
	% different ways to compile and run your programs.
	.