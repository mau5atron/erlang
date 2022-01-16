% Chapter 10: Compiling and Running Your Program

% As Erlang programs become more complex, you'll want to automate the
% process in order to make life easier. That's where makefiles come in.

% There are actually 3 different ways to run your programs. In this
% chapter, we'll look at all three so you can choose the best method for
% any particular occasion.

% Sometimes things will go wrong: makefiles will fail, environment
% variables will be wrong, and your search paths will be incorrect.

% 10.1 Modifying the Development Environment
modifying_the_dev_environment() ->
	% When you start programming in Erlang, you'll probably put all your
	% modules and files in the same directory and start Erlang from this
	% directory. If you so this, then the Erlang loader will have no trouble
	% finding your code. However, as your applications become more complex,
	% you'll want to split them into manageable chunks and put the code into
	% different directories. And when you include code from other projects,
	% this external code will have its own directory structure.
	.

	% Setting the Search Paths for Loading Code:
	search_paths_loading_code() ->
		% The Erlang runtime system makes use of a code autoloading mechanism.
		% For this to work correctly, you must set a number of search paths in
		% order to find the correct version of your code.

		% The code-loading mechanism is actually programmed in Erlang, as
		% stated in Section 8.10 - Dynamic Code Loading pg 122. Code loading is
		% performed "on demand."

		% When the system tries to call a function in a module that has not
		% been loaded, an exception occurs, and the system tries to find an
		% object code file for the missing module. If the missing module is
		% called myMissingModule, then the code loader will search for a file
		% called myMissingModule.beam in all directories that are in the
		% current load path. The search stops at the first matching file, and
		% the object code in this file is loaded into the system.

		% You can find the value of the current load path by starting an Erlang
		% Shell and giving the command < code:get_path() > 

		% Example:
		1> code:get_path().
		% bunch of stuff here


		% The two most common functions that we use to manipulate the load path
		% are as follows:

		% -spec code:add_patha(Dir) => true | {error,bad_directory}
			% Add a new directory, Dir, to the start of the load path
		% -spec code:add_pathz(Dir) => true | {error,bad_directory}
			% Add a new directory, Dir, to the end of the load path

		% Usually it does not matter which you use.
		% The only thing to watch out for is if using add_patha and add_pathz
		% produces different results. If you suspect an incorrect mdule was
		% loaded, you can call < code:all_loaded() > (which returns a list of
		% all loaded modules) or  < code:clash() > to help you investigate what
		% went wrong.

		% There are several other routines in the module code for manipulating
		% the path, but you probably won't ever need to use them, unless you're
		% doing some strange system programming.

		% The usual convention is to put these commands in a file called
		% .erlang in your home directory

		% Alternatively, you can start Erlang with a command like this:
		1> erl -pa Dir1 -pa Dir2 ... -pz DirK1 -pz DirK2

		% The < -pa Dir > flag adds Dir to the beginning of the code search
		% path, and -pz Dir adds the directory to the end of the code path.
		.

	% Executing a Set of Commands When the System is Started
		% We saw how you can set the load path in your .erlang file in your
		% home directory. In fact, you can put any Erlang code in this file -
		% when you start Erlang, it first reads and evaluates all the commands
		% in this file.

		% Suppose your .erlang file is as follows:

		% io:format("Hi, im in .erlang file~n").

		% Then when we start the system, we'll see the following output:

		% $ erl

		% hi, im in .erlang file
		% Eshell V5.9
		
		% 1>

		% If there is a file called .erlang in the current directory when
		% Erlang is started, then it will take precedence over the .erlang in
		% your home directory. This way, you can arrange that Erlang will
		% behave in different ways depending upon where it is started. This can
		% be useful for specialized applications. In this case, it's probably a
		% good idea to include some print statements in the startup file;
		% otherwise, you might forget about the local startup file, which could
		% be very confusing.

		% TIP: In some systems, it's not clear where your home directory is, or
		% it might not be where you think it is. To find out where Erlang
		% thinks your home directory is, do the following:

		1> init:get_argument(home).
		{ok, [[:"/home/joe"]]}

		% From this we can infer that Erlang thinks that my home directory is
		% /home/joe

% 10.2 Different Ways to Run your Program:
different_ways_to_run_program() ->
	% Erlang programs are stored in modules. Once you have written your
	% program, you have to compile it before you can run it. Alternatively,
	% you can run your program directly without compiling it by running an
	% escript.

	% The next sections show how to compile and run a couple of programs in a
	% number of ways. The programs are slightly different, and the ways in
	% which we start and stop them differ.

	% The first program hello.erl, justs prints "hello world." It's not
	% responsible for starting or stopping the system, and it does not need
	% to access sany command-line arguments. By way of contrast, the second
	% program, <fac>, needs to access the command-line arguments.

	% Here's the basic program. It writes the string containing "Hello world"
	% followed by a newline (~n is interpreted as a newline in the Erlang
	% io and io_lib modules).

		% hello.erl
		% -module(hello).
		% -export([start/0]).

		% start() ->
			% io:format("Hello world~n").

		% Lets compile
	.

	% Compile and Run in the Erlang Shell:
	compile_in_shell() ->
		% start shell
		% $ erl
		% c(hello).
		% hello:start().
		.

	% Quick Scripting with Eval
	scripting_with_eval() ->
		% Doing the same thing with eval
		% erl -eval 'io:format("Memory: ~p~n", [erlang:memory(total)]).'
		% -noshell -s init stop
		.

	% Compile and Run from the Command Prompt
	compile_run_from_command_prompt() ->
		% Compiling a program can be done directly from the command prompt.
		% This is the easiest way to do things if you just want to compile some
		% code but not run it. This is done as follows:

		% $ erlc hello.erl
		% $ erl -noshell -s hello start -s init stop
		% Hello world

		% The first line, erlc hello.erl, compiles the file hello.erl,
		% producing an object code file called hello.beam. The second command
		% has three options:
			% -noshell
				% Starts Erlang withotu an interactive shell ( so you don't get
				% the Erlang "banner," which ordinarily greets you when you start
				% the system)

			% -s hello start
				% Runs the function hello:start()
				% NOTE: When using the -s Mod ... option, the Mod must have been
				% compiled

			% -s init stop
				% Stops the system by evaluating the function init:stop() after the
				% previous command has finished

		% The command < erl -noshell > can be put ina shell script, so
		% typically we'd make a shell script to run our program that sets the
		% path (with -pa Directory) and launches the program.

		% In our example, we used two < -s > commands. We can have as many
		% functions as we like on the command line. Each < -s > command is
		% evaluated with an apply statement, and when it has to run to
		% completion, the next command is evaluated.

		% Here's an example that launches hello.erl:

			% hello.sh
			% $ erl -noshell -pa /home/joe/2012/book/jaerlang/book/code/ -s hello
			% start -s init stop

		% NOTE: this script needs an absolute path that points to the directory
		% containing the file hello.beam
		% So although this script works on my machine, you'll have to edit it
		% to get it to run on your machine

		% To run the shell script, we $ chmod the file (only once), and then we
		% can run the script

		% chmod u+x hello.sh
		% ./hello.sh
		% hello world
		.

	% Run as an Escript
	run_as_escript() ->
		% Using an escript, you can run your programs directly as scripts -
		% there's no need to compile them first. To run hello as an escript, we
		% create the following file:

		% hello
		% main(Args) ->
			% io:format("Hello world~n").

		% The file must contain a function main(Args). When called from an
		% operating system shell, Args will contain a list of the command-line
		% arguments represented as atoms. On a Unix system, we can run this
		% immediately and without compilation as follows:

		% $ chmod u+x hello
		% $ ./hello
		% Hello world

		% NOTE: the file mode for this file must be set to "executable" (on a
		% unix system, give the command chmod u+x File) -- you have to do this
		% only once, not every time you run the program
		.

	% Exporting Functions During Development
	exporting_functions_during_development() ->
		% Exporting Functions During Development
			% When you're developing code, it can be a bit of a pain to have to
			% continaully adding and removing export declarations to your program
			% just so that you can run the exported functions in the shell.

			% The special declaration < -compile(export_all) > tells the compiler
			% to export every function in the module. Using this makes life much
			% easier when you're developing code.

			% When you're finished developing the code, you should comment out
			% the export_all declaration adn add the appropriate export
			% declarations. This is for two reasons. First, when you come to read
			% your code later, you'll know that the only important functions are
			% the exported functions.

			% All the other functions cannot be called from outside the module,
			% so you can change them in any way you like, provided the interfaces
			% to the exported functions remain the same.

			% Second, the compiler can produce much better code if it knows
			% exactly which functions are exported from the module.

			% Note that using < -compile(export_all) > will make analyzing code
			% with the dialyzer a lot more difficult.
		.

	% Programs with Command Line Arguments:
	programs_with_command_line_arguments() ->
		% "Hello world" had no arguments. Let's repeat the exercise with a
		% program that computes factorials. It takes a single argument.
		% Here's the code:

		% fac.erl
			-module(fac).
			-export([fac/1]).

			fac(0) -> 1;
			fac(N) -> N*fac(N-1).
		% We can compile fac.erl and run it in the Erlang shell like this:
			% $ erl
			% 1> c(fac).
			% {ok, fac}
			% 2> fac:fac(25).
			% (some large number) wait its actually:
			% 15511210043330985984000000

		% If we want to be able to run this program from the command line,
		% we'll need to modify it to take command-line arguments

		% Look at fac1.erl
			% -module(fac1).
			% -export([main/1]).

			% main([A]) ->
				% I = list_to_integer(atom_to_list(A)),
				% F = fac(I),
				% io:format("factorial ~w = ~w~n", [I, F]),
				% init:stop.

			% fac(0) -> 1;
			% fac(N) -> N*fac(N-1).

		% We can then compile and run it:
		% $ erlc fac1.erl
		% $ erl -noshell -s fac1 main 25
		% factorial 25 = 15511210043330985984000000

		% NOTE: The fact that the function is called main has no significance;
		% it can be called anything. The important thing is that the function
		% name and the name on the command line agree

		% Finally, we can run it as an escript
		% factorial (thats the file name, just factorial)
		% Don't forget to make the file executable: chmod u+x factorial
			% #!/usr/bin/env escript
			% main([A]) ->
			% 	I = list_to_integer(A),
			% 	F = fac(I),
			% 	io:format("factorial ~w = ~w~n", [I, F]).

			% fac(0) -> 1;
			% fac(N) -> N*fac(N-1).

		% No compilation necessary; just run it like so:
		% $ ./factorial 25
		% factorial 25 = 15511210043330985984000000
		.

% 10.3 Automating Compilation with Makefiles
automating_compilation_with_makefiles() ->
	% When writing a large program, Joe likes to automate as much as
	% possible. There are two reasons for this. First, in the long run, it
	% saves typing -- typing the same old commands over and over again as I
	% test and retest my program takes a lot of keystrokes, and I don't
	% want to wear out my fingers (Joe).

	% Second, I (Joe) often suspend what I'm (Joe) working on and go work
	% on some other project. It can be months before I return to a project
	% that I have suspended, and when I return to the project, I've
	% usually forgotten how to build the code in my project. make to the
	% rescue!

	% make is a utility for automating my work --- I use it for compiling
	% and distributing my Erlang code. Most of my makefiles are extremely
	% simple, and I have a simple template that solves most of my needs.

	% I'm not going to explain makefiles in general. Instead, I'll show the
	% form that I find useful for compiling Erlang programs. In particular,
	% we'll look at the makefiles accompanying this book so you'll be able
	% to understand them and build your own makefiles.
	.

	% A Makefile template:
	make_file_template() ->
		% Makefile.template
			% # leave these lines alone
			% .SUFFIXES: .erl .beam .yrl
			% .erl.beam:
			% 				erlc -W $<
			% .yrl.erl:
			% 				erlc -W $<
			% 
			% ERL = erl -boot start_clean

		% # Here's a list of the erlang modules you want compiling if the
		% # modules don't fit onto one line add a \ character to the end of the
		% # line and continue on to the next line

			% # Edit the line below
			% MODS = module1 module 2 \
			% 			 module3 ... special1 \
			% 			 moduleN

			% # The first target in any makefile is the default target.
			% # If you just type "make" then "make all" is assumed (because "all"
			% # is the first target in this makefile)

			% all: compile
			% compile: ${MODS: %=%.beam} subdirs
			% ## special compilation requirements are added here
			% special1.beam: special1.erl ${ERL} -Dflag1 -W0 special1.erl

			% ## run an applicaiton from the makefile

			% application1: compile
			% 					${ERL} -pa Dir1 -s application1 start Arg1 Arg2

			% # the subdirs target compiles any code in sub-directories
			% subdirs:
			% 			cd dir1; $(MAKE)
			% 			cd dir2; $(MAKE)

			% # remove all the code
			% clean:
			% 			rm -rf *.beam erl_crash.dump
			% 			cd dir1; $(MAKE) clean
			% 			cd dir2; $(MAKE) clean

		% Then makefile starts with some rules to compile Erlang modules and
		% files with the extension .yrl (these are files containing parser
		% definitions for the Erlang parser generator program). The Erlang
		% parser generator is called yecc (an Erlang version of yacc, which is
		% short for yet another compiler compiler; see the online tutorial for
		% more details)

		% The important part is the line starting like this:

			% MODS = module1 module2

		% This is a list of all the Erlang modules that I want to compile

		% Any module in the MODS alist will be compiled with the Erlang command
			% $ erlc Mod.erl

		% Some modules might need special treatment (for example the module
		% special1 in the template file), so there is a separate rule to handle
		% this.

		% Inside a makefile there are a number of targets. A target is a
		% alphanumeric string starting in the first column and terminated by a
		% colon (:). In the makefile template, < all >, < compile >, and
		% special1.beam are all targets. To run the makefile, you give the
		% shell command.

			% $ make [Target]

		% The argument Target is optional. If Target is omitted, then the first
		% target in the file is assumed. In the previous example, the target <
		% all > is assumed if no target is specified on the command line.

		% If I wanted to build all my software and run application1, then I'd
		% give the command < make application1 >. If I wanted this to be the
		% default behavior, which happens when I just give the command < make
		% >, then I'd move the lines defining the target application1 so that
		% they were the first target in the makefile.

		% The target clean removes all compiled Erlang object code files and
		% the file erl_crash.dump. The crash dump contains information that can
		% help debug an application. See (Erlang has crashed and you want to
		% read the crash dump on page 172) for details.
		.

	% Specializing the Makefile Template:
	specializing_the_makefile_template() ->
		% I'm (Joe) not a fan of clutter in my software, so what I usually do
		% is start with the template makefile and remove all lines that aren't
		% relevant to my application. This results in makefiles that are
		% shorter and easier to read. Alternatively, you could have a common
		% makefile that is included by all makefiles and that is parameterized
		% by the variables in the makefiles.

		% Once I'm through with the process, I'll end with a much simplified
		% makefile, something like the following:

			% .SUFFIXES: .erl .beam
			% .erl.beam:
			% 	erlc -W $<

			% ERL = erl -boot start_clean
			% MODS = module1 module2 module3
			% all: compile
			% 		${ERL} -pa '/home/joe/.../this/dir' -s module1 start
			% compile: ${MODS:%=%.beam}
			% 
			% clean:
			% 	rm -rf *.beam erl_crash.dump
		.

% 10.4 When Things Go Wrong:
when_things_go_wrong() ->
	% This section lists some common problems (and their solutions)
	.

	% Stopping Erlang
	stopping_erlang() ->
		% Erlang can sometimes be difficult to stop. Here are a number of
		% possible reasons:

			% * The shell is not responding

			% * The CTRL+C handler has been disabled

			% * Erlang has been started with the < -detached > flag, so you may
			% not be aware that it is running

			% * Erlang has been started with the < -heart Cmd > option. This
			% option causes an OS monitor process to be set up that watches over
			% the Erlang OS process. If the Erlang OS process dies, then Cmd is
			% evaluated. Often Cmd will simple restart the Erlang system. this is
			% one of the tricks we use when make fault tolerant nodes - if Erlang
			% itself dies (which should never happen) it just gets restarted. The
			% trick here is to find the heartbeat process (use ps on Unix like
			% systems and the task manager on windows) and kill it before you
			% kill the Erlang process

			% * Something might have gone seriously wrong and left you with a
			% detached zombie Erlang process
		. 

	% Undefined (Missing) Code
	undefined_missing_code() ->
		% If you try to run code in a module that the code loader cannot find 
		% (because the code search path was wrong), you'll be met with an undef
		% error message, Here's an example:

			% 1> glurk:oops(1,23).
			% ** exception error: undefined function glurk:oops/2

		% Actually, there is no module called glurk, but that's not the issue
		% here. The thing you should be concentrating on is the error message.
		% The error message tells us that the system tried to call the function
		% oops with two arguments in the module glurk. So, one of four things
		% could have happened.

			% * There really is no module glurk -- nowhere, not anywhere. This is
			% probably because of a spelling mistake.

			% * There is a module glurk, but it hasn't been compiled. The system
			% is looking for a file called glurk.beam somewhere in the code
			% search path.

			% * There is a module glurk and it has been compiled, but the
			% directory containing glurk.beam is not one of the directories in
			% the code search path. To fix this, you'll have to change the search
			% path.

			% * There are several different versions of glurk in the code load
			% path, and we've chosen the wrong one. This is a rare error, but it
			% can happen.
				% If you suspect this has happened, you can run the < code:clash()
				% > function, which reports all duplicated modules in the code
				% search path.
		.


	% Has Anybody Seen My Semicolons?
		% If you forget the semicolons between the clauses in a function or put
		% periods there instead, you'll be in trouble.

		% If you're defining a function foo/2 in line 1234 of the module bar
		% and put a period instead of a semicolon, the compiler will say this:

			% bar.erl:1234 function foo/2 already defined

		% Don't do it. Make sure your clauses are always separated by
		% semicolons

	% The shell isn't Responding
	shell_isnt_responding() ->
		% If the shell is not responding to commands, then a number of things
		% might have happened. The shell process itself might have crashed, or
		% you might have issued a command that will never terminate. You might
		% even have forgotten to type a closing quote mark or forgotten to type
		% dot-carriage-return at the end of your command.
		.

	% My makefile doesn't work
	makefile_does_not_work() ->
		% What can go wrong with a makefile? Well, lots, actually.
			% * Blanks in the makefile:
				% Makefiles are extremely persnickety. Although you can't see them,
				% each of the indented lines in the makefile (with then exception
				% of continuation lines, where the previous line ends with a \
				% character) should begin with a tab character. If there are any
				% spaces there, make will get confused, and you'll start seeing
				% errors.

			% * Missing erlang file:
				% If one of the modules declared in MODS is missing, you'll get an
				% error message. To illustrate this, assume that MODS contains a
				% module name glurk but that there is no file called glurk.erl in
				% the code directory. In this case. make will fail with the
				% following message:

					% $ make
					% make: *** No rule to make target `glurk.beam',
					% 					needed by `compile'. Stop.

				% Alternatively, there is no missing module, but the module name is
				% spelled incorrectly in the makefile.
		.

	% Erlang has crashed and You Want to Read the Crash Dump
	read_crash_dump() ->
		% if Erlang crashes, it leaves behind a file called erl_crash.dump. The
		% contents of this file might give you a clue as to what has gone
		% wrong. To analyze the crash dump, there is a web-based crash
		% analyzer. To start the analyzer, give the following command:

			% 1> crashdump_viewer:start().
			% WebTool is available at http://localhost:8888
			% or http://127.0.0.1:8888/
		.


% 10.5 getting Help
% skipping this

% 10.6 Tweaking the Environment
% skipping this
% look at later for add user_default functions

