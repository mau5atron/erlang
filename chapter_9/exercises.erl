-module(exercises).
% Chapter 9 Exercises:

% 1.
	% Write some very small modules that export a single function. Write 
	% type specifications for the exported functions. In the functions make
	% some type errors; then run the dialyzer on these programs and try to 
	% understand the error messages. Sometimes you'll make an error but the 
	% dialyzer will not find the error; star hard at the program to try to 
	% work out why you did not get the error you expected.

% 2.
	% Look at the type annotations in the code in the standard libraries.
	% Find the source code for the module lists.erl and read all the type
	% annotations.

% 3.
	% Why is it a good idead to think about the types of a function in a
	% module before you write the module? Is this always a good idea?

% 4.
	% Experiment with opaque types. Create two modules; the first should
	% export an opaque type. The second module should use the internal data
	% structures of the opaque type exported by the first module in such a
	% way as to cause an abstraction violation. Run the dialyzer on the tow
	% modules and make sure you understand the error messages.