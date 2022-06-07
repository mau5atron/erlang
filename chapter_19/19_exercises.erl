% Chapter 19 Exercises
	
	% 1.
	% Mod:module_info(exports) returns a list of all the exported functions
	% in the module Mod. Use this function to find all the exported functions
	% from the Erlang system libraries. Make a key-value lookup table where
	% the key is a {Function, Arity} pair and the value is a module name.
	% Store this data in ETS and DETS tables.

	% HINT: Use code:lib_dir() and code:lib_dir(LibName) to find the names of
	% all the modules in the system.

	% 2.
	% Make a shared ETS counter table. Implement a function calle count:me
	% (Mod, Line) that you can add to your code. You'll call the function by
	% adding count:me(?MODULE, ?LINE) lines to your code. Every time this
	% function is called, it should increment a counter that counts how many
	% times this line has been executed. Implement routines to initialize and
	% read the counters.

	% 3.
	% Write a program to detect plagiarisms to text. To do this, use a
	% two-pass algorithm. In pass 1, break the text into 40-character blocks
	% and compute a checksum for each 40-character block. Store the checksum
	% and filename in an ETS table. In pass 2, compute the checksums of each
	% 40-character block in the data and compare it with the checksums in the
	% ETS table.

	% Hint: You will need to compute a "rolling checksum" to do this. For
	% example, if C1 = B1 + B2 + .... B40 and C2 = B2 + B3 + .... B41, then
	% C2 can be quickly computed by observing that C2 = C1 + B41 - B1.

