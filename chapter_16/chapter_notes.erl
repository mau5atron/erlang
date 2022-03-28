-module(chapter_notes).

% Chapter 16: Programming with Files

% In this chapter, we'll look at some of the most commonly 
% used functions for manipulating files. The standard Erlang 
% release has a large number of functions for workign with 
% files. We're going to concentrate on the small fraction of 
% these that I use to write most of my programs (Joe) and 
% that you'll use the most frequently as well. We'll also 
% seee a few examples of techniques for writing efficient 
% file handling code. In addition, I'll briefly mention some 
% of the more rarely used file operations so you'll know they 
% exist. If you want more details of the rarely used 
% techniques, consult the manual pages.

% We'll concentrate on the following areas:
	% * Overview of the main modules used for manipulating 
	% files.
	
	% * Different ways of reading a file.

	% * Different ways of writing to a file.

	% * Directory operations

	% * Finding information about a file.

% 16.1 Modules for Manipulating Files:
modules_for_manipulating_files() ->
	% The functions for file manipulation are organized into four modules.


		% file
			% This has routines for opening, closing, reading, and writing files;
			% listing directories; and so on. A short summary of some of the more
			% frequently used functions in file is show on page 245, (Summary of
			% File operations - module file)

		% filename
			% This module has routines that manipulate filenames in a
			% platform-independent manner, so you can run the same code on a
			% number of different operating systems.

		% filelib
			% This module is an extension to file, which contains a number of
			% utilities for listing files, checking file types, and so on. Most
			% of these are written using the functions in file.

		% io
			% This module has routines that work on opened files. It contains
			% routines for parsing data in a file and writing formatted data to a
			% file.
	.

% 16.2 Ways to Read a File:
ways_to_read_a_file() ->
	% Let's look at some options when it comes to reading files. We'll start
	% by writing five little programs that open a fileand input the data in a
	% number of ways.

	% The contents of a file are just a sequence of bytes. Whether they mean
	% anything depends upon the interpretation of these bytes.

	% To demonstrate this, we'll use the same input file for all our
	% examples. It actually contains a sequence of Erlang terms. Depending
	% upon how we open and read the file, we can interpret the contents as a
	% sequence of Erlang terms, as a sequence of text lines, or as raw chunks
	% of binary data with no particular interpretation.

	% Here's the raw data in a file:

		% data1.dat
			% {person, "joe", "armstrong",
			% 				 [{occupation, programmer},
			% 					{favoriteLanguage, erlang}]}.

			% {cat, {name, "zorro"},
			% 			{owner, "joe"}}.

	% Now we'll read parts of this file in a number of ways.
	.

	% Reading all the Terms in the File:
	reading_all_terms_in_file() ->
		% data1.dat contains a sequence of Erlang terms; we can read all of
		% these by calling file:consult as follows:

			% 1> file:consult("data1.dat").
			% {ok,[{person,"joe",
	    %     	      "armstrong",
   		%     [{occupation,programmer},{favoriteLanguage,erlang}]},
   		%     {cat,{name,"zorro"},{owner,"joe"}}]}

   	% file:consult(File) assumes that File contains a sequence of Erlang
   	% terms. It returns {ok, [Term]} if it can read all the terms in the
   	% file; otherwise, it returns {error, Reason}.

   	% Summary of File Operations

   		% Function & Description

   			% change_group : Change group of a file

   			% change_owner : Change owner of a file

   			% change_time : change the modification or last access time of a
   			% file

   			% close : close a file

   			% consult : Read Erlang terms from a file

   			% copy : Copy file contents

   			% del_dir : Delete a directory

   			% delete : Delete a file

   			% eval : Evaluate Erlang expressions in a file

   			% format_error : Return a descriptive string for an error reason

   			% get_cwd : Get the current working directory

   			% list_dir : List files in a directory

   			% make_dir : Make a directory

   			% make_link : Make a hard link to a file or directory

   			% make_symlink : Make a symbolic link to a file or directory

   			% open : Open a file

   			% position : Set the position in a file

   			% pread : Read from a file at a certain position

   			% pwrite : Write to a file at a certain position

   			% read : Read from a file

   			% read_file : Read an entire file

   			% read_file_info : Get information about a file

   			% read_link : See what a link is pointing to

   			% read_link_info : Get information about a link or file

   			% rename : Rename a file

   			% script : Evaluate and return the value of Erlang expressions in
   			% a file.

   			% set_cwd : Set the current working directory

   			% sync : Synchronize the in-memory state of a file with that on
   			% the physical medium

   			% truncate : Truncate a file

   			% write : Write to a file

   			% write_file : Write an entire file

   			% write_file_info : Change information about a file
		.

	% Reading the Terms in the File One at a Time
	reading_terms_in_file_one_at_a_time() ->
		% If we want to read the terms in a file one at a time, we first open
		% the file with file:open, then we read the individual terms with
		% io:read until we reach the end of file, and finally we close the file
		% with file:close.

		% Here's a shell session that shows what happens when we read the terms
		% in a file at a time:


			% 1> {ok, S} = file:open("data1.dat", read).
			% {ok, <0.36.0>}

			% 2> io:read(S, '').
			% {ok, {"joe",
			% "armstrong",
			% [{occupation, programmer},
			% {favoriteLanguage, erlang}]}}

			% 3> io:read(S, '').
			% {ok, {cat, {name,"zorro"}, {owner,"joe"}}}

			% 4> io:read(S, '').
			% eof

			% file:close(S).
			% ok

		% The functions we've used here are as follows:

			% -spec file:open(File, read) -> {ok, IoDevice} | {error, Why}

				% Tries to open File for reading. It returns {ok, IoDevice} if it
				% can open the file; otherwise, it returns {error, Reason}.
				% IoDevice is an I/O device that is used to access the file.


			% -spec io:read(IoDevice, Prompt) -> {ok, Terms} | {errory, Why} |
			% eof
				% Reads an Erlang Term from IoDevice. Prompt is ignored if IoDevice
				% represents an opened file. Prompt is used only to provide a
				% prompt if we use io:read to read from standard input.

			% -spec file:close(IoDevice) -> ok | {error, Why}
				% Closes IoDevice


		% Using these routines we could have implemented file:consult, which we
		% used in the previous section. Here's how file:consult might have
		% been defined:

			% Look at lib_misc.erl

		% This is not how file:consult is actually defined. The standard
		% libraries use an improved version with better error reporting.

		% Now is a good time to look at the version included in the standard
		% libraries. If you've understood the earlier version, then you should
		% find it easy to follow the code in the libraries. There's only one
		% problem: we need to find the source of the file.erl code. To find
		% this, we use the function code:which, which can locate the object
		% code for any module that has been loaded.

			% 1> code:which(file).
			% "/usr/local/lib/erlang/lib/kernel-2.16.1/ebin/file.beam"

		% In the standard release, each library has two subdirectories. One,
		% called src, contains the source code. The other, called ebin,
		% contains compiled Erlang code. So, the source code the file.erl
		% should be in the following directory:

			% "/usr/local/lib/erlang/lib/kernel-2.16.1/src/file.erl"

		% When all else fails and the manual pages don't provide the answers to
		% all your questions about the code, then a quick peek at the source
		% code can often reveal the answer. Now I know this shouldn't happen,
		% but we're all human, and sometimes the documentation doesn't anwer
		% all your questions.
		.

	% Reading the Lines in a File One at a Time
	reading_lines_in_file_one_at_a_time() ->
		% 1> {ok, S} = file:open("data1.dat", read).
		% {ok,<0.43.0>}

		% 2> io:get_line(S, '').
		% "{person, \"joe\", \"armstrong\",\n"

		% 3> io:get_line(S, '').
		% "\t[{occupation, programmer},\n"

		% 4> io:get_line(S, '').
		% "\t {favoriteLanguage, erlang}]}.\n"

		% 5> io:get_line(S, '').
		% "\n"

		% 6> io:get_line(S, '').
		% "{cat, {name, \"zorro\"},\n"

		% 7> io:get_line(S, '').
		% "      {owner, \"joe\"}}.\n"

		% 8> io:get_line(S, '').
		% eof

		% 9> file:close(S)
		% ok
		.

	% Reading the Entire File into a Binary
	reading_the_entire_file_into_a_binary() ->
		% You can use file:read_file(File) to read an entire file into a binary
		% using a single atomic operation

			% 1> file:read_file("data1.dat")
			% {ok, <<"{person, \"joe\", \"armstrong\"}">>}

		% file:read_file(File) returns {ok, Bin} if it succeeds and returns 
		% {error, Why} otherwise. This is by far the most efficient way of
		% reading files, and it's a method that I use a lot. For most
		% operations, I read the entire file into memory in one operation,
		% manipulate the contents, and store the file in a single operation 
		% (using file:write_file). We'll give an example of this later.
		.

	% Reading a File with Random Access
	reading_a_file_with_random_access() ->
		% If the file we want to read is very large or if it contains binary
		% data in some externally defined format, then we can open the file in
		% raw mode and read any portion of it using file:pread

		% Here's an example:

			% 1> {ok, S} = file:open("data1.dat", [read, binary, raw]).
			% {ok,{file_descriptor,prim_file,{#Port<0.106>,5}}}

			% 2> file:pread(S, 22, 46).
			% {ok,<<"rong\",\n\t[{occupation, programmer},\n\t {favorite">>}

			% 3> file:pread(S, 1, 10).
			% {ok,<<"person, \"j">>}

			% 4> file:pread(S, 2, 10).
			% {ok,<<"erson, \"jo">>}

			% 5> file:close(S).
			% ok

		% file:pread(IoDevice, Start, Len) reads exactly Len Bytes from
		% IoDevice starting at byte Start (the bytes in the file are numbered
		% so that the first byte in the file is at position 0). It returns 
		% {ok, Bin} or {error, Why}

		% Finally, we'll use the routines for random file access to write a
		% utility routine that we'll need in the next chapter. In section 17.6 
		% a SHOUTcast Server, on page 281, we'll develop a simple SHOUTcast
		% server (this is a server for so-called streaming media, in this case
		% for streaming MP3). Part of this server needs to be able to find the
		% artist and track names that are embedded in an MP3 file. We will do
		% this in the next section.
		.

	% Reading MP3 Metadata
	reading_mp3_metadata() ->
		% MP3 is a binary format used for storing compressed audio data. MP3
		% files do not themselves contain information about the content of the
		% file, so, for example, an an MP3 file that contains music, the name
		% of the artist who recorded the music is not contained in the audio
		% data. This data (the track name, artist name, and so on) is stored
		% inside the MP3 files in a tagged block format known as ID3. ID3 tags
		% were invented by a programmer called Eric Kemp to store metadata
		% described the content of an audio file. There are actually a number
		% of ID3 formats, but for our purposes, we'll write code to access only
		% the two simplest forms of ID3 tags, namely, the ID3v1 and ID3v1.1
		% tags.

		% The ID3v1 tag has a simple structure - the last 128 bytes of the file
		% contain a fixed-length tag. The first 3 bytes contain the ASCII
		% characters TAG, followed by a number of fixed-length fields. The
		% entire 128 bytes is packed as follows:

		% Length : Contents

			% 3    : Header containing the characters TAG
			% 30	 : Title
			% 30 	 : Artist
			% 30 	 : Album
			% 4    : Year
			% 30	 : Comment
			% 1 	 : Genre

		% In the ID3v1 tag there was no palce to add a track number. A method
		% for doing this was suggested by Michael Mutschler, in the ID3v1.1
		% format. The idea was to change the 30-byte comment field to the
		% following

		% Length : Contents
			% 28   : Comment
			% 1 	 : 0 (a zero)
			% 1 	 : Track number

		% It's easy to write a program that tries to read the ID3v1 tags in an
		% MP3 file and matches the fields using the binary bit-matching syntax.
		% Here's the program:

			% Look at id3_v1.erl

		% The main entry point to our program is id3_v1:dir(Dir). The first
		% thing we do is find all our MP3 files by calling lib_find:find(Dir,
		% "*.mp3", true) (shown later in Section 16.6, A find utility, pg 258),
		% which recursively scans the directories under Dir looking for MP3
		% files.

		% Having found the file, we parse the tags by calling read_id3_tag.
		% Parsing is greatly simplified because we can merely use the
		% bit-matching syntax to do the parsing for us, and then we can trim
		% the artist and track names by removing trailing whitespace and
		% zero-padding characters, which delimit the character strings. Finally
		%, we dump the results in a file for later use (lib_misc:dump is
		% described in Dumping to a file, page 349).

		% Most music files are tagged with ID3v1 tags, even if they also have
		% ID3v2, v3, and v4 tags - the later tagging standards added a
		% differently formatted tags to the beginning of the file (or more
		% rarely in the middle of the file). Tagging programs often appear to
		% add both ID3v1 tags and additional (and more difficult to read) tags
		% at the start of the file. For our purposes, we'll be concerned only
		% with files containing valid ID3v1 and ID3v1.1 tags.

		% Now that we know how to read a file, we can move to the different
		% ways of writing toa file.
		.

% 16.3 Ways to Write a File:

ways_to_write_a_file() ->
	% Writing a file involves pretty much the same operations as reading a
	% file. Let's look at them.
	.

	% Writing a List of Terms to a File:
	writing_a_list_of_terms_to_a_file() ->
		% Suppose we want to create a file that we can read with file:consult.
		% The standard libraries don't actually contain a function for this, so
		% we'll write our own. Let's call this function unconsult.

			% look at lib_misc.erl

		% 1> lib_misc:consult("test1.dat", [{cats, ["zorrow", "daisy"]},
		% 																		{weather, snowing}]).

		% unconsult opens the file in write mode and calls io:format(S, 
		% "~p.~n", [X]) to write terms to the file.

		% io:format is the workhorse for creating formatted output. To produce
		% formatted output, we call the following:


			% -spec io:format(IoDevice, Format, Args) -> ok
				% ioDevice is an I/O device (which must have been opened in write
				% mode), Format is a string containing formatting codes, and Args
				% is a list of items to be output.

		% For each item in Args, there must be a formatting command in the
		% format string. Formatting commands begin with a tilde (~) character.
		% Here are some of the most commonly used formatting commands:

			% ~n
				% Write a line feed. ~n is smart and writes a line feed in a
				% platform-dependent way. So, on a Unix machine, ~n will write
				% ASCII (10) to the output stream, and on a Windows machine it will
				% write carriage-return line-feed ASCII (13, 10) to the output
				% stream.

			% ~p
				% Pretty-print the argument

			% ~s
				% The argument is a string or I/O list, or an atom and will be
				% printed without any surrounding quotation marks.

			% ~w
				% Write data with the standard syntax. This is used to output
				% Erlang terms.

				% The format string has about ten quadzillion arguments that nobody
				% in their right mind could remember. You'll find a complete list
				% in the man page for the module io.

			% 
		.

	% Writing Lines to a File


	% Writing an Entire File in One Operation
	writing_an_entire_file_in_one_operation() ->
		% This is the most efficient way of writing to a file. file:write_file
		% (File, IO) writes the data in IO, which is an I/O list, to File. (An
		% I/O list is a list whose elements are I/O lists, binaries, or
		% integers from 0 to 255. When an I/O list is output, it is
		% automatically "flattened," which means that all the list brackets
		% are removed.) This method is extremely efficient and is one that I
		% often use. The program in the next section illustrates this.
		.

