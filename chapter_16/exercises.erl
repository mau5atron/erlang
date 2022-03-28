-module(exercises).


% 1.

	% When an Erlang file X.erl is compiled, a file X.beam is created 
	% (if the compilation succeeded). Write a program that checks 
	% whether an Erlang module needs recompiling. Do this by 
	% comparing the last modified time stamps of the Erlang and beam 
	% files conerned.

% 2.

	% Write a program to compute the MD5 checksum of a small file, 
	% and use the BIF erlang:md5/1 to compute the MD5 checksum of the 
	% data (see the Erlang manual page for details of this BIF).

% 3.

	% Repeat the previous exercise for a large file (say a few 
	% hundred megabytes). This time read the file in small chunks, using
	% erlang:md5_init, erlang:md5_update, and erlang:md5_final to compute the
	% MD5 sum of the file.

% 4.
	
	% Use the lib_find module to find all .jpg files on your computer.
	% Compute the MD5 checksum of each file by comparing MD5 checksums to see
	% whether any two images are identical.

% 5.

	% Write a cache mechanism that computes the MD5 checksum of a file and
	% remembers it in a cache, together with the last modified time of the
	% file. Which we want the MD5 sum of a file, check the cache to see
	% whether the value has been computed, and recompute it if the last
	% modified time of the file has been changed.

% 6.

	% Twits are exactly 140 bytes long. Write a random-access twit storage
	% module called twit_store.erl that exports the following: init(K)
	% allocates space for K twits, store(N, Buf) stores twit numbers N (1..K)
	% with data Buf (a 140-byte binary) in the store. fetch (N) fetches the
	% data for twit number N.