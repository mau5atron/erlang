% 19.4 Example Programs with ETS

	% The examples in this section have to do with trigram generation. This
	% is a nice "show-off" program that demonstrates the power of the ETS
	% tables.

	% Our goal is to write a heuristic program that tries to predict whether
	% a given string is an english word.

	% To predict whether a random sequence of letters is an English word,
	% we'll analyze which trigrams occur in the word. A trigram is a sequence
	% of three letters. Now, not all sequences of three letters can occur in
	% a valid English word. For example, there are no English words where
	% the three-letter combinations "akj" and "rwb" occur. So, to test
	% whether a string might be an english word, all we have to do is test
	% all sequences of three consecutive letters in the string against the
	% set of trigrams generated from a large set of English words.

	% The first thing our program does is to compute all trigrams in the
	% English language from a very large set of words. To do this, we use ETS
	% sets. The decision to use an ETS set is based on a set of measurements
	% of the relative performances of ETS sets and ordered sets and of using
	% "pure" Erlang sets as provided by the "sets" module.

	% This is what we're going to do in the next few sections:

		% 1. Make an "iterator" that runs through all the trigrams in the
		% English language. This will greatly simplify writing code to insert
		% the trigrams into different table types.

		% 2. Create ETS tables of type "set" and "ordered_set" to represent all
		% these trigrams. Also, build a set containing all these trigrams.

		% 3. Measure the time to "build" these different tables.

		% 4. Measure the time to access these different tables.

		% 5. Based on the measurements, choose the best method and write access
		% routines for the best method.

	% All the code is in lib_trigrams. We're going to present this in
	% sections, leaving out some of the details. But don't worry, the
	% complete code is in the file code/lib_trigrams.erl available from the
	% book's home page.

% -------------------------------------------------------------------------

% The Trigram Iterator

	% We'll define a function called
	% "for_each_trigram_in_the_english_language(F, A)". This function applies
	% the fun F to every trigram in the English language. F is a fun of type
	% fun(Str, A) -> A, Str ranges over all trigrams in the language, and A
	% is an accumulator.

	% To write our iterator, we need a massive word list. (Note: I've called
	% this an iterator here; to be more strict, it's actually a fold
	% operator verey much like lists:foldl.) I've used a collection of
	% 354,984 English words to generate the trigrams. Using this word list,
	% we can define the trigram iterator as follows:

	  for_each_trigram_in_the_english_language(F, A0) ->
	  	{ok, Bin0} = file:read_file("354984si.ngl.gz"),
	  	Bin = zlib:gunzip(Bin0),
	  	scan_word_list(binary_to_list(Bin), F, A0).

	  scan_word_list([], _, A) ->
	  	A;

	  scan_word_list(L, F, A) ->
	  	{Word, L1} = get_next_word(L, []),
	  	A1 = scan_trigrams([$\s|Word], F, A),
	  	scan_word_list(L1, F, A1).

	  % scan the word looking for \r\n
	  % the second argument is the word (reversed) so it
	  % has to be reversed when we find \r\n or run out of characters

	  get_next_word([$\r, $\n|T], L) -> {reverse([$\s|L]), T};
	  get_next_word([H|T], L) 			 -> get_next_word(T, [H|L]);
	  get_next_word([], L) 					 -> {reverse([$\s|L]), []}.

	 	scan_trigrams([X, Y, Z], F, A) ->
	 		F([X, Y, Z], A);

	 	scan_trigrams([X, Y, Z|T], F, A) ->
	 		A1 = F([X, Y, Z], A),
	 		scan_trigrams([Y, Z|T], F, A1);

	 	scan_trigrams(_, _, A) ->
	 		A.

	% Notice the two points here. First, we used zlib:gunzip(Bin) to unzip
	% the binary in the source file. The word list is rather long, so we
	% prefer to save it on disk as a compressed file rather than as a raw
	% ASCII file. Second, we add a space before and after each word; in our
	% trigram analysis, we want to treat space as if it were a regular
	% letter.

% -------------------------------------------------------------------------

% Build the Tables

	% We built our ETS tables like this:

	% lib_trigrams.erl

		make_ets_ordered_set() -> make_a_set(ordered_set, "trigramsOS.tab").
		make_ets_set() 				 -> make_a_set(set, "trigramsS.tab").

		make_a_set(Type, FileName) ->
			Tab = ets:new(table, [Type]),
			F = fun(Str, _) -> ets:insert(Tab, {list_to_binary(Str)}) end,
			for_each_trigram_in_the_english_language(F, 0),
			ets:table2file(Tab, FileName),
			Size = ets:info(Tab, size),
			ets:delete(Tab),
			Size.

	% Note how when we have isolated a trigram of three letters, ABC, we
	% actually insert the tuple {<<"ABC">>} into the ETS table representing
	% the trigrams. This looks funny --- a tuple with only one element.
	% Normally a tuple is a container for several elements, so it doesn't
	% make sense to have a tuple with only one element. But remember all the
	% entries in an ETS table are tuples, and by default the key in a tuple
	% is the first element in the tuple. So, in our case, the tuple {Key}
	% represents a key with no value.

	% Now for the code that builds a set of all trigrams (this time with the
	% Erlang module sets and not ETS):

		% lib_trigrams.erl

		make_mod_set() ->
			D = sets:new(),
			F = fun(Str, Set) -> sets:add_element(list_to_binary(Str), Set) end,
			D1 = for_each_trigram_in_the_english_language(F, D),
			file:write_file("trigrams.set", [term_to_binary(D1)]).

  % not finishing the rest of 19.4 as its going to waste time.







