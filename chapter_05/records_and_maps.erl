% Records and Maps:

% So far we have talked about two containers for date, namely, tuples and lists
% Tuples are used to store a fixed number of elements, and lists are used for
% a variable number of elements

% Records are really just tuples in disguise. Using records we can associate
% a name with each element in a tuple

% Maps are associative collections of key-value pairs
% The key can be any Erlang term. In Perl and Ruby they are called hashes;
% in C++ and Java they are called maps, in Lua they are called tables, and in
% Python they are called dictionaries

% Using records and maps makes programming easier; instead of remembering where
% a data item is stored in a complex data structure, we just use the name of the
% item and the system figures out where the data is stored. Records use a fixed
% and predefined set of names; maps can add new names dynamically



% 5.1 When to Use Maps or Records:
% - Records are just tuples in disguise, so they have the same storage and
% 	performance characteristics as tuples.
% - Maps use more storage than tuples and have slower lookup properties.
% 	On the other hand, maps are far more flexible that tuples.

% Records should be used in the following cases:
% - When you can represent your data using a fixed number of predetermined
% 	atoms
% - When the number of elements in the record and the names of the elements
% 	will not change with time
% - When storage is an issue, typically when you have a large array of tuples
% 	and each tuple has the same structure

% Maps are appropriate for the following cases:
% - Representing key-value data structures where the keys are not known in
% 	advance.
% - Representing data with large numbers of different keys.
% - As a ubiquitous data structure where efficiency is not important but
% 	convenience of use is.
% - For "self-documenting" data structures, that is, data structures where the
% 	user can mae good guess at the meaning of the value of a key from the key
% 	name
% - For representing key-value parse trees such as XML or configuration files
% - For communication with other programming languages, using JSON


% 5.2 Naming tuple items with Records

% In a small tuple, remembering what the individual elements represent is rarely
% a problem, but when there are a large number of elements in the tuple, it
% becomes convenient to name the individual elements. Once we have named the
% elements, we will be able to refer to them using the name and not have to
% remember what position they had in the tuple

% To name the elements in a tuple, we use a record declaration that has the
% following syntax:

-record(Name, {
	% the next two keys have default values
	key1 = Default1,
	key2 = Default2
	% the next line is equivalent to key = undefined
	key3
	}).

% Warning: record is not a shell command - Record declarations can be used only
% in Erlang source code modules and not in the shell.

% In the record example, Name is the name of the record, key1, key2, and so on,
% are the names of the fields in the record; they must always be atoms. Each
% field in a record can have a default value that is used if no value for this
% particular field is specified when the record is created.

% -------------------------------------------------------------------------

% Records are Tuples in Disguise
% records are just tuples

% 9> X2.
% #todo{status=done,who=joe,text="Fix errata in book"}

% Now let's tell the shell to forget the definition of todo

% 10> rf(todo).
% return: ok

% 11> X2.
% return: {todo, done, joe, "Fix errata in book"}

% in line 10 the command rf(todo) told the shell to forget the definition
% of the todo record. So, now when we print X2, the shell displays X2 as a
% tuple. Internally, there are only tuples. Records are a syntactic
% convenience, so you can refer to the different elements in a tuple by
% name and not position.

% -------------------------------------------------------------------------

% 5.3 Maps: Associative Key-Value Stores
% Maps were made available from version R17 of Erlang

% Maps have the following properties:
% - The syntax of maps is similar to that of records, the difference being
% 	that the record name is omitted and the key-value separator is either
% 	=> or :=
% - Maps are associative collections of key-value pairs
% - The keys in a map can be any fully ground erlang term (fully grounded
% 	means that there are no unbound variables in the term).
% - The elements in a map are ordered by the keys
% - Updating a map where the keys are not changed is a space-efficient
% 	operation
% - Looking up the value of a key in a map is an efficient operation
% - Maps have a well-defined order.

% -------------------------------------------------------------------------

% The Semantics of Maps:

% - Map literals are written with the following syntax:
#{key1 Op Val1, key2 Op Val2}

% This has a similar syntax to records, but there is no record name
% following the hash symbol, and the Op is one of the symbols => or :=

% The keys and values can be any valid Erlang terms, for example, suppose
% we want to create a map with two keys, a and b

% 1> F1 = #{ a := 1, b := 2}.
% returns: #{ a := 1, b := 2}.

% Or suppose we want to create a map with nonatomic keys

% 2> Facts = #{ 
% 	 	{wife, fred} 			:= "Sue",
% 		{age, fred} 			:= 45,
% 		{daughter, fred} 	:= "Mary"
% 	 }.

% Internally the map is stored as an ordered collection and will be always
% printed using the sort order of the keys, irrespective of how the map was
% created. Here's an example:

% 3> F2 = #{ b := 2, a := 1 }.
% returns: #{ a := 1, b := 2 }.

% 4> F1 = F2.
% returns: #{ a := 1, b := 2 }.

% To update a map based on an existing map, we use the following syntax
% where Op (the update operator) is => or :=:

NewMap = OldMap#{K1 Op V1, etc...}.

% *** The expression K => V is used for two purposes, either to update the
% value of the existing key with a new value V or to add a completely new
% K-V pair to the map. This operation always suceeds.

% *** The expression K := V is used to update the value of an existing key
% K with a new value V. This operation fails if the map being updated does
% not contain the key K

% 5> F3 = F1#{ c => xx }.
% returns: #{ a => xx, b => 2, c => xx }

% 6> F4 = F1#{ c := 3 }
% returns: exception error: bad argument key c does not exist in old map

% Reasons to use the := operator:
% There are two good reasons for using the := operator
% First, if we misspell the name of the new key, we want an error to occur.

% If we create a map Var = #{keypos => 1, ...} and later update it with
% Var#{key_pos := 2}, then we have almost certainly spelled the keyname
% incorrectly and we want to know about it.

% The second reason has to do with efficiency.
% If we use only the := operator in a map update operation, then we know
% that the old and new maps have an identical set of keys and thus can
% share the same key descriptor.

% If we had for example, a list with a few million maps, all with the same
% keys, then the space savings would be significant.

% The best way to use maps is to alwasys use Key => Val the first time a
% kew is defined and use Key := Val each time the value of a specific key
% is changed.

% -------------------------------------------------------------------------

% Pattern Matching the Fields of a Map

% The => syntax we used in a map literal can also be used as a map pattern.
% As before, the keys in a map pattern cannot contain any unbound
% variables, but the value can now contain variables that become bound if
% the pattern match succeeds.

% -------------------------------------------------------------------------

% Maps in Other Languages:

% Note that maps in Erlang work in aa very different manner that the
% equivalent constructs in many other programming languages. For example,
% in javascript:

% Suppose we do the following
% var x = { status: 'old', task: 'feed cats' };
% var y = x;
% y.status = 'done';

% Teh value of y is the object { status: 'done', task: 'feed cats' }. No
% surprises here. But surprise, x has change to { status: 'done', task:
% 'feed cats'}. This comes as a great surprise to an Erlang programmer. We
% managed to change the value of one of the fields of the variable x, not
% by referring to x but by assigning a value to a field of the variable y.
% Changing x through an aliased pointer leads to many kinds of subtle
% errors that can be very difficult to debug.

% The logically equivalent Erlang code is as follows:

% D1 = { status => old, task => 'feeds cats' }.
% D2 = D1#{status := done }.

% In the Erlang code, the variables D1 and D2 never change their initial
% values. D2 behaves exactly as if it were a deep copy of D1. In fact, a
% deep copy is not made; the erlang system copies only those parts of the
% internal structures necessary to maintain the illusion that a copy has
% been created, so creating what appears to be deep copies of an object is
% an extremely lightweight operation.

% 1> Henry8 = #{ class => king, born => 1491, died => 1547 }.
% returns: #{ born => 1491, class => king, died => 1547 }.

% 2> #{ born => B } = Henry8.
% returns: #{ born => 1491, class => king, died => 1547 }.

% 3> B.
% returns: 1491

% 4> #{ D => 1547 }.
% * 4: variable 'D' unbound

% In line 4 we tried to find some unknown key (D) whose value was 1547.
% But the shell prints an error since all keys in a map must be fully
% ground terms and D is undefined

% Note: The number of keys in the map pattern can be less than the number
% of keys in the map being matched

% We can use maps containing patterns in function heads, provided that all
% the keys in the map are known. For example, we can define a function
% count_characters(Str) that returns a map of the number of times
% particular character occurs in a string.

count_characters(Str) ->
	count_characters(Str, #{}).

count_characters( [Head|Tail], #{ Head => N } = X ) ->
	count_characters(Tail, X#{ Head := N+1 });

count_characters([Head|Tail], X) ->
	count_characters(Tail, X#{ Head => 1 });

count_characters([], X) ->
	X.

% Example in use:

% 1> count_characters("hello").
% returns: #{ 101 => 1, 104 => 1, 108 => 2, 111 => 1 }

% So, the character h (ASCII, 101) occurred once, and so on. There are two
% things to note about count_characters/2.
% In the first clause, the variable Head inside the map is also defined
% outside the map and thus is bound (as required). In the second clause, we
% used map_extend to add a new key to the map.

% -------------------------------------------------------------------------

% BIFs That Operate on Maps:
% A number of additional functions operate on maps. They are some of the
% functions in the module maps.

maps:new() -> #{}.
% Returns a new map

erlang:is_map(M) -> bool().
% Return true if M is a map; otherwise, return false. This can be used as a
% guard test or in a function body.

maps:to_list(M) -> [{K1,V1},...,{Kn, Vn}].
% Convert the keys and values in the map M to a list of keys and values.
% The keys in the resulting list are in strict ascending order.

maps:from_list([{K1, V1, ..., Kn, Vn}]) -> M.
% Converts a list of pairs to a map M. If the same key occurs more than
% once, then the value associated with first key in the list will be used,
% and any subsequent values will be ignored.

maps:map_size(Map) -> NumberOfEntries
% Return the number of entries in the map

maps:is_key(Key, Map) -> bool().
% Return true if the map contains an item with Key key; otherwise, raise an
% exception

maps:get(Key, Map) -> Val.
% Return teh value associated with Key from the map; otherwise, raise an
% exception

maps:find(Key, Map) -> { ok, Value } | error.
% Return the value associated with Key from the map; otherwise, return
% error.

maps:keys(Map) -> [Key1,...,KeyN].
% Return a list of keys, in ascending order, that are in the map.

maps:remove(Key, M) -> M1.
% Return a new map M1 that is the same as M except that the item with key
% Key (if present) has been removed.

maps:without([Key1,....KeyN], M) -> M1.
% Return a new map M1 that is a copy of M but with any elements having keys
% in the list [Key1, ..., KeyN] removed

maps:difference(M1, M2) -> M3.
% M3 is equivalent to M1 with any elements having the same keys as the
% elements in M2 removed.
% This behaves as if it had been defined as follows:
maps:difference(M1, M2) -> maps:without(maps:keys(M2), M1).


% -------------------------------------------------------------------------

% Ordering of Maps:

% Maps are compared by comparing first their size and then their keys and
% values in the sort order of their keys

% When comparing maps with other Erlang terms, maps are viewed as being
% "more complex" that list or tuples, and thus a map is always considered
% greater than a list or tuple.

% Maps can be output with the ~p option in io:format and read with io:read
% or file:consult

% -------------------------------------------------------------------------

% The JSON Bridge
% For those familiar with JSON will notice the similarity between maps and
% JSON terms. Two BIFs convert between maps and JSON terms.

maps:to_json(Map) -> Bin.
% Converts a map to a binary containing the JSON representation of the map.
% Binaries are discussed in CH 7, Binaries and Bit Syntax, on page 99.

% Note that not all maps can be converted to json terms. All the values in
% the map must be objects that can be represented in JSON. 

% So, for example, values cannot include objects such as funs, PIDs,
% references, and so on.

% maps:to_json fails if any of the keys or values cannot be represented in
% JSON.

maps:from_json(Bin) -> Map.
% Converts a binary containing a JSON term to a map.

maps:safe_from_json(Bin) -> Map.
% Converts a binary containing a JSON term to a map. Any atoms in Bin must
% exist before the BIF is called; otherwise an exception will be raised.

% The reason for this is to avoid creating large number of new atoms. For
% reasons of efficiency, Erlang does not garbage collect atoms, so
% continuously adding new atoms will, after a very long time, kill the
% Erlang VM.

% In both the previous definitions Map must be an instance of the type
% json_map(), which is defined as follows (type definitions will be
% introduced later in Chapter 9, Types, on page 141)

-type json_map() = [{json_key(), json_value()}].

% where

-type json_key() = 
	atom() | binary() | io_list().

% and
json_value() =
	integer() | binary() | float() | atom() | [json_value()] | json_map().

% The mapping between JSON objects and Erlang values is as follows:

% - JSON numbers are represented as Erlang integers or floats
% - JSON strings are represented as Erlang binaries
% - JSON lists are represented as Erlang lists
% - JSON true and false are represented as Erlang atoms true and false
% - JSON objects are represented as Erlang maps, with the restriction that
% 	the keys in the map must be atoms, strings, or binaries, and the values
% 	must be representable as JSON terms

% Things to look out for when converting to and from JSON terms, we should
% be aware of certain limitations of the conversion. Erlang provides
% integers with unlimited precision. So, Erlang will happily convert a
% bugnum in a map into a bignum in a JSON term; this may or may not be
% understandable by the program that decodes the JSON term.

% -------------------------------------------------------------------------

% In CH 18, Browsing with Websockets and Erlang, you'll find how to use
% maps combined with JSON terms and websockets to provide a simple method
% of communicating with an application running inside a web browser

