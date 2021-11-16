
% We've now covered all the ways there are of creating compound data
% structures in Erlang. We know about lists as containers for a variable
% number of items and tuples as containers for a fixed number of items.
% Records are used to add symbolic names to the elements of a tuple, and
% maps are used as associative arrays.

% Exercises:

% 1. Configuration files can be conveniently represented as JSON terms.
% 	 Write some functions to read configuration files containing JSON terms
% 	 and turn them into Erlang maps. Write come code to perform sanity
% 	 checks on the data in the configuration files.

% 2. Write a function map_search_pred(Map, Pred) that returns the first
% 	 element {Key, Value} in the map for which Pred(Key, Value) is true

% 3. Advanced: Look up the manual pages for the Ruby hash class. Make a
% 	 module of the methods in the Ruby class that you think would be
% 	 appropriate to Erlang.

