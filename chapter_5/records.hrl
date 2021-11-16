-record(todo, {status=reminder, who=joe, text}).

% once the record has been defined, instances of the record can be created.

% To do this in the shell, we have to read the record definitions into the shell
% before we can define a record. We use the shell function rr (short for read
% records) to do this:

% erl: rr("records.hrl").
% [todo]



% Creating and Updating Records 
% erl shell: 2> #todo{}. returns: #todo
% {status = reminder, who = joe, text = undefined}.

% 3> X1 = #todo{status = urgent, who = joe, text = "Fix errata in book"}
% returns: #todo{status = urgent, who = joe, text = "Fix errata in book"}

% 4> X2 = X1#todo{status = done}
% returns: #todo{status = done, who = joe, text = "Fix errata in book"}

% In lines 2 and 3 we created new records
% The syntax #todo{key1=Val1, keyN=ValN} is used to create a new record of
% type todo

% The keys are all atoms and must be the same as those used in the record
% definition.

% If a key is ommitted, then a default value is assumed for the value that
% comes from the value in the record definition.

% In line 4 we copied an existing record. The syntax X1 #todo{status=done}
% says to create a copy of X1 (which must be of type todo), changing the
% field value status to done. Remember, This makes a copy of the orginal
% record; the original record is not changed

% Extracing the Fields of a Record
% To extract several fields of a record in one operation we use pattern
% matching

% 5> #todo{who=W, text=Txt} = X2.
% returns: #todo{status=done, who=joe, text="Fix errata in book"}

% 6> W.
% returns: joe

% 7> Text.
% returns: "Fix errata in book"

% On the left side of the match operator (=), we write a record pattern
% with the unbound variables W and Txt. If he match succeeds, these
% variables get bound to the approprate fields in the record. If we want
% just one field of a record, we can use the "dot syntax" to extract the
% field.

% X2#todo.text.
% returns: "Fix errata in book"


% ---------------------

% Pattern Matching Records in Functions

% We can write functions that pattern match on the fields of a record and
% that create new records. We usually write code like this

clear_status(#todo{status=S, who=W} = R) ->
	% inside this function S and W are bound to the field values in the
	% record

	% R is the entire record
	R#todo{status=finished}.

% To match a record of a particular type, we might write the function
% definition

do_something(X) when is_record(X, todo) ->
	% something.
	.
% This clause matches when X is a record of type todo