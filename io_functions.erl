% Control Sequences for fwrite

% -------------------------------------------------------------------------
% ~c:
% The argument is a number that is interpreted as an ASCII code. The
% precision is the number of times the character is printed and defaults to
% the field width, which in turn defaults to 1. Example:

1> io:fwrite("|~10.5c|~-10.5c|~5c|~n", [$a, $b, $c]).
|     aaaaa|bbbbb     |ccccc|
ok

% If the unicode translation modifier (t) is in effect, the integer
% argument can be any number representing a valid Unicode codepoint,
% otherwise it is to be an integer less than or equal to 255, otherwise it
% is masked with 16#FF:

2> io:fwrite("~tc~n",[1024]).
\x{400}
ok
3> io:fwrite("~c~n",[1024]).
^@
ok

% -------------------------------------------------------------------------

% ~f:
% The argument is a float that is written as [-]ddd.ddd, where the
% precision is the number of digits after the decimal point. The default
% precision is 6 and it cannot be < 1.

% -------------------------------------------------------------------------

% ~e:
% The argument is a float that is written as [-]d.ddd+-ddd, where the
% precision is the number of digits written. The default precision is 6 and
% cannot be < 2.

% -------------------------------------------------------------------------

% ~g:
% The argument is a float that is written as f, if it is >= 0.1 and <
% 10000.0. Otherwise, it is written in the e format. The precision is the
% number of significant digits. It defaults to 6 and is not to be < 2.
% If the absolute value of the float does not allow it to be written in the
% f format with the desired number of significant digits, it is also
% written in the e format.

% -------------------------------------------------------------------------

% ~s:
% Prints the argument with the string syntax. The argument is, if no
% Unicode translation modifier is present, an iolist(), a binary(), or an
% atom(). If the Unicode translation modifier (t) is in effect, the
% argument is unicode:chardata(), meaning that binaries by the specified
% precision and then padded and justified to the specified field width. The
% default precision is the filed width.

% This format can be used for printing any object and trucating the output
% so it fits a specified field:

1> io:fwrite("|~10w|~n", [{hey, hey, hey}]).
|**********|
ok
2> io:fwrite("|~10s|~n", [io_lib:write({hey, hey, hey})]).
|{hey,hey,h|
3> io:fwrite("|~-10.8s|~n", [io_lib:write({hey, hey, hey})]).
|{hey,hey  |
ok

% A list with integers > 255 is considered an error if the Unicode
% translation modifier is not specified:

4> io:fwrite("~ts~n",[[1024]]).
\x{400}
ok
5> io:fwrite("~s~n",[[1024]]).
** exception error: bad argument
     in function  io:format/3
        called as io:format(<0.53.0>,"~s~n",[[1024]])

% -------------------------------------------------------------------------

% ~w:
% Writes data with the standard syntax. This is used to output Erlang
% terms. Atoms are printed within quotes if they contain embedded
% non-printable characters. Atom characters > 255 are escaped unless the
% Unicode translation modifier (t) is used. Floats are printed accurately
% as the shortest, correctly rounded string.

% -------------------------------------------------------------------------

% ~p:
% Writes the data with standard syntax in the same way as ~w, but breaks
% terms whose printed representation is longer than one line into many
% lines and indents each line sensibly. Left-justification is not
% supported. It also tries to detect flat lists of printable characters and
% output these as strings:

1> T = [{attributes,[[{id,age,1.50000},{mode,explicit},
{typename,"INTEGER"}], [{id,cho},{mode,explicit},{typename,'Cho'}]]},
{typename,'Person'},{tag,{'PRIVATE',3}},{mode,implicit}].
...
2> io:fwrite("~w~n", [T]).
[{attributes,[[{id,age,1.5},{mode,explicit},{typename,
[73,78,84,69,71,69,82]}],[{id,cho},{mode,explicit},{typena
me,'Cho'}]]},{typename,'Person'},{tag,{'PRIVATE',3}},{mode
,implicit}]
ok
3> io:fwrite("~62p~n", [T]).
[{attributes,[[{id,age,1.5},
               {mode,explicit},
               {typename,"INTEGER"}],
              [{id,cho},{mode,explicit},{typename,'Cho'}]]},
 {typename,'Person'},
 {tag,{'PRIVATE',3}},
 {mode,implicit}]
ok


% The field width specifies the maximum line length. It defaults to 80. The
% precision specifies the initial indentation of the term. It defaults to
% the number of characters printed on this line in the same call to write/1
% or format/1,2,3. For example, using T above:

4> io:fwrite("Here T = ~62p~n", [T]).
Here T = [{attributes,[[{id,age,1.5},
                        {mode,explicit},
                        {typename,"INTEGER"}],
                       [{id,cho},
                        {mode,explicit},
                        {typename,'Cho'}]]},
          {typename,'Person'},
          {tag,{'PRIVATE',3}},
          {mode,implicit}]
ok

% As from Erlang/OTP 21.0, a field width of value 0 can be used for
% specifying that a line is infinitely long, which means that no line
% breaks are inserted. For example:

5> io:fwrite("~0p~n", [lists:seq(1, 30)]).
[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30]
ok

% When the modifier l is specified, no detection of printable character
% lists takes place, for example:

6> S = [{a,"a"}, {b, "b"}],
   io:fwrite("~15p~n", [S]).
[{a,"a"},
 {b,"b"}]
ok
7> io:fwrite("~15lp~n", [S]).
[{a,[97]},
 {b,[98]}]
ok

% The unicode translation modifier t specifies how to treat characters
% outside the Latin-1 range of codepoints, in atoms, strings, and binaries.
% For example, printing an atom containing a character > 255:

8> io:fwrite("~p~n",[list_to_atom([1024])]).
'\x{400}'
ok
9> io:fwrite("~tp~n",[list_to_atom([1024])]).
'Ѐ'
ok

% By default, Erlang only detects lists of characters in the Latin-1 range
% as strings, but the +pc unicode flag can be used to change this (see
% printable_range/0 for details) For example:

10> io:fwrite("~p~n",[[214]]).
"Ö"
ok
11> io:fwrite("~p~n",[[1024]]).
[1024]
ok
12> io:fwrite("~tp~n",[[1024]]).
[1024]
ok

% But if Erlang was started with +pc unicode:

13> io:fwrite("~p~n",[[1024]]).
[1024]
ok
14> io:fwrite("~tp~n",[[1024]]).
"Ѐ"
ok

% Similarly, binaries that look like UTF-8 encoded strings are output with
% the binary string syntax if the t modifier is specified:

15> io:fwrite("~p~n", [<<208,128>>]).
<<208,128>>
ok
16> io:fwrite("~tp~n", [<<208,128>>]).
<<"Ѐ"/utf8>>
ok
17> io:fwrite("~tp~n", [<<128,128>>]).
<<128,128>>
ok

% -------------------------------------------------------------------------

% ~W:
% Writes data in the same way as ~w, but takes an extra argument that is
% the maximum depth to which terms are printed. Anything below to this
% depth is replaced with ... For example, using T above:

8> io:fwrite("~W~n", [T,9]).
[{attributes,[[{id,age,1.5},{mode,explicit},{typename,...}],
[{id,cho},{mode,...},{...}]]},{typename,'Person'},
{tag,{'PRIVATE',3}},{mode,implicit}]
ok

% If the maximum depth is reached, it cannot be read in the resultant
% output. Also, the ",..." form in a tuple denotes that there are more
% elements in the tuple but these are below the print depth

% -------------------------------------------------------------------------

% ~P:
% Writes data in the same way as ~p, but takes an extra argument that is
% the maximum depth to which terms are printed. Anything below this depth
% is replaced with ..., for example:

9> io:fwrite("~62P~n", [T,9]).
[{attributes,[[{id,age,1.5},{mode,explicit},{typename,...}],
              [{id,cho},{mode,...},{...}]]},
 {typename,'Person'},
 {tag,{'PRIVATE',3}},
 {mode,implicit}]
ok

% -------------------------------------------------------------------------

% ~B:
% Writes an integer in base 2-36, the default base is 10. A leading dash is
% printed for negative integers.

% The precision field selects base, for example:

1> io:fwrite("~.16B~n", [31]).
1F
ok
2> io:fwrite("~.2B~n", [-19]).
-10011
ok
3> io:fwrite("~.36B~n", [5*36+35]).
5Z
ok

% -------------------------------------------------------------------------

% ~X:
% Like B, but takes an extra argument that is a prefix to insert before the
% number, but after the leading dash, if any.

% The prefix can be a possibly deep list of characters or an atom. Example:

1> io:fwrite("~X~n", [31,"10#"]).
10#31
ok
2> io:fwrite("~.16X~n", [-31,"0x"]).
-0x1F
ok

% -------------------------------------------------------------------------

% ~#:
% Like B, but prints the number with an Erlang style #-separated base
% prefix. Example:

1> io:fwrite("~.10#~n", [31]).
10#31
ok
2> io:fwrite("~.16#~n", [-31]).
-16#1F
ok

% -------------------------------------------------------------------------

% ~b:
% Like ~B, but prints lowercase letters

% -------------------------------------------------------------------------

% ~x:
% Like ~X, but prints lowercase letters

% -------------------------------------------------------------------------

% ~+:
% Like #, but prints lowercase letters

% -------------------------------------------------------------------------

% ~n:
% Writes a new line

% -------------------------------------------------------------------------

% ~i:
% ignores the next term.
% The function returns "ok"

% If an error occurs, there is no output. Example:

1> io:fwrite("~s ~w ~i ~w ~c ~n",['abc def', 'abc def', {foo, 1},{foo, 1}, 65]).
abc def 'abc def'  {foo,1} A
ok
2> io:fwrite("~s", [65]).
** exception error: bad argument
     in function  io:format/3
        called as io:format(<0.53.0>,"~s","A")

% In this example, an attempt was made to output the single character 65
% with the aid of the string formatting directive "~s"
