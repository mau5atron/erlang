% Chapter 7: Binaries and Bit Syntax

% A "binary" is a data structure designed for storing large quantities of
% raw data in a space-efficient manner. The Erlang VM is optimized for the
% efficient input, output, and message passing of binaries.

% Binaries should be used whenever possible for storing the contents of
% large quantities of unstructured data, for example large strings or the
% contents of files.

% In most circumstances, the number of bits in a binary will be exactly
% divisible by 8 and thus corresponds to a sequence of bytes. If the number
% of bits is not exactly divisible by 8, we use the same bitstring to refer
% to the data. When we say bitstring, it is to emphasize the fact that the
% number of bits in the data is not an exact multiple of 8

% Binaries, bitstrings, and bit-level pattern matching were introduced in
% Erlang to simplify network programming where we often want to probe into
% the bit and byte-level structure of protocol packets.

% In this chapter, we'll first take a detailed look at binaries. Most of
% the operations on binaries work in the same way on bitstrings, so after
% underestanding binaries, we'll look at bitstrings emphasizing where they
% differ from binaries.

% -------------------------------------------------------------------------

% Binaries:

% Binaries are written and printed as sequences of integers or strings,
% enclosed in double less-than and greater-than brackets. Here's an example

1> <<5, 10, 20>>.
<<5, 10, 20>>

2> <<"hello">>.
<<"hello">>

3> <<65, 66, 67>>.
<<"ABC">>

% When you use integers in a binary, each must be in the range 0 to 255.
% The binary <<"cat">> is shorthand for <<99,97,116>>; that is, the binary
% is made from the ASCII character codes of the characters in the string

% As with strings, if the content of a binary is a printable string, then
% the shell will print the binary as a string; otherwise, it will be
% printed as a sequence of integers.

% We can build a binary and extract the elements of a binary using a BIF,
% or we can use the bit syntax. In this section, we'll look only at the
% BIFs that manipulate binaries.

% -------------------------------------------------------------------------

% Working with Binaries:

% We can manipulate binaries using BIFs or with functions from the binary
% module. Many of the functions exported from binary are implemented as
% native code. Here are some of the most important:

list_to_binary(L) -> B.
% list_to_binary returns a binary constructed by flattening (flatenning
% means removing all the list parentheses) all the elements in the iolist
% L. An iolist is defined recursively as a list whose elements are integers
% in 0..255, binaries, or iolists.

1> Bin1 = <<1,2,3>>.
<<1,2,3>>

2> Bin2 = <<4,5>>.
<<4,5>>

3> Bin3 = <<6>>.
<<6>>

4> list_to_binary([Bin1, 1, [2,3,Bin2], 4|Bin3]).
<<1, 2, 3, 1, 2, 3, 4, 5, 4, 6>>

% Note: the space surrounding the equals sign in line 1 is necessary.
% Without this space, the second symbol seen by the Erlang tokenizer would
% be the atom '=<', which is the equal-to-or-less-than operator. Sometimes
% we have to put spaces or parentheses around binary literals to avoid
% syntax errors.

% -------------------------------------------------------------------------

split_binary(Bin, Pos) -> {Bin1, Bin2}.
% This splits the binary Bin into two parts at position Pos

1> split_binary(<<1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>, 3).
{<<1, 2, 3>>,<<4, 5, 6, 7, 8, 9, 10>>}

% -------------------------------------------------------------------------

term_to_binary(Term) -> Bin.

% The binary produced by term_to_binary is represented in the so-called
% external term format. Terms that have been converted to binaries using
% term_to_binary can be stored in files, sent in messages over a network,
% and so on, and the original term from which they were made can be
% reconstructed later. This is extremely useful for storing complex data
% structures in files or sending complex data structures to remote
% machines.

% -------------------------------------------------------------------------

binary_to_term(Bin) -> Term.
% This is the inverse of term_to_binary.

1> B = term_to_binary({binaries, "are", useful}).
<<131, 104, 3, 100, 0, 8, 98, 105, 110, 97, 114, 105, 101, 115, 107, 0, 3,
97, 114, 101, 100, 0, 6, 117, 115, 101, 102, 117, 108>>

2> binary_to_term(B).
{binaries, "are", useful}

byte_size(Bin) -> Size
% This returns the number of bytes in the binary
1> byte_size(<<1,2,3,4,5>>).
5

% -------------------------------------------------------------------------

% Of all these, term_to_binary and binary_to_term are favorites. They are
% incredibly useful. term_to_binary turns any term into a binary. Inside
% the binary (if you peeked), you'll find data stored in "the Erlang
% external term format" (defined in the Erlang documentation). Once we have
% converted a term to a binary, we can send the binary in a message over a
% socket or store it in a file. This is the basic underlying method used
% for implementing distributed Erlang and is used internally in many
% databases.

% -------------------------------------------------------------------------

% 7.2 The Bit Syntax is a notation used for extracting and packing
% individual bits or sequences of bits in binary data. When you're writing
% low-level code to pack and unpack binary data at a bit level, you'll find
% the bit syntax incredibly useful. The bit syntax was developed for
% protocol programming (something that Erlang excels at) and produces
% highly efficient code for manipulating binary data.

% Suppose we have three variables - X, Y, and Z - that we want to pack into
% a 16-bit memory area. X should take 3 bits in the result, Y should take 7
% bits, and Z should take 6. In most languages this involves some messy
% low-level operations involving bit shifting and masking. In Erlang, we
% just write the following:

M = <<X:3, Y:7, Z:6>>

% This creates a binary and stores it in the variable M. Note: M is of type
% binary since the total bit length of the data is 16 bits., which is
% exactly divisible by 8. If we change the size of X to 2 bits and write
% this:

M = <<X:2, Y:7, Z:6>>

% then the total number of bits in M is 15, so the resulting data structure
% is of type bitstring.

% The full bit syntax is slightly more complex, we'll go through it in
% small steps. First, we'll look at some simple code to pack and unpack RGB
% color data into 16-bit words.

% Then, we'll dive into the details of bit syntax expressions. Finally,
% we'll look at three examples take from real-world code that uses the bit
% syntax.

% -------------------------------------------------------------------------

% Packing and Unpacking 16-Bit colors:
% Suppose we want to represent a 16-bit RGB color. We decide to allocate 5
% bits for the red channel, 6 bits for the green channel, and 5 bits for
% the blue channel. (We use one more bit for the green channel because the
% human eye is more sensitive to green light)

% We can create a 16-bit memory area Mem containing a single RGB triplet as
% follows:

1> Red = 2.
2

2> Green = 61.
61

3> Blue = 20.
20

4> Mem = <<Red:5, Green:6, Blue:5>>
<23, 180>>

% Note: In expression 4 we created a 2-byte binary containing a 16-bit
% quantity. The shell prints this as <<23,180>>

% To pack the memory, we just wrote the expression <<Red:5, Green:6,
% Blue:5>>

% To unpack the binary into integer variables, R1, G1, and B1, we write a
% pattern.

5> <<R1:5, G1:6, B1:5>> = Mem.
<<23,180>>

6> R1.
2

7> G1.
61

8> B1.
20

% That was easy. If you don't believe me, try doing that using bitshifts
% and logical ands and ors in your favorite programming language

% We can actually do far more with the bit syntax that this simple example
% suggests, but first we need to master a rather complex syntax. Once we've
% done this, we'll be able to write remarkably short code to pack and
% unpack complex binary data structures.

% -------------------------------------------------------------------------

% Bit Syntax Expressions:

% Bit syntax expressions are used to construct binaries or bitstrings. They
% have the following form:

<<>>
<<E1, E2, ..., En>>

% Each element Ei specifies a single segment of the binary or bitstring.
% Each element Ei can have one of four possible forms.

Ei = Value |
		 Value:Size |
		 Value/TypeSpecifierList |
		 Value:Size/TypeSpecifierList

% If the total nunmber of btis in the expressions is evenly divisible by 8,
% then this will construct a binary; otherwise, it will construct a
% bitstring.

% When you construct a binary, Value must be a bound variable, a literal
% string or an expression that evaluates to an integer, a float, or a
% binary. When used in a pattern matching operation, Value can be a bound
% or unbound variable, integer, literal string, float, or binary.

% Size must be an expression that evaluates to an integer. In pattern
% matching, size must be a bound variable, at the point in the pattern
% where the value is needed. The value of the Size can be obtained from
% earlier pattern matches in the binary. For example, the following:

<<Size:4, Data:Size/binary, ...>>

% is a legal pattern, since the value of Size is unpacked from the first
% four bits of the binary and then used to denote the size of the next
% segment in the binary.

% The value of Size specifies the size of the segment. The default value
% depends on the type. For an integer it is 8, for a flaot it is 64, and
% for a binary it is the size of the binary. In pattern matching, this
% default value is only for the very last element. If the size of a segment
% is not specified, a default value will be assumed.

% TypeSpecifierList is a hyphen-separated lsit of items of the form
% End-Sign-Type-Unit.

% Any of the previous items can be omitted, and the items can occur in any
% order. if an item is omitted, then a default value for the item is used.

% The items in the specifier list can have the following values:

% End is big | little | native
	
	% This specifies the endianess of the machine.
	% 'native' is determined at runtime, depending upon the CPU of your
	% machine. The default is 'big', which is also known as network byte
	% order. The only significance of this has to do with packing and
	% unpacking integers adn floats form binaries. When packing and unpacking
	% integers from binaries on different endian machines, you should take
	% care to use the correct endianess.

	% When writing bit syntax expressions, some experimentation may be
	% necessary. To assure yourself that you are doing the right thing, try
	% the following shell command:

	1> {<<16#12345678:32/big>>, <<16#12345678:32/little>>,
	<<16#12345678:32/native>>, <<16#12345678:32>>}.

	{<<18,52,86,120>>,<<120,86,52,18>>,<<120,86,52,18>>,<<18,52,86,120>>}

	% The output shows you exactly how integers are packed in a binary using
	% the bit syntax

	% In case you're worried, term_to_binary and binary_to_term "do the
	% right thing" when packing and unpacking integers. So, you can, for
	% example, create a tuple containing integers on a big-endian machine.
	% then use term_to_binary to convert the term to a binary and send this
	% to a little-endian machine. On the little-endian, you do
	% binary_to_term, and all the integers in the tuple will have the correct
	% values.

% Sign is signed | unsigned
	%  This parameter is used only in pattern matching. The default is
	% 'unsigned'.

% Type is integer | float | binary | bytes | bitstring | bits | utf8 |
% utf16 | utf32
	% The default is integer

% Unit is written unit:1 | 2 | ... 256
	% The default value Unit is 1 for integer, float, and bitstring and is 8
	% for binary. No value is required for types utf8, utf16, utf32
	% The total size of the segment is Size x Unit btis long. A segment of
	% type binary must have a size that is evenly divisible by 8.

% -------------------------------------------------------------------------

% Real World Bit Syntax Examples:

% Learning the Bit Syntax is a bit of extra effort, but the benefits are
% enormous. This section has 3 examples from real life. Al the code here is
% cut and pasted from real-world programs.

% The first example looks for synchronization points in MPEG audio data.
% This example shows the power of bit syntax pattern matching; the code is
% very easy to understand and has a clear correspondence to the MPEG header
% frame specification. The second example was used to build binary data
% files in the Microsoft Common Object File Format (COFF) format. Packing
% and unpacking binary data files (like COFF) is typically performed using
% binaries and binary pattern matching. The final example shows how to
% unpack and IPv4 datagram.

% -------------------------------------------------------------------------

% Find the Synchronization Frame in MPEG Data

% Suppose we want to write a program that manipulates MPEG audio data. We
% might want to write a streaming media server in Erlang to extract the
% data tags that describe the content of an MPEG audio stream. To do this,
% we need to identify and synchronize with the data frames in an MPEG
% stream.

% MPEG audio data is made up from a number of frames. Each frame has its
% own header followed by audio information - there is no file header, and
% in principle, you an cut an MPEG file into pieces and play any of the
% pieces. Any software that reads an MPEG stream is supposed to find the
% header frames and thereafter synchronize the MPEG data.

% An MPEG header starts with an 11-bit frame sync consisting of eleven
% sensecutive 1 bits followed by information that describes the data that
% follows.


AAAAAAAA AAABBCCD EEEEFFGH IIJJKLMM

AAAAAAAAAAA % The sync word (11 bits, all 1s)

BB % 2 bits is the MPEG audio version ID

CC % 2 bits is the layer description

% and so on

% The exact details of these bits need not concern us here. Basically,
% given knowledge of the valyes of A to M, we can compute the total length
% of an MPEG frame.

% To find the sync point, we first assume that we are correctly positioned
% at the start of an MPEG header. We then try to compute the length of the
% frame. Then one of the following can happen.

	% Our assumption was correct, so when we skip forward by the legnth of
	% the frame, we will find another MPEG header.

	% Our assumption was incorrect; either we are not positioned at a
	% sequence of 11 consecutive 1 bits that marks the start of a header or
	% the format of the word is incorrect so that we cannot compute the
	% length of the frame.

	% Our assumption was incorrect, but we are positioned at a couple of
	% bytes of music data that happen to look like the start of a header. In
	% this case, we can compute a frame length, but when we skip forward by
	% this length, we cannot find a new header.

% To be really sure, we look for three consecutive headers. The
% synchronization routine as follows:

find_sync(Bin, N) ->
	case is_header(N, Bin) of
		{ ok, Len1, _} ->
			case is_header(N + Len1, Bin) of
				{ ok, Len2, _ } ->
					case is_header(N + Len1 + Len2, Bin) of
						{ ok, _, _ } ->
							{ ok, N };
						error ->
							find_sync(Bin, N+1)
					end;
				error ->
					find_sync(Bin, N+1)
			end;
		error ->
			find_sync(Bin, N+1)
	end.


% find_sync() tries to find 3 consecutive MPEG header frames. If byte N in
% Bin is the start of a header frame, then is_header(N, Bin) will return 
% {ok, Length, Info}. If is_header() returns error, then N cannot point to
% the start of a correct frame.

% We can do a quick test in the shell to make sure this works.

1> c(mp3_sync).
{ok, mp3_sync}

2> {ok, Bin} = file:read("/home/joe/music/mymusic.mp3").
{ok, <<73,68,51,0......>>}

3> mp3_sync:find_sync(Bin, 1).
{ok,4256}

% This uses file:read_file to read the entire file into a binary (see
% Reading the Entire File into a Binary pg 248). Now for is_header()

% mp3_sync.erl

is_header(N, Bin) ->
	unpack_header(get_word(N, Bin)).

get_word(N, Bin) ->
	{ _, <<C:4/binary,_/binary>> } = split_binary(Bin, N),
	C.

unpack_header(X) ->
	try decode_header(X)
	catch
		_:_ -> error

% This is slightly more complicated. First we extract 32 bits of data to
% analyze (this is done by get_word); then we unpack the header using
% decode_header(). Now the decode_header() is written to crash (by calling
% exit/1) if its argument is not at the start of a header. To catch any
% errors, we wrap the call to decode_header() in a try .... catch statement

% This will also cantch any errors that might be casued by incorrect code
% in framelength/4. decode_header() is where all the fun starts

% mp3_sync.erl

decode_header(
	<<2#11111111111:11, B:2, C:2, _D:1, E:4, F:2, G:1, Bits:9>>
) ->
	Vsn = case B of
					0 -> {2,5};
					1 -> exit(badVsn);
					2 -> 2;
					3 -> 1
				end,

	Layer = case C of
						0 -> exit(badLayer);
						1 -> 3;
						2 -> 2;
						3 -> 1
					end,

	% Protection = D,
	BitRate = bitrate(Vsn, Layer, E) * 1000,
	SampleRate = sampeRate(Vsn, F),
	Padding = G,
	FrameLength = frameLength(Layer, BitRate, SampleRate, Padding),
	if
		FrameLength < 21 ->
			exit(frameSize);
		true ->
			{ok, FrameLength, {Layer, BitRate, SampleRate, Vsn, Bits}}
	end;

decode_header(_) ->
	exit(badHeader).


% The magic lies in the amazing expression in the first line of code.

decode_header(<<2#11111111111:11, B:2, C:2, _D:1, E:4, F:2, G:1, Bits:9>>)

% 2#11111111111:11 is a base 2 integer, so this pattern matches eleven
% consecutive 1 bits, 2 bits into B, 2 bits into C, and so on.

% Note: The code exactly follows the bit-level specification of the MPEG
% header given earlier. More beautiful and direct code would be difficult
% to write. This code is beautiful and also highly efficient. The Erlang
% compiler turns the bit syntax patterns into highly optimized code that
% extracts the fields in an optimal manner.

% -------------------------------------------------------------------------

% Unpacking COFF Data

% A few years ago I decided to write a program to make stand-alone Erlang
% programs that would run on Windows ---- I wanted to build a Windows
% executable on any machine that could run Erlang. Doing this involved
% understanding and manipulating the Microsoft Common Object File Format 
% (COFF)-formatted files. Finding out the details of COFF was pretty tricky
%, but various APIs for C++ programs were documented. The C++ programs used
% the tupe declarations DWORD, LONG, WORD, and BYTE; these type
% declarations will be familiar to programmers who have programmed Windows
% internals.

% The data structures involved were documented, but only from a C or C++
% programmer's point of view.

% The following is a typical C typedef:

typdef struct _IMAGE_RESOURCE_DIRECTORY {
	DWORD Characteristics;
	DWORD TimeDataStamp;
	WORD MajorVersion;
	WORD MinorVersion;
	WORD NumberOfNamedEntries;
	WORD NumberOfIdEntries;
} IMAGE_RESOURCE_DIRECTORY, *PIMAGE_RESOURCE_DIRECTORY;

% To write the equivalent Erlang program, I first defined four macros tht
% must be included in the Erlang source code file.

-define(DWORD, 32/unsigned-little-integer).
-define(LONG, 32/unsigned-little-integer).
-define(WORD, 16/unsigned-little-integer).
-define(BYTE, 8/unsigned-little-integer).

% Note: Macros are explained on page 129

% To expand these macros, we use the syntax ?DWORD, ?LONG, and so on. For
% example, the macro ?DWORD expands to the literal text
% 32/unsigned-little-integer

% These macros deliberately have the same names as their c counterparts.
% Armed with these macros, I could easily write some code to unpack iamge
% resource data into a binary.

unpack_image_resource_directory(Dir) ->
	<<Characteristics					: ?DWORD,
		TimeStampData 					: ?DWORD,
		MajorVersion						: ?WORD,
		MinorVersion						: ?WORD,
		NumberOfNamedEntries		: ?WORD,
		NumberOfIdEntries 			: ?WORD, _/binary>> = Dir,

% If you compare the C and Erlang code, you'll see that they are pretty
% similar. So, by taking care with the names of the macros and the layout
% of the Erlang code, we can minimize the semantic gap between the C code
% and the Erlang code, something that makes our program easier to
% understand and less likely to have errors.

% The next step was to unpack data in Characteristics, and so on.

% Characteristics is a 32-bit word consisting of a collection of flags.
% Unpacking these using the bit syntax is extremely easy; we just write
% code like this:

<<ImageFilerelocsStripped:1, ImageFileExecutableImage:1, ...>> =
<<Characteristics:32>>

% The code <<Characteristics:32>> connverted Characteristics, which was an
% integer into a binary of 32 bit. Then the following code unpacked the
% required bits into the variables ImageFileRelocsStripped,
% ImageFileExecutableImage, and so on:

<<ImageFilerelocsStripped:1, ImageFileExecutableImage:1,...>> = ...

% Again, I kept the same names as in the Windows API in order to keep the
% semantic gap between the specification and the Erlang program to a
% minimum.

% Using these macros made unpacking data i nthe COFF format... well, I
% can't really use the word easy, but the code was reasonably
% understandable.

% -------------------------------------------------------------------------

% Unpacking the Header of an IPv4 Datagram

% This example illustrates parsing an Internet Protocol version 4 (IPv4)
% datagram in a single patthern-matching operation:

-define(IP_VERSION, 4).
-define(IP_MIN_HDR_LEN, 5).

DgramSize = byte_size(Dgram),
case Dgram of
	<<?IP_VERSION:4, HLen:4, SrvcType:8, TotLen:16, ID:16, Flags:3,
	FragOff:13, TTL:8, Proto:8, HdrChkSum:16, SrcIP:32,DestIP:32,
	RestDgrm/binary>> when HLen >= 5, 4*HLen =< DgramSize ->
		OptsLen = 4 * (HLen - ?IP_MIN_HDR_LEN),
		<<Opts:OptsLen/binary, Data/binary>> = RestDgrm,
		...
end.

% This code matches an IP datagram in a single pattern-matching expression.
% The pattern is complex and illustrates how data that does not fall on
% byte boundaries can easily be extracted (for example, the Flags and
% FragOff fields taht are 3 and 13 bits long, respectively). Having a
% pattern matched the IP datagram, the header and data part of the datagram
% are extracted in a second pattern matching operation.

% We've now covered bit field operations on binaries. Recall that binaries
% must be multiple of eight bits long. The next section covers bitstrings,
% which are used to store sequences of bits.

% -------------------------------------------------------------------------

% 7.3: Bitstrings: Processing Bit-Level Data

% Pattern matching on bitstrings works at a bit level, so we can pack and
% unpack sequences of bits into a bitstring in a single operation. This is
% extremely useful when writing code that needs to manipulate bit-level
% data, such as with data that is not aligned to 8-bit boundaries, or
% variable-length data, where the data length is expressed in bits rather
% than bytes.

% We can illustrate bit-level processing in the shell.

1> B1 = <<1:8>>.
<<1>>

2> byte_size(B1).
1

3> is_binary(B1).
true

4> is_bitstring(B1).
true

5> B2 = <<1:17>>.
<<0,0,1:1>>

6> is_binary(B2).
false

7> is_bitstring(B2).
true

8> byte_size(B2).
3

9> bit_size(B2).
17


% -------------------------------------------------------------------------

% Notes: Bit-Level Storage

% In most programming languages, the least addressable unit of storage is
% typically 8 bits wide. Most C compilers, for example, define a char (the
% least addressable unit of storage) to be 8 bits wide. Manipulating bits
% within a char is complicated, since to access individual bits, they have
% to be masked out and shifted into registers. Writing such code is tricky
% and error-prone.

% In Erlang, the least addressable unit of storage is a bit, and individual
% sequences of bits within a bitstring can be accessed directly without any
% shifting and masking operations.




% in the previous example, B1 is a binary, but B2 is a bitstring since it
% is 17 bits long. We construct B2 with the syntax <<1:17>>, and it is
% printed as <<0,0,1:1>>, that is, as a binary literal whose third segment
% is a bitstring of length 1. The bit size of B2 is 17, and the byte size
% is 3 (this is actually the size of the binary that contains the
% bitstring)

% Working with bitstrings is tricky. We can't, for example, write a
% bitstring to a file or socket (which we can do with a binary), sicne
% files and sockets work in units of bytes.

% We'll conclude this section with a single example, which extracts the
% individual bits of a byte. To do so, we'll make use of a new construct
% called a bit comprehension. Bit comprehensions are to binaries what list
% comprehensions are to lists. List comprehensions iterate over lists and
% return lists. Bit comprehensions iterate over binaries adn produce lists
% or binaries.

% This example shows how to extract the bits from a byte:

1> B = <<16#5f>>.
<<"_">>

2> [ X || <<X:1>> <= B ].
[0,1,0,1,1,1,1,1]

3> << <<X>> || <<X:1>> <= B >>.
<<0,1,0,1,1,1,1,1>>

% In line 1 we made a binary that contains a single byte. 16#5f is a
% hexadecimal constant. The shell prints this as <<"_">> since 16#f5 is the
% ASCII code for the _ character (ascii code is 95).

% In line 2, the syntax <<X:1>> is a pattern representing one bit. The
% result is a list of the bits in the byte.

% Line 3 is similar to line 2, only we construct a binary from the bits
% instead of a list.

% The syntax of bit comprehensions is not described here but can be found
% in the Erlang Reference Manual. More examples of bitstring processing can
% be found in the paper "Bit-Level Binaries and Generalized Comprehensions
% in Erlang."

% Now we know about binaries and bitstrings. Binaries are used internally
% in the Erlang system whenever we want to manage large amounts of
% unstructured data. In later chapters, we'll see how binaries can be sent
% in messages over sockets and stored in files.

% We're almost done with sequential programming. What remains are a number
% of small topics; there's nothing really fundamental or exciting, but
% they're useful subjects to know.