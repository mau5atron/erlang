-module(afile_server).
-export([init/0, before/1, start/1, loop/1]).

init() ->
	io:format("Initializing shell....\n").

before(Dir) ->
	%io:format("Args: ~p\n", [Dir]).
	io:format("file_list: ~p\n", [file:list_dir(Dir)]).


% spawn(Module, Function, Args) -> pid()
start(Dir) ->
	spawn(afile_server, loop, [Dir]).


loop(Dir) -> % writing an infinite loop
	% wait to receive a command
	receive
		% execute commands
		{Client, list_dir} -> % respond with a list of files from the directory
			Client ! {self(), file:list_dir(Dir)};

		{Client, {get_file, File}} -> % respond with a specific file
			Full = filename:join(Dir, File),
			Client ! {self(), file:read_file(Full)};

		% {Client, {put_file, File, Bytes}} ->
			% Full = filename:join(Dir, File),
			% file:write_file(Full, Bytes),
			% Client ! { self(), file_sent }
		{Client, {put_file, Content, NewFileName}} ->
			% responding with the content of a file into a new file
			Content, % getting content
			Full = filename:join(Dir, NewFileName), % dir and new file name
			Return = file:write_file(Full, Content), % write to the new file in
			% specified directory with content of the source file
			Client ! { self(), { Return, Full, Content } }
	end,
	% call recursively to get the next command
	loop(Dir).
% loop never returns anything


                                                                                                                       
% +------------+                                                                                                          
% |            |                                                                                                                                                                                                        
% |   Client   | = Process identifier of the process that sent the request and 
%	|						 |	 to whom the reply should be sent                                                                                                         
% +------------+   

% +-----------+  The reply sent by the server contains the argument self() 
% |						|	 (in this case self() is the process identifier of the server)
% |  			    |                                                                                                         
% |  self()   |  =   This identifier is added to the message so that the client 
%	|						|			 can check that the message the client received came from the 
%	|						|			 server and not some other process
% +-----------+                                            

% Pattern matching is used to select the message
% 	The inside of the receive statement has two patterns. We just write them like this:

% 	receive
% 		Pattern1 ->
% 			Actions1;
% 		Pattern2 ->
% 			Actions2 ->
% 		...
% 	end

% The Erlang compiler and runtime will correctly figure out how to run the appropriate code when
% a message is received. We don't have to write any if-then-else or switch statements to work out what to do.
% This is one of the joys of pattern matching, which will save you lots of work.



