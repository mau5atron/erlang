-module(afile_server).
-export([start/1, loop/1]).

start(Dir) ->
  spawn(afile_server, loop, [Dir]).

loop(Dir) ->
  % wait for command
  receive
    % receive command, do something
    % this is called a pattern (pattern matching)
    { Client, list_dir } ->
      % list directory?
      % this is called an action
      Client ! { self(), file:list_dir(Dir) };
    { Client, { get_file, File } } ->
      % list file in directory
      Full = filename:join(Dir, File),
      Client ! { self(), file:read_file(Full) }

  end,
  loop(Dir).
  % this is an infinite loop, however, we won't run out of stack space
  % var Dir contains the current working directory of the file server
  % wait to receive a command in the loop
  % when a command is received, obey the command and then call the loop method again to get the next command
  
  % Erlang applies a tail-call optimization to the code, which means the code runs in constant space (we dont run out of stack space)

  % loop is a function that never returns
  % in sequential programming, we have to avoid infinite loops since we only have one thread of control, if the thread gets stuck, we are in trouble
  % no such problem in erlang
  % a server is just a program that services requests in an infinite loop and that runs in parallel with any other tasks that we want to perform



  % To run this program, enter:
  % $ erl
  % $ c(afile_server)
  % 
