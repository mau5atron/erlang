-module(hello).
-export([start/0]).

start() ->
  io:format("Hello world!~n").

% to compile, start erlang shell in same directory as hello.erl
% $ erl
% 1> c(hello).
% 2> hello:start().
% Hello world!
% 3> halt().
% $
% 
% To compile outside the shell
% 
% erlc hello.erl
% erl -noshell -s hello start -s init stop
% This is the preferred way of compiling erlang as we can use automation within makefiles or rakefile to automate the build process
% 