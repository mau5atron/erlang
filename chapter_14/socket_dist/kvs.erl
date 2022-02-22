-module(kvs).
-export([start/0, store/2, lookup/1]).

% -spec kvs:start() -> true.
% -spec kvs:store(Key,Value) -> true.
% -spec kvs:lookup(Key) -> {ok,Value} | undefined.

start() ->
	% spawns and registers the process as kvs for easy address access
	% With the loop function pattern matching messages sent to the process
	register(kvs, spawn(fun() -> loop() end)).

store(Key,Value) ->
	rpc({store,Key,Value}).

lookup(Key) ->
	rpc({lookup,Key}).

rpc(Q) ->
	% self() becomes "From" inside loop() function
	% Q is the query to be pattern matched inside loop()
 	kvs ! {self(), Q},
	receive 
		{kvs,Reply} ->
			Reply
	end.

loop() ->
	receive
		{From, {store,Key,Value}} ->
			put(Key, {ok,Value}),
			From ! {kvs,true},
			loop();
		{From, {lookup,Key}} ->
			From ! {kvs,get(Key)},
			loop()
	end.

% Usage example:

% 5> kvs:start()
% 6> whereis(kvs).
% <0.95.0>

% 7> kvs:store({location, gabriel}, "Florida").
% true
% 8> kvs:store(last_name, "Betancourt").       
% true
% 9> kvs:lookup(last_name).
% {ok,"Betancourt"}
% 10> kvs:lookup({location, gabriel}).
% {ok,"Florida"}