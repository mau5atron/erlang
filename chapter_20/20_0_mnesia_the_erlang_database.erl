% Chapter 20: Mnesia - The Erlang Database

	% Suppose you want to write a multiuser game, make a new website, or
	% create an online payment system. You'll probably need a database
	% management system (DBMS).

	% Mnesia is a database written in Erlang for demanding telecommunications
	% applications, and it is part of the standard Erlang distribution. It
	% can be configured with RAM replicates on two physically separated nodes
	% to provide a fast fault-tolerant data store. It provides transactions
	% and comes with its own query language.

	% Mnesia is extremely fast, and it can store any type of Erlang data
	% structure. It's also highly configurable. Database tables can be stored
	% in RAM (for speed) or on disk (for persistence), and the tables can be
	% replicated on different machines to provide fault-tolerant behavior.