% Chapter 12 Exercises:

% 1.

	% Write a function start(AnAtom, Fun) to register AnAtom as spawn(Fun).
	% Make sure your program works correctly in the case when two parallel
	% processes simultaneously evauate start/2. In this case, you must
	% guarantee that one of these processes succeeds and the other fails.

% 2.

	% Measure the process spawning time on your machine, usingthe program in
	% section 12.3, pg 189. Plot a graph of the number of processes against
	% the process creation time. What can you deduce from the graph?

% 3.

	% Write a ring benchmark. Create N processes in a ring. Send a message
	% round the ring M times so that a total of N * M messages get sent. Time
	% how long this takes for different values of N and M.

	% Write a similar program in some other programming language you are
	% familiar with. Compare the results. Write a blog and publish the
	% results on the internet.
