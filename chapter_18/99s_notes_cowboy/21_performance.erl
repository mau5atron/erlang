% Performance

	% This chapter describes the performance characteristics of Cowboy and
	% offers suggestions to get the most performance out of your application.

% -------------------------------------------------------------------------

% One Process per Connection

	% The first version of Cowboy featured a single process per connection,
	% whereas the current version of Cowboy features one process per
	% connection plus one process per request. This has a negative impact on
	% performance, but is necessary in order to provide a common interface
	% for both HTTP/1.1 and HTTP/2 (as well as future HTTP versions).

	% It is still possible to use a single process per connection, and avoid
	% the creation of additional processes for each request, by implementing
	% a stream handler to process the requests. This can be done for all
	% requests, or just for a single endpoint depending on the application's
	% needs.

	% Stream handlers provide an asynchronous interface and must not block,
	% so the implementation will be very different from normal Cowboy
	% handlers, but the performance gains are important enough to justify it
	% in some cases.
