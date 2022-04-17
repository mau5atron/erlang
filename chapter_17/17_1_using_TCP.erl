% 17.1 Using TCP:

% We'll start our adventures in socket programming by looking at a simple
% TCP program that fetches data from a server. After this, we'll write a
% simple sequential TCP server and show how it can be parallelized to
% handle multiple parallel sessions.

% Fetching Data From a Server:

	% Let's start by writing a little function called nano_get_url/0 that
	% uses a TCP socket to fetch an HTML page from https://www.google.com

	% look at socket_examples.erl