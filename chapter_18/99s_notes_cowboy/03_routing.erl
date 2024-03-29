% Routing

	% Cowboy does nothing by default.

	% To make Cowboy useful, you need to map URI to Erlang modules that will
	% handle the requests. this is called routing.

	% Cowboy routes requests using the following algorithm:

		% If no configured host matches the request URI, a 400 response is
		% returned.

		% Otherwise, the first configured host that matches the request URI
		% will be used. Only the paths configured for this host will be
		% considered.

		% If none of the configured paths found in the previous step match the
		% request URI, a 404 response is returned.

		% Otherwise, the handler and its initial state are added to the
		% environment and the request continues to be processed.

	% NOTE: it is possible to run into a situation where two hosts match a
	% request URI, but only the paths on the second host match the request
	% URI. In this case the expected result is a 404 response because the
	% only paths used udring routing are the paths from the first configured
	% host that matches the request URI.

	% Routes need to be compiled before they can be used by Cowboy. The
	% result of the compilation is the dispatch rules.

% -------------------------------------------------------------------------

% Syntax

	% The general structure for the routes is defined as follow:

		% Routes = [Host1, Host2, .... HostN].

	% Each host contains matching rules for the host along with optional
	% constraints, and a list of routes for the path component.

		% Host1 = {HostMatch, PathList}.
		% Host2 = {HostMatch, Constraints, PathList}.

	% the list of routes for the path component is defined similar to the
	% list of hosts.

		% PathList = [Path1, Path2, .... PathN].
	
	% Finally, each path contains matching rules for the path along with
	% optional constraints, and gives us the handler module to be used along
	% with its initial state.

		% Path1 = {PathMatch, Handler, InitialState}.
		% Path2 = {PathMatch, Constraints, Handler, InitialState}.

	% Continue reading to learn more about the match syntax and the optional
	% constraints.

% -------------------------------------------------------------------------

% Match Syntax
	
	% The match syntax is used to associate host names and paths with their
	% respective handlers. The match syntax is the same for host and path
	% with a few subleties. Indeed, the segments separator is different, and
	% the host is matched starting from the last segment going to the first.
	% All examples will feature both host and path match rules and explain
	% the differences when encountered.

	% Excluding special values that we will explain at the end of this
	% section, the simplest match value is a host or a path. It can be given
	% as either a string() or a binary().

		% PathMatch1 = "/".
		% PathMatch2 = "/path/to/resource".
		% HostMatch1 = "cowboy.example.org".

	% As you can see, all paths defined this way must start with a slash
	% character. Note that these two paths are identical as far as routing is
	% concerned.

		% PathMatch2 = "/path/to/resource".
		% PathMatch3 = "/path/to/resource".

	% Hosts with and without a trailing dot are equivalent for routing.
	% Similarly, hosts with and without a leading dot are also equivalent.

		% HostMatch1 = "cowboy.example.org".
		% HostMatch2 = "cowboy.example.org.".
		% HostMatch3 = ".cowboy.example.org".

	% It is possible to extract segments of the host and path and to store
	% the values in the Req object for later use. We call these kind of
	% values bindings.

	% The syntax for bindings is very simple. A segment that begins with the
	% ":" character means that what follows until the end of the segment is
	% the name of the binding in which the segment value will be stored.

		% PathMatch = "/hats/:name/prices".
		% HostMatch = ":subdomain.example.org".
	
	% If these two end up matching when routing, you will end up with two
	% bindings defined, subdomain and name, each containing the segment value
	% where they were defined. For example, the URL
	% "http://test.example.org/hats/wild_cowboy_legendary/prices" will result
	% in having the value "test" bound to the name "subdomain" and the value
	% "wild_cowboy_legendary" bound to the "name". They can later be
	% retrieved using "cowboy_req:binding/{2,3}". The binding name must be
	% given as an atom.

	% There is a special binding name you can use to mimic the underscore
	% variable in Erlang. Any match against the "_" binding will succeed but
	% the data will be discarded. This is especially useful for matching
	% against many domain names in one go.

		% HostMatch = "ninenines.:_".

	% Similarly, it is possible to have optional segments. Anything between
	% brackets is optional.

		% PathMatch = "/hats/[page/:number]".
		% HostMatch = "[www.]ninenines.eu".

	% You can also have imbricated optional segments.

		% PathMatch = "/hats/[page/[:number]]".

	% While Cowboy does not reject multiple brackets in a route, the
	% behavior may be undefined if the route is under-specified. For example,
	% this route requires constraints to determine what is a chapter and what
	% is a page, since they are both optional:

		% PathMatch = "/book/[:chapter]/[:page]".

	% You can retrieve the rest of the host or path using "[...]". In the
	% case of hosts, it will match anything before. In the case of paths,
	% anything after the previously matched segments. You can then find the
	% segments using cowboy_req:host_info/1 and cowboy_req:path_info/1
	% respectively. They will be represented as a list of segments.

		% PathMatch = "/hats/[...]".
		% HostMatch = "[...]ninenines.eu".

	% If a binding appears twice in the routing rules, then the match will
	% succeed only if they share the same value. This copies the Erlang
	% pattern matching behavior.

		% PathMatch = "/hats/:name/:name".

	% This is also true when an optional segment is present. In this case the
	% two values must be identical only if the segment is available.

		% PathMatch = "/hats/:name/[:name]".

	% If a binding is defined in both the host and path, then they must also
	% share the same value.
		
		% PathMatch = "/:user/[...]".
		% HostMatch = ":user.github.com".

	% Finally, there are two special match values that can be used. The first
	% is the atom '_' which will match any host or path.

		% PathMatch = '_'.
		% HostMatch = '_'.

	% The second is the speical host match "*" which will match the wildcard
	% path, generally used alongside the "OPTIONS" method.

		% HostMatch = "*".

% -------------------------------------------------------------------------

% Constraints
	
	% After the matching has completed, the resulting bindings can be tested
	% against a set of constraints. Constraints are only tested when the
	% binding is defined. They run in the order you defined them. The match
	% will succeed only if they all succeed. If the match fails, then Cowboy
	% tries the next route in the list.

	% The format used for constraints is the same as match functions in
	% "cowboy_req": they are provided as a list of fields which may have one
	% or more constraints. While the routere accepts the same format, it will
	% skip fields with no constraints and will also ignore default values, if
	% any.

	% Read more about constrains "https://ninenines.eu/docs/en/cowboy/2.9/guide/constraints"


% -------------------------------------------------------------------------

% Compilation

	% The routes must be compiled before Cowboy can use them. The compilation
	% step normalizes the routes to simplify the code and speed up the
	% execution, but the routes are still looked up one by one in the end.
	% Faster compilation strategies could be to compile the routes directly
	% to Erlang code, but would require heavier dependencies.

	% To compile routes. just call the appropriate function:

		Dispatch = cowboy_router:compile([
			% {HostMatch, list({PathMatch, Handler, InitialState})}
			{'_', [{'_', my_handler, #{}}]}
		]),

		% Name, TransOpts, ProtoOpts
		cowboy:start_clear(my_http_listener, [{port, 8080}],
			#{env => #{dispatch => Dispatch}}).

% -------------------------------------------------------------------------

% Using persistent_term

	% The routes can be stored in persistent_term starting from Erlang/OTP
	% 21.2. This may give a performance improvement when there are large
	% number of routes.

	% To use this functionality you need to compile the routes, store them in
	% persistent_term and then inform Cowboy:

		Dispatch = cowboy_router:compile([
			{'_', [{'_', my_handler, #{}}]}
		]),
		persistent_term:put(my_app_dispatch, Dispatch),
		cowboy:start_clear(my_http_listener, [{port, 8080}], 
			#{env => {dispath => {persistent_term, my_app_dispatch}}}).

% -------------------------------------------------------------------------

% Live Update

	% You can use the "cowboy:set_env/3" function for updating the dispatch
	% list used by routing. This will apply to all new connections accepted
	% by the listener:

		Dispatch = cowboy_router:compile(Routes),
		cowboy:set_env(my_http_listener, dispatch, Dispatch).

	% Note that you need to compile the routes again before updating.
	
	% When using "persistent_term" there is no need to call this function,
	% you can simply put the new routes in the storage.