% The Req Object

	% The Req object is a variable used for obtaining information about a
	% request, read its body or send a response.

	% It is not really an object in the object-oriented sense. It is a simple
	% map that can be directly accessed or used when calling functions from
	% the "cowboy_req" module.

	% The Req object is the subject of a few different chapters. In this
	% chapter we will learn about the Req object and look at how to retrieve
	% information about the request.

% -------------------------------------------------------------------------

% Direct Access

	% The Req map contains a number of fields which are documented and can be
	% accessed directly. They are the fields that have a direct mapping to
	% HTTP:

		% the request "method";
		% the HTTP "version" used;
		% the effective URI components "scheme", "host", "port", "path", and
		% "qs";
		% the request "headers";
		% the connection "peer" address and port;
		% and the TLS certificate "cert" when applicable

	% Note that the "version" field can be used to determine whether a
	% connection is using HTTP/2.

	% To access a field, you can simply match the function head. The
	% following example sends a simple "Hello world!" response when the
	% "method" is GET, and a 405 error otherwise.

		init(Req0=#{method := <<"GET">>}, State) ->
			Req = cowboy_req:reply(200, #{
				<<"content-type">> => <<"text/plain">>
				}, <<"Hello world!">>, Req0),
			{ok, Req, State};

		init(Req0, State) ->
			Req = cowboy_req:reply(405, #{
				<<"allow">> => <<"GET">>
				}, Req0),
			{ok, Req, State}.

	% Any other field is internal and should not be accessed. They may change
	% in future releases including maintenance releases without notice.

	% Modifying the Req object is allowed, but extra caution must be used
	% when modifying existing fields. You can add as many new fields as
	% necessary, however, just make sure to namespace the field with names so
	% that no conflict can occur with future Cowboy updates or with third
	% party projects.

% -------------------------------------------------------------------------

% Introduction to the cowboy_req interface

	% Functions in the "cowboy_req" module provide access to the request
	% information but also various operations that are common when dealing
	% with HTTP requests.

	% All the functions that begin with a verb indicate an action. Other
	% functions simply return the corresponding value (sometimes that value
	% does need to be built, but the cost of the operation is equivalent to
	% retrievign a value).

	% Some of the "cowboy_req" functions return an updated Req object. They
	% are the read, reply, set and delete functions. While ignoring the
	% returned Req will not cause incorrect behavior for some of them, it is
	% highly recommended to always keep and use the last returned req object.
	% The manual for "cowboy_req" details these functions and what
	% modifications are done to the Req object.

	% Some of the calls to "cowboy_req" have side effects. This is the case
	% of the read and reply functions. Cowboy reads the request body or
	% replies immediately when the function is called.

	% All functions will crash if something goes wrong. There is usually no
	% need to catch these errors, Cowboy will send the appropriate 4xx or 5xx
	% response 

% -------------------------------------------------------------------------

% Request Method

	% The request method can be retrieved directly at

		#{method := Method} = Req.

	% Or using a function

		Method = cowboy_req:method(Req).

	% The method is a case sensitive binary string. Standard methods include
	% "GET", "HEAD", "OPTIONS", "PATCH", "POST", "PUT", or "DELETE".

% -------------------------------------------------------------------------

% HTTP Version

	% The HTTP version is informational. It does not indicate that the client
	% implements the protocol well or fully.

	% There is typically no need to change behavior based on the HTTP
	% version: Cowboy already does it for you.

	% It can be useful in some cases though. For example, one may want to
	% redirect HTTP/1.1 clients to use Websocket, while HTTP/2 clients keep
	% using HTTP/2.

	% The HTTP version can be retrieved directly.

		#{version := Version} = Req.

	% or using a function:

		Version = cowboy_req:version(Req).

	% Cowboy defines the 'HTTP/1.0'. 'HTTP/1.1' and 'HTTP/2' versions. Custom
	% protocols can define their own values as atoms.

% -------------------------------------------------------------------------

% Effective Request URI

	% The scheme, host, port, path, and query string components of the
	% effective request URI can all be retrieved directly at:

		#{
			scheme := Scheme,
			host 	 := Host,
			port 	 := Port,
			path 	 := Path,
			qs 		 := Qs
		} = Req.

	% or using the related functions

		Scheme = cowboy_req:scheme(Req),
		Host = cowboy_req:host(Req),
		Port = cowboy_req:port(Req),
		Path = cowboy_req:path(Req).
		Qs = cowboy_req:qs(Req).

	% The scheme and host are lowercased case insensitive binary strings. The
	% port is an integer representing the port number. The path and query
	% string are case sensitive binary strings.

	% Cowboy defines only the <<"http">> and <<"https">> schemes. They are
	% chosen so that the scheme will only be <<"https">> for requests on
	% secure HTTP/1.1 or HTTP/2 connections.

	% The effective request URI itself can be reconstructed with the
	% cowboy_req:uri/1,2 function.

	% By default, an absolute URI is returned:

		%% scheme://host[:port]/path[?qs]
		URI = cowboy_req:uri(Req).

	% Options are available to either disable or replacesome or all of the
	% components. Various URIs or URI formats can be generated this way,
	% including the origin form:

		%% /path[?qs]
		URI = cowboy_req:uri(Req, #{host => undefined}).

	% The protocol relative form:

		%% //host[:port]/path[?qs]
		URI = cowboy_req:uri(Req, #{scheme = undefined}).

	% The absolute URI without a query string:

		URI = cowboy_req:uri(Req, #{qs = undefined}).

	% A different host

		URI = cowboy_req:uri(Req, #{host => <<"example.org">>}).

	% And any other combination


% -------------------------------------------------------------------------

% Bindings

	% Bindings are the host and path components that you chose to extract
	% when defining the routes of your application. They are only available
	% after the routing.

	% Cowboy provides functions to retrieve one or all bindings.

	% To retrieve a single value:

		Value = cowboy_req:binding(userid, Req).

	% When attempting to retrieve a value that was not bound, "undefined"
	% will be returned. A different default value can be provided:

		Value = cowboy_req:binding(userid, Req, 42).

	% To retrieve everything that was bound:

		Bindings = cowboy_req:bindings(Req).

	% They are returned as a map, with keys being atoms.

	% The Cowboy router also allows you to capture many host or path segments
	% at once using the ... qualifier.

	% To retrieve the segments captured from the host name:

		HostInfo = cowboy_req:host_info(Req).

	% And the path segments:

		PathInfo = cowboy_req:path_info(Req).

	% Cowboy will return "undefined" if "..." was not used in the route

% -------------------------------------------------------------------------

% Query Parameters

	% Cowboy provides two functions to access query parameters. You can use
	% the first to get the entire list of parameters.

		QsVals = cowboy_req:parse_qs(Req),
		{_, Lang} = lists:keyfind(<<"lang">>, 1, QsVals).

	% Cowboy will only parse the query string, and not do any transformation.
	% This function may therefore return duplicates, or parameter names
	% without an associated value. the order of the list returned is
	% undefined.

	% When a query string is key=1&key=2, the list returned will contain two
	% parameters of name "key".

	% The same is true wheh trying to use the PHP-style suffix "[]". When a
	% query string is "key[]=1&key[]=2", the list returned will contain two
	% parameters of name "key[]".

	% When a query string is simply "key", Cowboy will return the list 
	% "[{<<"key">>, true}]", using "true" to indicate that the parameter
	% "key" was defined, but with no value.

	% The second function function Cowboy provides allows you to match out
	% only the parameters you are interested in, and at the same time do any
	% post processing you require using constraints.

	% This function returns a map:


		#{id := ID, lang := Lang} = cowboy_req:match_qs([id, lang], Req).

	% Constraints can be applied automatically. The following snippet will
	% crash when the "id" parameter is not an integer, or when the "lang"
	% parameter is empty. At the same time, the value for "id" will be
	% converted to an integer term:

		QsMap = cowboy_req:match_qs([{id, int}, {lang, nonempty}], Req).

	%  A default value may also be provided. The default will be used if the
	% "lang" key is not found but has an empty value.

		#{lang := Lang} = cowboy_req:match_qs([{lang, [], <<"en-US">>}], Req).

	% If no default is provided and the value is missing, the query string is
	% deemed invalid and the process will crash.

	% When the query string is "key=1&key=2", the value for "key" will be the
	% list "[1, 2]". Parameter names do not need to include the PHP-style
	% suffix. Constraints may be used to ensure that only one value was
	% passed through.

% -------------------------------------------------------------------------

% Headers

	% Header values can be retrieved either as a binary string or parsed into
	% a more meaningful representation.

	% To get the raw value:

		HeaderVal = cowboy_req:header(<<"content-type">>, Req).

	% Cowboy expects all header names to be provided as lowercase binary
	% strings. This is true for both requests and responses, regardless of
	% the underlying protocol.

	% When a header is missing from the request, "undefined" will be
	% returned. A different default can be provided:

		HeaderVal = cowboy_req:header(<<"content-type">>, Req,
			<<"text/plain">>).

	% All headers can be retrieved once, either directly:

		#{headers := AllHeaders} = Req.

	% Or using a function

		AllHeaders = cowboy_req:headers(Req).

	% Cowboy provides equivalent functions to parse indiividual headers.
	% There is no function to parse all headers at once.

	% To parse a specific header:

		ParsedVal = cowboy_req:parse_header(<<"content-type">>, Req).

	% An exception will be thrown if it doesn't know how to parse the given
	% header, or if the value is invalid. The list of known headers and
	% default values can be found in the manual.

	% When the header is missing, "undefined" is returned. You can change the
	% default value. Note that it should be the parsed value directly:

		ParsedVal = cowboy_req:parse_header(<<"content-type">>, Req, 
			{<<"text">>, <<"plain">>, []}).

% -------------------------------------------------------------------------

% Peer

	% The peer address and port number for the connection can be retrieved
	% either directly or using a function.

	% The retrieve the peer directly:

		#{peer := {IP, Port}} = Req.

	% And using a function

		{IP, Port} = cowboy_req:peer(Req).

	% Note that the peer corresponds to the remote end of the connection to
	% the server, which may or may not be the client itself.

	