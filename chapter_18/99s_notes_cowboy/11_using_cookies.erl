% Using Cookies

	% Cookies are a mechanism allowing applications to maintain state on top
	% of the stateless HTTP protocol.

	% Cookies are a name/value store where the names and values are stored in
	% plain text. They expire either after a delay or when the browser
	% closes. They can be configured on a specific domain name or path, and
	% restricted to secure resources (sent or downloaded over HTTPS), or
	% restricted to the server (disallowing access from client-side scripts).

	% Cookie names are de facto case sensitive.

	% Cookies are stored client-side and sent with every subsequent request
	% that matches the domain and path for which they were stored, until they
	% expire. This can create a non-negligible cost.

	% Cookies should not be considered secure. They are stored on the user's
	% computer in plain text, and can be read by any program. They can also
	% be read by proxies when using clear connections. Always validate the
	% value before using it, and never store any sensitive information inside
	% it.

	% Cookies set by the server are only available in requests following the
	% client reception of the response containing them.

	% Cookies may be sent repeatedly. This is often useful to update the
	% expiration time and avoid losing a cookie.

% -------------------------------------------------------------------------

% Setting Cookies

	% By default cookies are defined for the duration of the session:

		SessionID = generate_session_id(),
		Req = cowboy_req:set_resp_cookie(<<"sessionid">>, SessionID, Req0).

	% They can also be set for a duration in seconds:

		SessionID = generate_session_id(),
		Req = cowboy_req:set_resp_cookie(<<"sessionid">>, SessionID, Req0, #
			{max_age => 3600}).

	% To delete cookies, set "max_age" to 0:

		SessionID = generate_session_id(),
		Req = cowboy_req:set_resp_cookie(<<"sessionid">>, SessionID, Req0,#
			{max_age => 0}).

	% To restrict cookies to a specific domain and path, the options of the
	% same name can be used:

		Req = cowboy_req:set_resp_cookie(<<"inaccount">>, <<"1">>, Req0, #
			{domain => "my.example.org", path => "/account"}).

	% Cookies will be sent with requests to this domain and all its
	% subdomains, and to resources on this path or deeper in the path
	% hierarchy.

	% To restrict cookies to secure channels (typically resources available
	% over HTTPS):

		SessionID = generate_session_id(),
		Req = cowboy_req:set_resp_cookie(<<"sessionid">>, SessionID, Req0, #
			{secure => true}).

	% To prevent client-side scripts from accessing a cookie:

		SessionID = generate_session_id(),
		Req = cowboy_req:set_resp_cookie(<<"sessionid">>, SessionID, Req0, #
			{http_only = true}).

	% Cookies may also be set client-side, for example using JavaScript.

% -------------------------------------------------------------------------

% Reading Cookiess
	
	% The client only ever sends back the cookie name and value. All other
	% options that canbe set are never sent back.

	% Cowboy provides two functions for reading cookies. Both involve parsing
	% the cookie header(s) and so should not be called repeatedly.

	% You can get all cookies as a key/value list:

		Cookies = cowboy_req:parse_cookies(Req),
		{_, Lang} = lists:keyfind(<<"lang">>, 1, Cookies).

	% Or you can perform a match against cookies and retrieve only the ones
	% you need, while at the same time doing any required post processing
	% using "constraints". This function returns a map:

		#{id := ID, lang := Lang} = cowboy_req:match_cookies([id, lang], Req).

	% You can use constraints to validate the values while matching them. The
	% following snippet will crash if the "id" cookie is not an integer
	% number or if the "lang" cookie is empty.

	% Additionally the "id" cookie value will be converted to an integer
	% term:

		CookiesMap = cowboy_req:match_cookies([{id, int}, {lang, nonempty}],
			Req).
	% Note that if two cookies share the same name, then the map value will
	% be a list of the two cookie values.

	% A default value can be provided. the default will be used if the "lang"
	% cookie is not found. It will not be used if the cookie is found but has
	% an empty value:

		#{lang := Lang} = cowboy_req:match_cookies([{lang, [], <<"en-US">>}],
			Req).

	% If no default is provided and the value is missing, an exception is
	% thrown.