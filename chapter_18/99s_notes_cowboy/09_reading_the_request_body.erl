% Reading the Request Body

	% The request body can be read using the Req object

	% Cowboy will not attempt to read the body until requested. You need to
	% call the body reading functions in order to retrieve it.

	% Cowboy will not cache the body, it is therefore only possible to read
	% it once.

	% You are not required to read it, however, if a body is present and was
	% not read, Cowboy will either cancel or skip its download, depending on
	% the protocol.

	% Cowboy provides functions for reading the body raw, and read and parse
	% form urlencoded or multipart bodies. The latter is covered in its own
	% chapter.

% -------------------------------------------------------------------------

% Request Body Presence

	% Not all requests come with a body. You can check for the presence of a
	% request body with this function:

		cowboy_req:has_body(Req).

	% It returns "true" if there is a body; "false" otherwise.

	% In practice, this function is rarely used. When the method is "POST",
	% "PUT" or "PATCH", the request body is often required by the
	% application, which should just attempt to read it directly.

% -------------------------------------------------------------------------

% Request Body Length

	% You can obtain the length of the body

		Length = cowboy_req:body_length(Req).

	% Note that the length may not be known in advance. In that case
	% "undefined" will be returned. This can happen with HTTP/1.1's chunked
	% transfer-encoding, or HTTP/2 when no content-length was provided.

	% Cowboy will update the body length in the Req object once the body has
	% been read completely. A length will always be returned when attempting
	% to call this function after reading the body completely.

% -------------------------------------------------------------------------

% Reading the Body

	% You can read the entire body with one function call:

		{ok, Data, Req} = cowboy_req:read_body(Req0).

	% Cowboy returns an "ok" tuple when the body has been read fully.

	% By default, Cowboy will attempt to read up to 8MB of data, for up to 15
	% seconds. The call will return once Cowboy has read at least 8MB of
	% data, or at the end of the 15 seconds period.

	% These values can be customized. For example, to read up to 1MB for up
	% to 5 seconds:

		{ok, Data, Req} = cowboy_req:read_body(Req0, #{length => 1000000,
			period => 5000}).

	% You may also disable the length limit

		{ok, Data, Req} = cowboy_req:read_body(Req0, #{length => infinity}).

	% This makes the function wait 15 seconds and return with whatever
	% arrived during that period. This is not recommended for public facing
	% applications.

	% These two options can effectively be used to control the rate of
	% transmission of the request body.

% -------------------------------------------------------------------------

% Streaming the Body

	% When the body is too large, the first call will return a "more" tuple
	% instead of "ok". You can call the function again to read more of the
	% body, reading it one chunk at a time.

		read_body_to_console(Req0) ->
			case cowboy_req:read_body(Req0) of
				{ok, Data, Req} ->
					io:format("~s", [Data]),
					Req;
				{more, Data, Req} ->
					io:format("~s", [Data]),
					read_body_to_console(Req)
			end.

	% The "length" and "period" options can also be used. They need to be
	% passed for every call.

% -------------------------------------------------------------------------

% Reading a form urlencoded Body

	% Cowboy provides a convenient function for reading and parsing bodies
	% sent as application/x-www-form-urlencoded

		{ok, KeyValues, Req} = cowboy_req:read_urlencoded_body(Req0).

	% This function returnrs a list of key/values, exactly like the function
	% "cowboy_req:parse_qs/1".

	% The defaults for this function are different. Cowboy will read for up
	% to 64KB and up to 5 seconds. They can be modified:

		{ok, KeyValues, Req} = cowboy_req:read_urlencoded_body(Req0, #{length
			=> 4096, period => 3000}).