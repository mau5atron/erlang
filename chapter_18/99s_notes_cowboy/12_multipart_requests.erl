% Multipart Requests

	% Multipart originates from MIME, an Internet standard that extends the
	% format of emails.

	% A multipart message is a list of parts. A part contains headers and a
	% body. The body of the parts may be of any media type, and contain text
	% or binary data. It is possible for parts to contain a multipart media
	% type.

	% In the context of HTTP, multipart is most often used with the
	% "multipart/form-data" type. It is what browsers use to upload files
	% through HTML forms.

	% The "multipart/byteranges" is also common. It is the media type used to
	% send arbitrary bytes from a resource, enabling clients to resume
	% downloads.

% -------------------------------------------------------------------------

% Form-Data

	% In the normal use case, when a form is submitted, the browser will use
	% the "application/x-www-form-urlencoded" content-type. This type is just
	% a list of keys and values and is therefore not fit for uploading files.

	% That's where the "multipart/form-data" content-type comes in. When the
	% form is configured to use this content-type, the browser will create a
	% multipart message where each part corresponds to a fiel on the form.
	% For files, it also adds some metadata in the part headers, like the
	% file name.

	% A form with a text input, a file input and a select choice box will
	% result in a multipart message with three parts, one for each field.

	% The browser does its best to determine the media type of the files it
	% sends this way, but you should not rely on it for determining the
	% contents of the file. Proper investigation of the contents is
	% recommended.

% -------------------------------------------------------------------------

% Checking for Multipart Messages

	% The content-type header indicates the presence of a multipart message:

		{<<"multipart">>, <<"form-data">>, _} = cowboy_req:parse_header
		(<<"content-type">>, Req).

% -------------------------------------------------------------------------

% Reading a Multipart Message

	% Cowboy provides two sets of functions for reading request bodies as
	% multipart messages.

	% The "cowboy_req:read_part/1,2" functions return the next part's
	% headers, if any.

	% The "cowboy_req:read_part_body/1,2" functions return the current part's
	% body. For large bodies you may need to call the function multiple
	% times.

	% To reada multipart message you need to iterate over all its parts:

		multipart(Req0) ->
			case cowboy_req:read_part(Req0) of
				{ok, _Headers, Req1} ->
					{ok, _Body, Req} = cowboy_req:read_part_body(Req1),
					multipart(Req);
				{done, Req} ->
					Req
			end.

	% When part bodies are too large, Cowboy will return a "more" tuple, and
	% allow you to loop until the part body has been fully read.

	% The function "cowboy_multipart:form_data/1" can be used to quickly
	% obtain information about a part from a "multipart/form-data" message.
	% The function returns a "data" or a "file" tuple depending on whether
	% this is a normal field or a file being uploaded.

	% The following snippet will use this function and use different
	% strategies depending whether part is a file:

		multipart(Req0) ->
			case cowboy_req:read_part(Req0) of
				{ok, Headers, Req1} ->
					Req = case cow_multipart:form_data(Headers) of
						{data, _Filename} ->
							{ok, _Body, Req2} = cowboy_req:read_part_body(Req1),
							Req2;
						{file, _Fieldname, _Filename, _CType} ->
							stream_file(Req1)
					end,
					multipart(Req);

				{done, Req} ->
					Req
			end.

		stream_file(Req0) ->
			case cowboy_req:read_part_body(Req0) of
				{ok, _LastBodyChunk, Req} ->
					Req;

				{more, _BodyChunk, Req} ->
					stream_file(Req)
			end.

	% Both the part header and body reading functions can take options that
	% will be given to the request body reading functions. By default,
	% "cowboy_req:read_part/1" reads up to 64KB for up to 5 seconds.
	% "cowboy_req:read_part_body/1" has the same defaults as
	% "cowboy_req:read_body/1"

	% To change the defaults for part headers:

		cowboy_req:read_part(Req, #{length => 128000}).

	% And for part bodies:

		cowboy_req:read_part_body(Req, #{length => 1000000, period => 7000}).

% -------------------------------------------------------------------------

% Skipping Unwanted Parts

	% Part bodies do not have to be read. Cowboy will automatically skip it
	% when you request the next part's body.

	% The following snippet reads all part headers and skips all bodies:

		multipart(Req0) ->
			case cowboy_req:read_part(Req0) of
				{ok, _Headers, Req} ->
					multipart(Req);
				{done, Req} ->
					Req
			end.

	% Similarly, if you start reading the body and it ends up being too big,
	% you can simply continue with the next part. Cowboy will automatically
	% skip what remains.

	% While Cowboy can skip part bodies automatically, the read rate is not
	% configurable. Depending on your application you may want to skip
	% manually, in particular if you observe poor performance while skipping.

	% You do not have to read all parts either. You can stop reading as soon
	% as you find the data you need.

