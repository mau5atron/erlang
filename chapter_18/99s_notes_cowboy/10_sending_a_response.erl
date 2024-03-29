% Sending a Response

	% The response must be sent using the Req object.

	% Cowboy provides two different ways of sending responses: 
	
		% Either directly
		% or by streaming the body

	% Response headers and body may be set in advance. The response is sent
	% as soon as one of the reply or stream reply function is called.

	% Cowboy also provides a simplified interface for sending files. It can
	% also send only specific parts of a file.

	% While only one response is allowed for every request, HTTP/2 introduced
	% a mechanism that allows the server to push additional resources related
	% to the response. This chapter also describes how this feature works in
	% Cowboy.

% -------------------------------------------------------------------------

% Reply
	
	% Cowboy provides three functions for sending the entire reply, depending
	% on whether you need to set headers and body. In all cases, Cowboy will
	% add any headers required by the protocol (for example the date header
	% will always be sent).

	% When you need to set only the status code, use "cowboy_req:reply/2"

		Req = cowboy_req:reply(200, Req0).

	% When you need to set response headers at the same time, use
	% "cowboy_req:reply/3"

		Req = cowboy_req:reply(303, #{
			<<"location">> => <<"https://ninenines.eu">>
		}, Req0).

	% Note that the header name must always be a lowercase binary.

	% When you also need to set the response body, use "cowboy_req:reply/4":

		Req = cowboy_req:reply(200, #{
			<<"content-type">> => <<"text/plain">>
		}, "Hello world!", Req0).

	% You should always set the content-type header when the response has a
	% body. There is however no need to set the content-length header, Cowboy
	% does it automatically.

	% The response body and the header values must be either a binary or an
	% iolist. An iolist is a list containing binaries, characters, strings or
	% other iolists. This allows you to build a response form different parts
	% without having to do any concatenation.

		Title = "Hello world!",
		Body = <<"Hats off!">>,
		Req = cowboy_req:reply(200, #{
			<<"content-type">> => <<"text/html">>
		}, ["<html><head><title>", Title, "</title></head>",
				"<body><p>", Body, "</p></body></html>"], Req0).

	% This method of building responses is more efficient that concatenating.
	% Behind the scenes, each element of the list is simply a pointer, and
	% those pointers are used directly when writing to the socket.

% -------------------------------------------------------------------------

% Stream Reply

	% Cowboy provides two functions for intiating a response, and an
	% additional function for streaming the response body. Cowboy will add
	% any required headers to the response.

	% When you need to set only the status code, use
	% cowboy_req:stream_reply/2

		Req = cowboy_req:stream_reply(200, Req0),
		cowboy_req:stream_body("Hello...", nofin, Req),
		cowboy_req:stream_body("chunked...", nofin, Req),
		cowboy_req:stream_body("world!!", fin, Req).

	% The second argument to "cowboy_req:stream_body/3" indicates whether
	% this data terminates the body. Use "fin" for the final flag, and
	% "nofin" otherwise.

	% This snippet does not set a content-type header. This is not
	% recommended. All responses with a body should have a content-type. The
	% header can be set beforehand, or using the "cowboy_req:stream_reply/3".

		Req = cowboy_req:stream_reply(200, #{
			<<"content-type">> => <<"text/html">>
		}, Req0),

		cowboy_req:stream_body("<html><head>Hello world!</head>", nofin, Req),
		cowboy_req:stream_body("<body><p>Hats off!</p></body></html>", fin,
			Req).

	% HTTP provides a few different ways to stream response bodies. Cowboy
	% will select the most appropriate one based on the HTTP version and the
	% request and response headers.

	% While not required by any means, it is recommended that you set the
	% content-length header in the response if you know it in advance. This
	% will ensure that The best response method is selected and help clients
	% understand when the response is fully received.

	% Cowboy also provides a function to send response trailers. Response
	% trailers are semantically equivalent to the headers you send in the
	% response, only they are sent at the end. This is especially useful to
	% attach information to the response that could not be generated until
	% the response body was fully generated.

	% Trailer fields must be listed in the trailer header. Any field not
	% listed might be dropped by the client or an intermediary.

		Req = cowboy_req:stream_reply(200, #{
			<<"content-type">> => <<"text/html">>,
			<<"trailer">> => <<"expires, content-md5">>
		}, Req0),

		cowboy_req:stream_body("<html><head>Hello world!</head>", nofin, Req),
		cowboy_req:stream_body("<body><p>Hats off!</p></body></html>", nofin,
			Req),

		cowboy_req:stream_trailers(#{
			<<"expires">> => <<"Sun, 10 Dec 2017 19:13:47 GMT">>,
			<<"content-md5">> => <<"c6081d20ff41a42ce17048ed1c0345e2">>
		}, Req).

	% The stream ends with trailers. It is no longer possible to send data
	% after sending trailers. You cannot send trailers after setting the
	% "fin" flag when streaming the body.

% -------------------------------------------------------------------------

% Preset Response Headers

	% Cowboy provides functions to set response headers without immediately
	% sending them. They are stored in the Req object and sent as part of the
	% response when a reply function is called.

	% To set response headers:

		Req = cowboy_req:set_resp_header(<<"allow">>, "GET", Req0).

	% Header names must be a lowercase binary.

	% Do not use this function for setting cookies. Refer to the "Cookies"
	% chapter for more information.

	% To check if a response header has already been set

		cowboy_req:has_resp_header(<<"allow">>, Req).

	% It returns "true" if the header was set, "false" otherwise

	% To delete a response header that was set previously:

		Req = cowboy_req:delete_resp_header(<<"allow">>, Req0).

% -------------------------------------------------------------------------

% Overriding Headers

	% As Cowboy provides different ways of setting response headers and body,
	% clashes may occur, so it's important to understand what happens when a
	% header is set twice.

	% Headers come from five different origins:

		% Protocol-specific headers (for example HTTP/1.1's connection header)
		% Other required headers (for example the date header)
		% Preset headers
		% Headers given to the reply function
		% Set-cookie headers

	% Cowboy does not allow overriding protocol-specific headers.

	% Set-cookie headers will always be appended at the end of the list of
	% headers before sending the response.

	% Headers given to the reply function will always override preset headers
	% and required headers. If a header is found in two or three of these,
	% then the one in the reply function is picked and the others are
	% dropped.

	% Similarly, preset headers will always override required headers.

	% To illustrate, look at the following snippet. Cowboy by default sends
	% the server header with the value "Cowboy". We can override it.

		Req = cowboy_req:reply(200, #{
			<<"server">> => <<"yaws">>
		}, Req0).

% -------------------------------------------------------------------------

% Preset Response Body

	% Cowboy provides functions to set the response body without immediately
	% sending it. It is stored in the Req object and setn when the reply
	% function is called.

	% To set the response body:

		Req = cowboy_req:set_resp_body("Hello world!", Req0).

	% To check if a response body has already been set:

		cowboy_req:has_resp_body(Req).

	% It returns "true" if the body was set and is non-empty, "false"
	% otherwise.

	% The preset response body is only sent if the reply function used is
	% "cowboy_req:reply/2" or "cowboy_req:reply/3"

% -------------------------------------------------------------------------

% Sending Files

	% Cowboy provides a shortcut for sending files. When using
	% "cowboy_req:reply/4", or when presetting the response header, you can
	% give a "sendfile" tuple to Cowboy:

		{sendfile, Offset, Length, Filename}

	% Depending on the values for "Offset" or "Length", the entire file may
	% be sent, or just a part of it.

	% The length is required even for sending the entire file. Cowboy sends
	% it in the content-length header.

	% To send a file while replying:

		Req = cowboy_req:reply(200, #{
			<<"content-type">> => "image/png"
		}, {sendfile, 0, 12345, "path/to/logo.png"}, Req0).

% -------------------------------------------------------------------------

% Information Responses

	% Cowboy allows you to send information responses.

	% Informational responses are responses that have a status code between
	% 100 and 199. Any number can be sent before the proper response. Sending
	% an informational response does not change the behavior of the proper
	% response, and clients are expected to ignore any informational response
	% they do not understand.

	% The following snippet sends a 103 informational response with some
	% headers that are expected to be in the final response.

		Req = cowboy_req:inform(103, #{
			<<"link">> => <<"</style.css; rel=preload; as=style, </script.js>;
			rel=preload; as=script">>
		}, Req0).

% -------------------------------------------------------------------------

% Push

	% The HTTP/2 protocol introduced the ability to push resources related to
	% the one sent in the response. Cowboy provides two functions for that
	% purpose: "cowboy_req:push/3,4"

	% Push is only available for HTTP/2. Cowboy will automatically ignore
	% push requests if the protocol doesn't support it.

	% The push function must be called before any of the reply functions.
	% Doing otherwise will result in a crash.

	% To push a resource, you need to provide the same information as a
	% client performing a request would. This includes the HTTP method, the
	% URI and any necessary request headers.
	
	% Cowboy by default only requires you to give the path to the resource
	% and the request headers. The rest of the URI is taken from the current
	% request (excluding the query string, set to empty) and the method is
	% GET by default.

	% The following snippet pushes a CSS file that is linked to in the
	% response:

		cowboy_req:push("/static/styles.css", #{
			<<"accept">> => <<"text/css">>
		}, Req0),

		Req = cowboy_req:reply(200, #{
			<<"content-type">> => <<"text/html">>
		}, ["<html><head><title>My web page</title>",
    "<link rel='stylesheet' type='text/css' href='/static/style.css'>",
    "<body><p>Welcome to Erlang!</p></body></html>"], Req0).

  % To override the method, scheme, host, port, or query string, simply
  % pass in a fourth argument. The following snippet uses a different host
  % name:

   	cowboy_req:push("/static/style.css", #{
   		<<"accept">> => <<"text/css">>
   	}, #{host => <<"cdn.example.org">>}, Req).

  % Pushed resources don't have to be files. As long as the push request is
  % cacheable, safe and does not include a body, the resource can be
  % pushed.

  % Under the hood, Cowboy handles pushed requests the same as normal
  % requests: a different process is created which will ultimately send a
  % response to the client.

  