% Rest Handlers

	% REST is implemented in Cowboy as a sub protocol. The request is handled
	% as a state machine with many optional callbacks describing the resource
	% and modifying the machine's behavior.

	% The REST handler is the recommended way to handle HTTP requests.

% -------------------------------------------------------------------------

% Initialization
	
	% First, the "init/2" callback is called. This callback is common to all
	% handlers. To use the REST for the current request, this function must
	% return a "cowboy_rest" tuple.

		init(Req, State) ->
			{cowboy_rest, Req, State}.

	% Cowboy will then switch to the REST protocol and start executing the
	% state machine.

	% After reaching the end of the flowchart, the "terminate/3" callback
	% will be called if it is defined.

% -------------------------------------------------------------------------

% Methods

	% The REST component has code for handling the following HTTP methods:
	% HEAD, GET, POST, PATCH, PUT, DELETE and OPTIONS

	% Other method scan be accepted, however, they have no specific callback
	% defined for them at this time.

% -------------------------------------------------------------------------

% Callbacks

	% All callbacks are optional. Some may become mandatory depending on what
	% other defined callbacks you need.

	% All callbacks take two arguments, the Req object and the State, and
	% return a three-element tuple of the form "{Value, Req, State}"

	% Nearly all callbacks can also return "{stop, Req, State}" to stop
	% execution of the request, and "{{switch_handler, Module}, Req, State}"
	% or "{{switch_handler, Module, Opts}, Req, State}" to switch to a
	% different handler type. The exceptions are "expires", "generate_etag",
	% "last_modified" and "variances".

	% The following table summarizes the callbacks and their default values.
	% If the callback isn't defined, then the default value will be used.
	% Please look at the flowcharts to find out the result of each return
	% value.

	% In the following table, "skip" means the callback is entirely skipped
	% if it is undefined, moving directly to the next step. Similarly

		% Look at callback table: https://ninenines.eu/docs/en/cowboy/2.9/guide/rest_handlers/

		% Callback name: Default value

			% allowed_methods: [<<"GET">>, <<"HEAD">>, <<"OPTIONS">>]

			% allow_missing_post: true

			% charsets_provided: skip

			% content_types_accepted: none

			% content_types_provided: {{ <<"text">>, <<"html">>, '*'}, to_html}]

			% delete_completed: true

			% delete_resource: false

			% expires: undefined

			% forbidden: false

			% generate_etag: undefined

			% is_authorized: true

			% is_conflict: false

			% known_methods: [<<"GET">>, <<"HEAD">>, <<"POST">>, <<"PUT">>, 
			% <<"PATCH">>, <<"DELETE">>, <<"OPTIONS">>]

			% languages_provided: skip

			% last_modified: undefined

			% malformed_request: false

			% moved_permanently: false

			% moved_temporarily: false

			% multiple_choices: false

			% options: ok

			% previously_existed: false

			% rate_limited: false

			% resource_exists: true

			% service_unavailable: true

			% uri_too_long: false

			% valid_content_headers: true

			% valid_entity_length: true

			% variances: []

	% As you can see, Cowboy tries to move one with the request whenever
	% possible by using well thought out default values.

	% In addition to these, there can be any number of user-defined callbacks
	% that are specified through "content_types_accepted/2" and
	% "content_types_provided/2". They can take any name, however, it is
	% recommended to use a separate prefix for the callbacks of each
	% function. For example, "from_html" and "to_html" indicate in the first
	% case that we're accepting a resource given as HTML, and in the second
	% case tha we send one as HTML.

% -------------------------------------------------------------------------

% Meta Data

	% Cowboy will set informative values to the Req object at various pionts
	% of execution. You can retrieve them by matching the Req object
	% directly. The values are defined in the following table:

		% Key: Details

			% media_type: The content-type negotiated for the response entity.

			% language: The language negotiated for the response entity

			% charset: The charset negotiated for the response entity

	% They can be used to send a proper body with the response to a request
	% that used a method other than HEAD or GET.

% -------------------------------------------------------------------------

% Response Headers

	% Cowboy will set response headers automatically over the execution of
	% the REST code. They are listed in the following table.

		% Header name: Details

			% content-language: Language used in the response body

			% content-type: Media type and charset of the response body'

			% etag: Etag of the resource

			% expires: Expiration date of the resource

			% last-modified: Last modification date for the resource

			% location: Relative or absolute URI to the requested resource

			% vary: List of headers that may change the represenation of the
			% resource.

