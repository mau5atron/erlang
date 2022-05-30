% 18.7 The Browser Server Protocol

	% The Browser server protocol is extremely simple. It makes use of JSON
	% messages sent over a websocket. This fits nicely with both Erlang and
	% JavaScript and makes interfacing with the browser to Erlang really
	% easy.

% -------------------------------------------------------------------------

% Sending a Message from Erlang to the Browser

	% To change something in the browser, we send a message form Erlang to
	% the browser. Suppose we have a div in the browser, declared like this:

		<div id="id123"></div>

	% To change the content of the div to the tring abc, Erlang sends the
	% following JSON message to the websocket that is connected to the
	% Browser.

		[{cmd: 'fill_div', id: 'id123', txt: 'abc'}]

	% The code in webserver.js, which is activated when the browser receives
	% a message from a websocket, is a callback routine called onMessage. It
	% is set up with the following code.

		websocket = new Websocket(wsUri);
		% ...

		websocket.onmessage = onMessage;

	% The callback is defined like this:

		function onMessage(evt) {
			var json = JSON.parse(evt.data);
			do_cmds(json);
		}

		function do_cmds(objs){
			for ( var i = 0 ; i < objs.length ; i++ ) {
				if ( eval("typeof("+o.cmd+")") == "function") {
					eval(o.cmd + "(o)");
				} else {
					alert("bad_command: "+o.cmd);
				}
			}
		}

	% do_cmds(objs) expects to receive a list of commands of the following
	% form:

		% [{cmd:command1, ...:..., ...:...},
			% {cmd:command2, ...:..., ...:...},
			% ...
			% {cmd:commandN, ...:..., ...:...}]

	% Each command in the list is a JavaScript object, which must contain a
	% key called cmd. For each object x in the list, the system checks
	% whether there is a function called x.cmd, and if there is, it calls
	% x.cmd(x). So, {cmd:'fill_div', id: 'id123', txt: 'abc'} causes the
	% following:

		fill_div({cmd:'fill_div', id: 'id123', txt: 'abc'})

	% to be called. This method of encoding and evaluating commands is easily
	% extensible so we can add more commands to the interface as necessary.

% -------------------------------------------------------------------------

% Messages from the Browser to Erlang

	% In the browser when we clicked a button, we evaluate commands like
	% this:

		send_json({'clicked': txt});

	% send_json(x) encodes the argument x and a JSON term and writes it to
	% the websocket. This message is received in websocket.erl where it is
	% converted to a frame and sent to the controlling process that manages
	% the websocket.

	% We have seen how to extend the notion of message passing to outside
	% Erlang and use message passing to directly control a browser. From the
	% point of view of an Erlang programmer, the world now consists of a
	% well-ordered space where everything responds to Erlang messages. We
	% don't have one way of doing things inside Erlang and another way of
	% doing things outside Erlang. This adds a sense of order and unity to
	% our programs, making a complex world appear simple. a web browser is,
	% of course, a massively complicated object. But by making it respond in
	% a predictable manner to a small number of messages, we can easily
	% contain this complexity and harness it to build powerful applications.


