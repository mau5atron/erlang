% 18.1 Creating a Digital Clock

	% The following image shows the clock running in a browser. All the 
	% irrelevant details of the browser window, such as the menus, toolbars, 
	% and scrollbars, are not shown so that we can concentrate on the code.


	% The essential part of this application is the display. This contains a 
	% time, which is updated every second. From the Erlang point of view, 
	% the entire browser is a process; so, to update the clock on the value
	% shown earlier, Erlang sent the browser the following message:

		% Browser ! #{ cmd => fill_div, id => clock, txt => <<"16:30:52">>}

	% Inside the browser, we have loaded an HTML page with a small fragment
	% of HTML like this:

		% <div id="clock">
			% ....
		% </div>

	% When the browser receives a fill_div, it converts this into the
	% JavaScript command fill_div({cmd:'fill_div', id:'clock',
	% txt:'16:30:52'}), which then fills the content of the div with the
	% required string.

	% Note how the Erlang message containing a frame gets converted to an
	% equivalent JavaScript function call, which is evaluated in the browser.
	% Extending the system is extremely easy. All you have to do is write a
	% small JavaScript function corresponding to the Erlang message that you
	% need to process.

	% To complete the picture, we need to add the code that starts and stops
	% the clock. Putting everything together, the HTML code looks like this:

		% Look at websockets/clock1.html
		% go to the right folder for section 18.1
		% lol dont use jquery

	% First, we load two JavaScript libraries (don't do this) and a style
	% sheet. clock1.css
	% is used to style the display of the clock.

	% Second, there is some HTML that creates the display. Finally, we have a
	% small fragment of JavaScript that is run when the page is loaded.

	% NOTE: In all our examples we assume some familiarity with jQuery.
	% jQuery is an extremely popular JavaScript library that simplifies
	% manipulating objects in the browser.

	% websock.js has all the code necessary for opening a websocket and
	% connecting the objects in the browser DOM to Erlang. It does the
	% following:

		% 1.

			% Adds click handlers to all buttons with class live_button in the
			% page. The click handlers send messages to Erlang when the buttons
			% are clicked.

		% 2.

			% Tries to start a websocket connection to http://localhost:2233. On
			% the server side, the function clock1:start(Browser) will be called
			% in a freshly spawned process. All this is achieved by calling the
			% JavaScript function connect("localhost", 2233, "clock1") (lol dont
			% use jQuery). The number 2233 has no particular significance; any
			% unused port number over 1023 would do.

	% Now to the Erlang code;

		% Look at clock1.erl
		
	% The Erlang code begins execution in start(Browser); Browser is a
	% process representing the browser. This is the first interesting line of
	% code:

		% Browser ! #{ cmd => fill_div, id => clock, txt => current_time() }

	% This updates the display. I've repeated this line for emphasis. My
	% editor told me to remove it. But no. To me this is very beautiful code.
	% To get the browser to do something, we send it a message. Just like
	% Erlang. We've tamed the browser. It looks like an Erlang processes.
	% Whoopee.

	% After the initializing, clock1 calls running/1. If a {clicked =>
	% <<"stop">>} message is received, then we call idle(Browser). Otherwise,
	% after a timeout of one second, we send a command to the browser telling
	% it to update the clock and call ourselves.

		% idle/1 waits for a start message and then calls running/1