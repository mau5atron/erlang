% 17.0 Browsing with Websockets and Erlang

	% In this chapter, we will see how to build applications in the browser 
	% and extend the idea of using message passing to outside Erlang. This 
	% way, we can easily build distributed applications and integrate them 
	% with a web browser.

	% We're going to pretend thgat a web browser is an Erlang process. If we
	% want the browser to do something, we'll send it a message; if something
	% happens within the browser that we need to attend to, the browser will
	% send us a message. All of this is possible thanks to websockets.

	% Websockets are part of the HTML5 standard and are bidirectional
	% asynchronous sockets that can be used to pass messages between a
	% browser and an external program. In our case, the external program is
	% the Erlang runtime system.

	% To interface the Erlang runtime system to websockets, we run a simple
	% Erlang web server, called cowboy, to manage the socket and the
	% websocket protocol. Details of how to install cowboy are covered in
	% Chapter 25, Third-Party Programs, on page 425. To simplify things, we
	% assume that all messages between Erlang and the Browser are JSON
	% messages.

	% On the Erlang side of the application, these messages appear as Erlang
	% maps (see Section 5.3, Maps: Associative Key-Value stores, on page 79),
	% and in the browser these messages appear as JavaScript objects.

	% In The rest of this chapter, we'll look at six example programs,
	% including the code that runs in the browser and the code that runs in
	% the server. Finally, we'll look at the client-server protocol and see
	% how messages from Erlang to the browser are processed.

	% To run these examples, we need three things: some code that runs in the
	% browser, some code that runs in an Erlang server, and an Erlang server
	% that understands the websockets protocol. We're not going to look at
	% all the code here; we'll look at the code that runs in the browser and
	% in the server but not the code for the server itself. All the examples
	% can be found at github.com/joearms/ezwebframe. The browser code in the
	% examples has been tested only in the Chrome browser


		% NOTE: The code shown here is a simplified version of the code in the
		% ezwebframe repo. The code in the repo is kept in sync with the Erlang
		% distribution and will reflect any changes to Erlang when maps are
		% introduced in version R17 of Erlang (expected in late 2013, but maps
		% will appear in branches on github before the official release).

	% To run the code yourself, you'll need to download the code and follow
	% the installation interactions. As far as we are concerned, the
	% interesting parts of the code are the part that runs in the browser and
	% the part the runs in the server.

	% All the examples use a simple technique for controlling the browser
	% from Erlang. If Erlang wants the browser to do something, it just sends
	% the browser a message telling it what to do. If the user wants to do
	% something they click a button or som other control in the browser and a
	% message is sent to Erlang. The first example shows in detail how this
	% works.