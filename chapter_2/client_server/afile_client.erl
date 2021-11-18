-module(afile_client).
-export([ls/1, get_file/2, put_file/3]).

ls(Server) ->
	Server ! { self(), list_dir },
	receive
		{ Server, FileList } ->
			FileList
	end.

get_file(Server, File) ->
	% ! is the send operator
	% Expr1 ! Expr2
	% it sends the value of Expr2 as a message to the process specified by
	% Expr1. The value of Expr2 is also the return value of the expression.

	% Expr1 must evaluate to a pid, an alias (reference), a port, a
	% registered name (atom), or a tuple {Name, Node}
	% Name is an atom and Node is a node name, also an atom.

	% - if Expr1 evaluates to a name, but this name is not registered, a
	% 	"badarg" run-time error occurs.

	% - Sending a message to a reference never fails, even if the reference
	% 	is no longer (or never was) an alias.

	% - Sending a message to a pid never fails, even if the pid identifies a
	% 	non-existing process.

	% - Distributed message sending, that is, if Expr1 evaluates to a tuple 
	% 	{Name, Node} (or a pid located at another node), also never fails.

	Server ! { self(), {get_file, File} },
	% send this to the server {Client, {get_file, File}}

	% server responds with content from
	% Client ! {self(), file:read_file(Full)}; in afile_server.erl
	% and sends it back here to the client
	receive
		{ Server, Content } ->
			Content
	end.

% put_file(Server, File) ->
% 	Full = filename:join("..", File),
% 	{ ok, Bytes } = file:read_file(Full),
% 	Server ! { self(), {put_file, File, Bytes} },
% 	receive
% 		{Server, Content} -> Content
% 	end.

put_file(Server, File, NewFileName) ->
	case file:read_file(File) of
		% content of read_file cannot be sent directly
		% file:read_file returns a tuple, and the Content binary
		% information needs to be extracted

		{ ok, Content } ->
			Server ! { self(), { put_file, Content, NewFileName } },
			% 
			receive
				{Server, Result} -> Result
			end;

		{ error, Reason } -> Reason
	end.