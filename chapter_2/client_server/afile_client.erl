-module(afile_client).
-export([ls/1, get_file/2, put_file/3]).

ls(Server) ->
	Server ! { self(), list_dir },
	receive
		{ Server, FileList } ->
			FileList
	end.

get_file(Server, File) ->
	Server ! { self(), {get_file, File} },
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