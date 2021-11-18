-module(myfile).
-export([read/1, read1/1]).
-define(CurrentDir, ".").
% to add color, don't need to include, just compile color.erl

% Exercise 1
read(File) ->
	FilePath = filename:join(?CurrentDir, File),
	case file:read_file(FilePath) of
		{ok, Bin} 	 ->
			io:format("Able to read the file~n"),
			Bin;
		{error, Why} ->
			io:format("Not able to read the file~n"),
			error(Why)
	end.

% Exercise 2
read1(File) ->
	FilePath = filename:join(?CurrentDir, File),
	case file:read_file(FilePath) of
		{ok, Bin} 	 ->
			io:format("Able to read the file~n"), Bin;
		{error, Why} ->
			% io:format("File: ~p", [File]),
			FileNameNotFound = io:fwrite(
				"\nWas not able to find the file {~s}.~n\n",[color:red(File)]
			),
			try error(FileNameNotFound, Why)
			catch
				error:X:Stk -> {X, Stk}
			end
	end.

