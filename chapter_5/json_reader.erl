-module(json_reader).
-export([read_dir/1, read_file/2]).
% -import(binary).

read_dir(Dir) ->
	file:list_dir(Dir).
	% returns a list of files like: 
	% {ok,["example.json","json_reader.beam","json_reader.erl",
     % "records.hrl","records_and_maps.erl","zsh_history.txt"]}

read_file(Dir, FileName) ->
	FilePath = filename:join(Dir, FileName),
	Json = file:read_file(FilePath),
	{_, JsonBin} = Json,
	% binary:replace(JsonBin, <<$:>>, <<"Y">>).
	loop_through_binary(JsonBin).
	% JsonBitSize = erlang:bit_size(JsonBin),
	% JsonList = binary:bin_to_list(JsonBin, 0, JsonBitSize),
	% JsonList.

loop_through_binary(RawJSON) ->
	[$" | JSONtail ] = RawJSON,
	io:format("Next ~p", JSONtail).
	% case Next of
	% 	$: -> throw("unexpected token");
	% 	$\" -> throw("a backslash\n");
	% 	${ -> throw("a curly bracket");
	% 	$" -> throw("a quotation")
	% end.


% use this as reference for creating a parser
% https://robhirschfeld.com/2011/08/01/erlang-a-mini-recursive-json-parser/

% -export([json/1]).
% -record(json, {list=[], raw=[]}).
% -record(jsonkv, {value=[], raw=[]}).
% % handles values that are quoted (this one ends the quote)
% json_value_quoted(Value, [$" | T]) ->
%   #jsonkv{value=Value, raw=T};

% json_value_quoted(Value, [Next | T]) ->
%   json_value_quoted(Value ++ [Next], T).

% % returns JSON Key Values with remaining JSON
% json_value(Value, RawJSON) ->
%   [Next | T] = RawJSON, 
%   case Next of
%     $: -> throw('unexpected token');
%     ${ -> J = json(RawJSON),                                  % recurse to get list
%             #jsonkv{value=J#json.list, raw=J#json.raw};  
%     $, -> #jsonkv{value=string:strip(Value), raw=RawJSON};    % terminator, return
%     $} -> #jsonkv{value=string:strip(Value), raw=RawJSON};    % terminator, return
%     $" -> json_value_quoted(Value, T);                        % run to next quote,exit
%     _ -> json_value(Value ++ [Next], T)                       % recurse
%   end.
% % parses the Key Value pairs (KVPs) based on , & } delimiters
% json(JSON, Key) ->
%   [Next | T] = JSON#json.raw,
%   case {Next, T} of
%     {$", _} -> json(JSON#json{raw=T}, Key);        % ignore
%     {${, _} -> json(#json{raw=T}, []);             % start new hash
%     {$,, _} -> json(JSON#json{raw=T}, []);         % add new value
%     {$:, _} -> KV = json_value([], T),  % get value for key
%             List = lists:merge(JSON#json.list, [{string:strip(Key), KV#jsonkv.value}]),
%             json(#json{list=List, raw=KV#jsonkv.raw}, []);  % add new KVP
%     {$}, []} -> JSON#json.list;                    %DONE!
%     {$}, _} -> JSON#json{raw=T};                   %List parse, but more remains!
%     {_, _} -> json(JSON#json{raw=T}, Key ++ [Next])  % add to key
%   end.
% % entry point
% json(RawJSON) ->
%   json(#json{raw=RawJSON}, []).
