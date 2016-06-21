-module(fmt_tool).
-export([read/1, tokenize/1, process/1]).

% erlang source code processor

process(File) ->
	reassemble(tokenize(read(File))).

read(IO, Acc) ->
	Str = fun([BinStr]) -> erlang:binary_to_list(BinStr) end,
	case file:read(IO, 4096) of
		{ok, Data} -> read(IO, [ Data | Acc ]);
		eof -> Str(lists:flatten(lists:reverse(Acc)));
		{error, Error} -> exit(Error)
	end.

read(File) ->
	case file:open(File, [read, raw, binary]) of
		{ok, Dev} -> read(Dev, []);
		{error, Error} -> exit(Error)
	end.

tokenize(Data) ->
	{ok, Tokens, _Line} = erl_scan:string(Data, 1, [text, return]),
	Tokens.

reassemble(Tokens) when is_list(Tokens) ->
	lists:flatten([ reassemble(Token) || Token <- Tokens ]);

reassemble({_Item, [{text, Text}, {location, _Line}]}) ->
	Text;
reassemble({atom, [{text, _Text}, {location, _Line}], Value}) ->
	erlang:atom_to_list(Value);
reassemble({_Item, [{text, Text}, {location, _Line}], _Value}) ->
	Text.