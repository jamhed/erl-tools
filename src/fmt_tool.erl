-module(fmt_tool).
-export([tokens/1, tokens/2, process/1, sentences/1, ast/1]).

% erlang source code processor

process(File) ->
	Data = read(File),
	Tokens = tokenize(Data),
	reassemble(Tokens).

tokens(File) ->
	Data = read(File),
	tokenize(Data).

tokens(File, Opts) ->
	Data = read(File),
	tokenize(Data, Opts).

sentence(Tokens) -> sentence(Tokens, []).

sentence([], Acc) -> {lists:reverse(Acc), []};
sentence([Token = {dot, _} | Rest], Acc) -> {lists:reverse([Token | Acc]), Rest};
sentence([Token | Tokens], Acc) -> sentence(Tokens, [ Token | Acc ]).

sentences(Tokens) -> sentences(Tokens, []).

sentences([], Acc) -> lists:reverse(Acc);
sentences(Tokens, Acc) ->
	{Sentence, Rest} = sentence(Tokens),
	sentences(Rest, [Sentence | Acc]).

ast(Sentences) ->
	[ ast_1(S) || S <- Sentences ]. 
ast_1(S) ->
	try erl_parse:parse_form(S) of
		{ok, Ast} -> {S, Ast}
	catch _C:E ->
		{S, E}
	end.

read(IO, Acc) ->
	case file:read(IO, 96) of
		{ok, Data} -> read(IO, [ Data | Acc ]);
		eof ->
			file:close(IO),
			Bin = erlang:list_to_binary(lists:reverse(Acc)),
			erlang:binary_to_list(Bin);
		{error, Error} -> exit(Error)
	end.

read(File) ->
	case file:open(File, [read, raw, binary]) of
		{ok, Dev} -> read(Dev, []);
		{error, Error} -> exit(Error)
	end.

tokenize(Data) -> tokenize(Data, [text, return]).

tokenize(Data, Opts) ->
	{ok, Tokens, _Line} = erl_scan:string(Data, 1, Opts),
	Tokens.

reassemble(Tokens) when is_list(Tokens) ->
	lists:flatten([ reassemble(Token) || Token <- Tokens ]);

reassemble({_Item, [{text, Text}, {location, _Line}]}) ->
	Text;
reassemble({atom, [{text, _Text}, {location, _Line}], Value}) ->
	io_lib:format("~p", [Value]);
reassemble({_Item, [{text, Text}, {location, _Line}], _Value}) ->
	Text.
