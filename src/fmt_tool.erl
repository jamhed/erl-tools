-module(fmt_tool).
-export([process/2]).
-export([tokens/1, tokens/2, sentences/1, ast/1, analyze/1, make_st/1]).

process(tick, File) ->
	analyze(make_st(File));
process(untick, File) ->
	Data = read(File),
	Tokens = tokenize(Data),
	lists:flatten([ untick(Token) || Token <- Tokens ]).

make_st(File) ->
	Tokens = tokens(File),
	Sentences = [ [] | sentences(Tokens) ] ++ [ [] ],
	AST = ast(File),
	lists:zip(Sentences, AST).

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

ast(File) ->
	{ok, AST} = fmt_epp:parse_file(File, []),
	AST.

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

tokenize(Data) ->
	tokenize(Data, [text, return]).

tokenize(Data, Opts) ->
	{ok, Tokens, _Line} = erl_scan:string(Data, 1, Opts),
	Tokens.

reassemble(Tokens, TargetAtoms) ->
	{_, TokenTexts} = lists:foldl(
		fun(Token, {TA, Acc}) ->
			{NewTA, Text} = tick(TA, Token),
			{NewTA, [Text | Acc]}
		end, 
		{TargetAtoms, []}, Tokens
	),
	lists:flatten(lists:reverse(TokenTexts)).

tick(Target, {_Item, [{text, Text}, {location, _Line}]}) ->
	{Target, Text};
tick([{keep,Value}|Rest], {atom, [{text, _Text}, {location, _Line}], Value}) ->
	{Rest, io_lib:format("'~p'", [Value])};
tick([{skip,Value}|Rest], {atom, [{text, Text}, {location, _Line}], Value}) ->
	{Rest, Text};
tick(Target, {_Item, [{text, Text}, {location, _Line}], _Value}) ->
	{Target, Text}.

untick({_Item, [{text, Text}, {location, _Line}]}) -> Text;
untick({atom, [{text, _Text}, {location, _Line}], Value}) ->
	io_lib:format("~p", [Value]);
untick({_Item, [{text, Text}, {location, _Line}], _Value}) ->
	Text.

analyze([]) -> [];
analyze([{Tokens, Form} | Rest]) ->
	TargetAtoms = rep([], Form),
	%% io:format("~p~n~p~n~n", [TargetAtoms, Form]),
	reassemble(Tokens, TargetAtoms) ++ analyze(Rest).

rep(Path, {function, _L, Name, _Arity, Rep}) ->
	[{skip,Name}] ++ rep([Name | Path], Rep);
rep(Path, {attribute, _L, _Attr, {{Name, _}, Rep}}) ->
	[{skip, Name}] ++ rep(Path, Rep);
rep(Path, {type, _L, _Type, Rep}) ->
	rep(Path, Rep);
rep(Path, {clause, _L, Rep1, Rep2, Rep3}) ->
	rep(Path, [Rep1, Rep2, Rep3]);
rep(Path, {'case', _L, Rep1, Rep2}) ->
	rep(Path, [Rep1, Rep2]);
rep(Path, {'try', _L, Rep1, Rep2, Rep3, Rep4}) ->
	rep(Path, [Rep1, Rep2, Rep3, Rep4]);
rep(Path, {tuple, _L, Rep}) ->
	rep(Path, Rep);
rep(Path, {call, _, {atom, _, F}, Rep}) ->
	[{skip, F}] ++ rep(Path, Rep);
rep(Path, {call, _, Rep1, Rep2}) -> 
	rep(Path, [Rep1, Rep2]);
rep(_Path, {remote, _, {atom,_,M}, {atom,_,F}}) ->
	[{skip, M}, {skip,F}];
rep(Path, {remote, _, Rep, {atom,_,F}}) ->
	rep(Path, Rep) ++ [{skip,F}];
rep(_Path, {atom, _L, Atom}) ->
	[{keep, Atom}];
rep(Path, {cons, _L, Rep1, Rep2}) ->
	rep(Path, [Rep1, Rep2]);
rep(Path, {match, _L, Rep1, Rep2}) ->
	rep(Path, [Rep1, Rep2]);
rep(Path, [Rep|Rest]) ->
	lists:append(rep(Path, Rep), rep(Path, Rest));
rep(_Path, _P) -> [].

