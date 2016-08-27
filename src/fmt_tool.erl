-module(fmt_tool).
-export([tokens/1, tokens/2, process/1, sentences/1, ast/1, test/0, analyze/1, make_st/1]).

% erlang source code processor

process(File) ->
	analyze(make_st(File)).

make_st(File) ->
	Data = read(File),
	Tokens = tokenize(Data),
	Sentences = [ [] | sentences(Tokens) ] ++ [[]],
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
	{ok, AST} = epp:parse_file(File, []),
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

tokenize(Data) -> tokenize(Data, [text, return]).

tokenize(Data, Opts) ->
	{ok, Tokens, _Line} = erl_scan:string(Data, 1, Opts),
	Tokens.

reassemble(Tokens, TargetAtoms) ->
	{[], TokenTexts} = lists:foldl(
		fun(Token, {TA, Acc}) ->
			{NewTA, Text} = reassemble_token(TA, Token),
			{NewTA, [Text | Acc]}
		end, 
		{TargetAtoms, []}, Tokens
	),
	lists:flatten(lists:reverse(TokenTexts)).

reassemble_token(Target, {_Item, [{text, Text}, {location, _Line}]}) ->
	{Target, Text};
reassemble_token([Value|Rest], {atom, [{text, _Text}, {location, _Line}], Value}) ->
	{Rest, io_lib:format("'~p'", [Value])};
reassemble_token(Target, {_Item, [{text, Text}, {location, _Line}], _Value}) ->
	{Target, Text}.

analyze([]) -> [];
analyze([{Tokens, Form} | Rest]) ->
	TargetAtoms = rep([], Form),
	% io:format("~p~n~p~n~p~n~n", [TokenAtoms, TargetAtoms, Form]),
	reassemble(Tokens, TargetAtoms) ++ analyze(Rest).

rep(Path, {function, _L, Name, _Arity, Rep}) ->
	rep([Name | Path], Rep);
rep(Path, {clause, _L, Rep1, Rep2, Rep3}) ->
	rep(Path, [Rep1, Rep2, Rep3]);
rep(Path, {tuple, _L, Rep}) ->
	rep(Path, Rep);
rep(Path, {call, _, {remote,_,{atom,_,M},{atom,_,F}}, Rep}) -> 
	rep([M,F | Path], Rep);
rep(Path, {call, _, {atom, _, F}, Rep}) ->
	rep([F | Path], Rep);
rep(_Path, {atom, _L, Atom}) ->
	[Atom];
rep(Path, {cons, _L, Rep1, Rep2}) ->
	rep(Path, [Rep1, Rep2]);
rep(Path, {match, _L, Rep1, Rep2}) ->
	rep(Path, [Rep1, Rep2]);
rep(Path, [Rep|Rest]) ->
	lists:append(rep(Path, Rep), rep(Path, Rest));
rep(_Path, _P) -> [].

test() ->
	{ erlang:error("text"), tokenize(wtf), atom1, [atom3, atom4, atom5] },
	test = test(),
	{ok, _} = tokenize(wtf),
	atomX.
