-module(fmt_tool).
-export([quote/2, unquote/2, parse/2]).
-export([tokens/1, tokens/2, sentences/1, ast/1, analyze/2, make_st/1]).

is_verbose(Opts) -> proplists:get_value(verbose, Opts).
verbose(true, Fmt, Args) -> io:format(Fmt, Args);
verbose(_, _Fmt, _Args) -> skip.

quote(File, Opts) ->
	analyze(make_st(File), is_verbose(Opts)).

unquote(File, _Opts) ->
	Data = read(File),
	Tokens = tokenize(Data),
	lists:flatten([ untick(Token) || Token <- Tokens ]).

parse(File, _Opts) ->
	AST = ast(File),
	fmt_ast:rep(AST, []).

make_st(File) ->
	Tokens = tokens(File),
	Sentences = [ [] | sentences(Tokens) ],
	AST = ast(File),
	case (_L1 = length(AST)) == (_L2 = length(Sentences)) of
		true ->
			lists:zip(Sentences, AST);
		false ->
			lists:zip(Sentences ++ [[]], AST)
	end.

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
	{Rest, io_lib:format("'~s'", [Value])};
tick([{skip,Value}|Rest], {atom, [{text, Text}, {location, _Line}], Value}) ->
	{Rest, Text};
tick(Target, {_Item, [{text, Text}, {location, _Line}], _Value}) ->
	{Target, Text}.

untick({_Item, [{text, Text}, {location, _Line}]}) -> Text;
untick({atom, [{text, _Text}, {location, _Line}], Value}) ->
	io_lib:format("~p", [Value]);
untick({_Item, [{text, Text}, {location, _Line}], _Value}) ->
	Text.

analyze([], _) -> [];
analyze([{Tokens, Form} | Rest], Verbose) ->
	TargetAtoms = rep(Form),
	verbose(Verbose, "~p~n~p~n~n", [TargetAtoms, Form]),
	reassemble(Tokens, TargetAtoms) ++ analyze(Rest, Verbose).

rep({function, L, Name, Arity, Rep}) ->
	rep(lists:map(fun(R) -> [{fmt_function_clause, L, Name, Arity, R}] end, Rep));
rep({fmt_function_clause, _L, Name, _Arity, Rep}) ->
	[{skip, Name}] ++ rep(Rep);
rep({attribute, _L, spec, {{Name, _}, Rep}}) ->
	[{skip, Name}] ++ rep(Rep);
rep({attribute, _L, type, {Name, Rep1, Rep2}}) ->
	[{skip, Name}] ++ rep([Rep1, Rep2]);
rep({attribute, _L, record, {Name, Rep}}) ->
	[{skip, Name}] ++ rep(Rep);
rep({'fun', _, {clauses, Rep}}) ->
	rep(Rep);
rep({type, _L, record, [{atom, _, Name}]}) ->
	[{skip, Name}];
rep({type, _L, _Type, Rep}) ->
	rep(Rep);
rep({record, _, Name, Rep}) ->
	[{skip, Name}] ++ rep(Rep);
rep({record, _, _Var, Name, Rep}) ->
	[{skip, Name}] ++ rep(Rep);
rep({record_field, _, {atom, _, Name}, Rep}) ->
	[{skip, Name}] ++ rep(Rep);
rep({typed_record_field, Field, _Rep}) ->
	rep(Field);
rep({clause, _L, [{tuple, _, [{atom, _, throw}, Rep1, Rep2]}], Rep3, Rep4}) ->
	rep([Rep1, Rep2, Rep3, Rep4]);
rep({clause, _L, Rep1, Rep2, Rep3}) ->
	rep([Rep1, Rep2, Rep3]);
rep({'case', _L, Rep1, Rep2}) ->
	rep([Rep1, Rep2]);
rep({'try', _L, Rep1, Rep2, Rep3, Rep4}) ->
	rep([Rep1, Rep2, Rep3, Rep4]);
rep({'catch', _, Rep}) ->
	rep(Rep);
rep({op, _L, _Op, Rep1, Rep2}) ->
	rep([Rep1, Rep2]);
rep({op, _L, _Op, Rep}) ->
	rep(Rep);
rep({bin, _, Rep}) ->
	rep(Rep);
rep({bin_element, _, Rep, _, _}) ->
	rep(Rep);
rep({tuple, _L, Rep}) ->
	rep(Rep);
rep({call, _, {atom, _, F}, Rep}) ->
	[{skip, F}] ++ rep(Rep);
rep({call, _, Rep1, Rep2}) -> 
	rep([Rep1, Rep2]);
rep({remote, _, {atom,_,M}, {atom,_,F}}) ->
	[{skip, M}, {skip,F}];
rep({remote, _, Rep, {atom,_,F}}) ->
	rep(Rep) ++ [{skip,F}];
rep({atom, _L, Atom}) ->
	[{keep, Atom}];
rep({cons, _L, Rep1, Rep2}) ->
	rep([Rep1, Rep2]);
rep({match, _L, Rep1, Rep2}) ->
	rep([Rep1, Rep2]);
rep({'receive', _, Rep1, Rep2, Rep3}) ->
	rep([Rep1, Rep2, Rep3]);
rep({lc, _, Rep1, Rep2}) ->
	rep([Rep1, Rep2]);
rep({generate, _, Rep1, Rep2}) ->
	rep([Rep1, Rep2]);
rep([Rep|Rest]) ->
	rep(Rep) ++ rep(Rest);
rep(_P) -> [].
