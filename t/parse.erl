-module(fmt_parse_test).

parse(File, _Opts) ->
	AST = ast(File),
	io:format("AST:~p~n", [AST]),
	fmt_ast:rep(AST, []),
	"ok".

