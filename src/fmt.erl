-module(fmt).
-mode(compile).
-export([main/1]).

opts() ->
	[
		{inplace, $i, "inplace", {boolean, false}, "Edit file inplace"},
		{verbose, $v, "verbose", {boolean, false}, "Dump parse tree"}
	].

main(Args) ->
	case getopt:parse(opts(), Args) of
		{ok, {Opts, [Action|Files]}} when is_list(Files) ->
			[ file(Action, File, Opts) || File <- Files];
		_ ->
			io:format("Usage: ./fmt [-iv] tick|untick file.erl~n", [])
	end.

file(Action, File, Opts) ->
	Txt = fmt_tool:process(erlang:list_to_atom(Action), File, Opts),
	results(proplists:get_value(inplace, Opts), File, Txt).

results(true=_Inplace, File, Text) ->
	file:write_file(File, Text);
results(_Inplace, _File, Text) ->
	io:format("~s", [Text]).
