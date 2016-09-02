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
		{ok, {Opts, [Action, File]}} ->
			Txt = fmt_tool:process(erlang:list_to_atom(Action), File, Opts),
			process_results(proplists:get_value(inplace, Opts), File, Txt);
		_ ->
			io:format("Usage: ./fmt [-iv] tick|untick file.erl~n", [])
	end.

process_results(true=_Inplace, File, Text) ->
	file:write_file(File, Text);
process_results(_Inplace, _File, Text) ->
	io:format("~s", [Text]).
