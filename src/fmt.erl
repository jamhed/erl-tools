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
			[ handle_action(erlang:list_to_atom(Action), File, Opts) || File <- Files];
		_ ->
			io:format("Usage: ./fmt [-iv] tick|untick file.erl~n", [])
	end.

handle_action(Action, File, Opts) ->
	Inplace = proplists:get_value(inplace, Opts),
	results(Inplace, File, action(Action, File, Opts)).

action(tick, File, Opts) ->
	fmt_tool:quote(File, Opts);
action(untick, File, Opts) ->
	fmt_tool:unquote(File, Opts);
action(parse, File, Opts) ->
	fmt_tool:parse(File, Opts).

results(true=_Inplace, File, Text) ->
	file:write_file(File, Text);
results(_Inplace, _File, Text) ->
	io:format("~s", [Text]).
