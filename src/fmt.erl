-module(fmt).
-mode(compile).
-export([main/1]).

main(Args) ->
	case Args of
		[Action, File] ->
			Str = fmt_tool:process(erlang:list_to_atom(Action), File),
			io:format("~s", [Str]);
		_ ->
			io:format("Usage: ./fmt tick|untick file.erl~n", [])
	end.
