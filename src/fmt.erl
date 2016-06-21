-module(fmt).
-mode(compile).
-export([main/1]).

main(Args) ->
	case Args of
		[File] ->
			Str = fmt_tool:process(File),
			io:format("~s", [Str]);
		_ ->
			io:format("Usage: ./fmt file.erl~n", [])
	end.