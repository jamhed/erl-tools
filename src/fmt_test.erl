-module(fmt_test).
-export([test/0, kz_extra/5]).
-define(MMM, test).

tokenize(X) -> X.

test() ->
	{ erlang:error("text"), tokenize(wtf), atom1, [atom3, atom4, atom5] },
	test = test(),
	{ok, _} = tokenize(wtf),
	atomX,
	case test() of
		test -> test();
		_ -> tokenize(test);
		{ok, X} -> tokenize(X)
	end,
	try
		test()
	catch
		'try':test -> ?MMM
	end.

-spec kz_extra(term(), term(), term(), term(), term()) -> ok.
kz_extra(Module, Node, Id, Conf, Data) ->
	(kz_util:to_atom(Module, true)):handle_config_req(Node, Id, Conf, Data).