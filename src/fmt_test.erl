-module(fmt_test).
-export([test/0, kz_extra/5, moar/1]).
-define(MMM, test).

-record(state1, {field = true :: boolean(), another }).

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

-type fs_app() :: {binary(), binary() | noop}.
-spec kz_extra(term(), term(), term(), term(), fs_app()) -> ok.
kz_extra(Module, Node, Id, Conf, Data) ->
	case Node of
		'$already_ticked' when Module =:= test -> test();
		false -> put('$prior_terminators', Id)
	end,
	Media = <<$', (ecallmgr_util:media_path(kz_json:get_value(<<"Media-Name">>, Node), new))/binary, $'>>,
	(kz_util:to_atom(Media, true)):handle_config_req(Node, Id, Conf, Data),
	tokenize([fun() ->
		case Id of
			test -> test()
		end
	end
	]),
	tokenize(#state1{field=test}),
	(catch gproc:unreg({p, l, {call_control, Id}})),
	tokenize(Data#state1{field=test}),
	receive
		keep_alive_expired -> ok
		after 0 -> ok
	end,
	[gen_listener:cast(Srv, {update_node, Node}) || Srv <- test].

-type state() :: #state1{}.
-spec moar(state()) -> state().
moar(S) -> 
	S#state1{
		field=false,
		another=true
	}.
