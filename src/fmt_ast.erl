-module(fmt_ast).
-export([rep/2]).

rep({attribute, _L, export, [{_Fun, _A}]} = _Node, Nodes) ->
	Nodes;

rep({attribute, _L, import, {_Mod,[{_Fun, _A}]}} = _Node, Nodes) ->
	Nodes;

rep({attribute, _L, module, _Mod} = _Node, Nodes) ->
	Nodes;

rep({attribute, _L, file, {_File, _Line}} = _Node, Nodes) ->
	Nodes;

rep({attribute, _L, _Spec, {{_Name, _Arity}, Rep}} = Node, Nodes) ->
	rep(Rep, [Node | Nodes]);

rep({attribute, _L, _Type, {_Name, RepT, Rep}} = Node, Nodes) ->
	rep([RepT, Rep], [Node | Nodes]);

rep({attribute, _L, record, {_Name, Rep}} = Node, Nodes) ->
	rep(Rep, [Node | Nodes]);

rep({attribute, _L, _A, _T} = _Node, Nodes) -> Nodes;

rep({function,_L, _Name, _Arity, Rep} = Node, Nodes) -> 
	rep(Rep, [Node | Nodes]);

rep({record_field,_L, RepA} = Node, Nodes) ->
	rep(RepA, [Node | Nodes]);

rep({record_field, _L, RepA, RepE} = Node, Nodes) ->
	rep([RepA, RepE], [Node | Nodes]);

rep({typed_record_field, RepR, RepT} = Node, Nodes) ->
	rep([RepR, RepT], [Node | Nodes]);

rep({error, _E} = _Node, Nodes) -> Nodes;
rep({warning, _E} = _Node, Nodes) -> Nodes;
rep({eof, _E} = _Node, Nodes) -> Nodes;
rep({nil, _L} = _Node, Nodes) -> Nodes;
rep({atom, _L, _V} = _Node, Nodes) -> Nodes;
rep({char, _L, _V} = _Node, Nodes) -> Nodes;
rep({float,_L, _V} = _Node, Nodes) -> Nodes;
rep({integer, _L, _V} = _Node, Nodes) -> Nodes;
rep({var, _L, _V} = _Node, Nodes) -> Nodes;

rep({string, _L, _String} = Node, Nodes) -> Nodes;

rep({bin,_L, Rep} = Node, Nodes) ->
	rep(Rep, [Node | Nodes]);

rep({match, _L, Rep1, Rep2} = Node, Nodes) ->
	rep([Rep1, Rep2], [Node | Nodes]);

rep({cons, _L, RepP_h, RepP_t} = Node, Nodes) ->
	rep([RepP_h, RepP_t], [Node | Nodes]);

rep({map,_L, Rep} = Node, Nodes) ->
	rep(Rep, [Node | Nodes]);

rep({op, _L, _Op, RepP_1, RepP_2} = Node, Nodes) ->
	rep([RepP_1, RepP_2], [Node | Nodes]);

rep({op, _L, _Op, RepP_0} = Node, Nodes) ->
	rep(RepP_0, [Node | Nodes]);

rep({record_index, _L, _Name, Rep} = Node, Nodes) ->
	rep(Rep, [Node | Nodes]);

rep({record,_L, _Name, Rep} = Node, Nodes) -> 
	rep(Rep, [Node | Nodes]);

rep({tuple, _L, Rep} = Node, Nodes) ->
	rep(Rep, [Node | Nodes]);

rep({bc, _L, RepE_0, Rep} = Node, Nodes) ->
	rep([RepE_0, Rep], [Node | Nodes]);

rep({block, _L, Rep} = Node, Nodes) ->
	rep(Rep, [Node | Nodes]);

rep({'case',_L,RepE_0, Rep} = Node, Nodes) ->
	rep([RepE_0, Rep], [Node | Nodes]);

rep({'catch', _L, Rep} = Node, Nodes) ->
	rep(Rep, [Node | Nodes]);

rep({'fun',_L, {function, _Name, _Arity}} = _Node, Nodes) ->
	Nodes;

rep({'fun',_L, {function, _Module, _Name, _Arity}} = _Node, Nodes) ->
	Nodes;

rep({'fun',_L, {clauses, Rep}} = Node, Nodes) ->
	rep(Rep, [Node | Nodes]);

rep({named_fun, _L, _Name, Rep} = Node, Nodes) ->
	rep(Rep, [Node | Nodes]);

rep({call, _L, RepE_0, Rep} = Node, Nodes) ->
	rep([RepE_0, Rep], [Node | Nodes]);

rep({remote, _L, RepE_m, RepE_0} = Node, Nodes) ->
	rep([RepE_m, RepE_0], [Node | Nodes]);

rep({'if',_L, Rep} = Node, Nodes) -> 
	rep(Rep, [Node | Nodes]);

rep({lc,_L,RepE_0, Rep} = Node, Nodes) ->
	rep(RepE_0, [Node | Nodes]) ++ rep(Rep, [Node | Nodes]);

rep({'receive', _L, Rep} = Node, Nodes) ->
	rep(Rep, [Node | Nodes]);
rep({'receive', _L, Rep, RepE_0, RepB_t} = Node, Nodes) ->
	rep([Rep, RepE_0, RepB_t], [Node | Nodes]);

rep({record_field, _L, RepE_0, _Name, RepField} = Node, Nodes) ->
	rep([RepE_0, RepField], [Node | Nodes]);

rep({'try',_L, RepB, RepCc, RepTc, []} = Node, Nodes) ->
	rep([RepB, RepCc, RepTc], [Node | Nodes]);

rep({generate, _L, RepP, RepE} = Node, Nodes) ->
	rep([RepP, RepE], [Node | Nodes]);

rep({b_generate, _L, RepP, RepE} = Node, Nodes) ->
	rep([RepP, RepE], [Node | Nodes]);

rep({map_field_assoc, _L, RepK, RepV} = Node, Nodes) ->
	rep([RepK, RepV], [Node | Nodes]);

rep({map_field_exact, _L, RepK, RepV} = Node, Nodes) ->
	rep([RepK, RepV], [Node | Nodes]);

rep({clause, _L, P, Gs, B} = Node, Nodes) ->
	rep([P, Gs, B], [Node | Nodes]);

rep({ann_type, _L, Rep} = Node, Nodes) ->
	rep(Rep, [Node | Nodes]);

rep({type, _L, _Type, Rep} = Node, Nodes) ->
	rep(Rep, [Node | Nodes]);

rep([Rep|Rest], Nodes) ->
	rep(Rep, Nodes) ++ rep(Rest, Nodes);

rep([], Nodes) -> Nodes;

rep(_P, _Nodes) ->
	io:format("unmatched expression:~p~n", [_P]),
	erlang:error(unmatched_expression).