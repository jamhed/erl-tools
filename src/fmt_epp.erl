%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%

-module(fmt_epp).

%% An Erlang code preprocessor.

-export([open/1, open/2,open/3,open/5,close/1,format_error/1]).
-export([scan_erl_form/1,parse_erl_form/1,macro_defs/1]).
-export([parse_file/1, parse_file/2, parse_file/3]).
-export([default_encoding/0, encoding_to_string/1,
		 read_encoding_from_binary/1, read_encoding_from_binary/2,
		 set_encoding/1, set_encoding/2, read_encoding/1, read_encoding/2]).
-export([interpret_file_attribute/1]).
-export([normalize_typed_record_fields/1,restore_typed_record_fields/1]).

%%------------------------------------------------------------------------

-export_type([source_encoding/0]).

-type macros() :: [atom() | {atom(), term()}].
-type epp_handle() :: pid().
-type source_encoding() :: latin1 | utf8.

-type ifdef() :: 'ifdef' | 'ifndef' | 'else'.

-type name() :: atom().
-type argspec() :: 'none'                       %No arguments
				 | non_neg_integer().           %Number of arguments
-type argnames() :: [atom()].
-type tokens() :: [erl_scan:token()].
-type predef() :: 'undefined' | {'none', tokens()}.
-type userdef() :: {argspec(), {argnames(), tokens()}}.
-type used() :: {name(), argspec()}.

-type function_name_type() :: 'undefined'
				| {atom(),non_neg_integer()}
				| tokens().

-type warning_info() :: {erl_anno:location(), module(), term()}.

-define(DEFAULT_ENCODING, utf8).

%% Epp state record.
-record(epp, {file :: file:io_device()
					| 'undefined',              %Current file
		  location=1,         		%Current location
			  delta=0 :: non_neg_integer(),     %Offset from Location (-file)
			  name="" :: file:name(),           %Current file name
			  name2="" :: file:name(),          %-"-, modified by -file
			  istk=[] :: [ifdef()],             %Ifdef stack
			  sstk=[] :: [#epp{}],              %State stack
			  path=[] :: [file:name()],         %Include-path
			  macs = #{}		        %Macros (don't care locations)
				:: #{name() => predef() | [userdef()]},
			  uses = #{}			%Macro use structure
				:: #{name() => [{argspec(), [used()]}]},
			  default_encoding = ?DEFAULT_ENCODING :: source_encoding(),
		  pre_opened = false :: boolean(),
		  fname = [] :: function_name_type()
		 }).

%% open(Options)
%% open(FileName, IncludePath)
%% open(FileName, IncludePath, PreDefMacros)
%% open(FileName, IoDevice, StartLocation, IncludePath, PreDefMacros)
%% close(Epp)
%% scan_erl_form(Epp)
%% parse_erl_form(Epp)
%% parse_file(Epp)
%% parse_file(FileName, Options)
%% parse_file(FileName, IncludePath, PreDefMacros)
%% macro_defs(Epp)

-spec open(FileName, IncludePath) ->
	{'ok', Epp} | {'error', ErrorDescriptor} when
	  FileName :: file:name(),
	  IncludePath :: [DirectoryName :: file:name()],
	  Epp :: epp_handle(),
	  ErrorDescriptor :: term().

open(Name, Path) ->
	open(Name, Path, []).

-spec open(FileName, IncludePath, PredefMacros) ->
	{'ok', Epp} | {'error', ErrorDescriptor} when
	  FileName :: file:name(),
	  IncludePath :: [DirectoryName :: file:name()],
	  PredefMacros :: macros(),
	  Epp :: epp_handle(),
	  ErrorDescriptor :: term().

open(Name, Path, Pdm) ->
	internal_open([{name, Name}, {includes, Path}, {macros, Pdm}], #epp{}).

open(Name, File, StartLocation, Path, Pdm) ->
	internal_open([{name, Name}, {includes, Path}, {macros, Pdm}],
		  #epp{file=File, pre_opened=true, location=StartLocation}).

-spec open(Options) ->
		  {'ok', Epp} | {'ok', Epp, Extra} | {'error', ErrorDescriptor} when
	  Options :: [{'default_encoding', DefEncoding :: source_encoding()} |
		  {'includes', IncludePath :: [DirectoryName :: file:name()]} |
		  {'macros', PredefMacros :: macros()} |
		  {'name',FileName :: file:name()} |
		  'extra'],
	  Epp :: epp_handle(),
	  Extra :: [{'encoding', source_encoding() | 'none'}],
	  ErrorDescriptor :: term().

open(Options) ->
	internal_open(Options, #epp{}).

internal_open(Options, St) ->
	case proplists:get_value(name, Options) of
		undefined ->
			erlang:error(badarg);
		Name ->
			Self = self(),
			Epp = spawn(fun() -> server(Self, Name, Options, St) end),
			case epp_request(Epp) of
				{ok, Pid, Encoding} ->
					case proplists:get_bool(extra, Options) of
						true -> {ok, Pid, [{encoding, Encoding}]};
						false -> {ok, Pid}
					end;
				Other ->
					Other
			end
	end.

-spec close(Epp) -> 'ok' when
	  Epp :: epp_handle().

close(Epp) ->
	%% Make sure that close is synchronous as a courtesy to test
	%% cases that test for resource leaks.
	Ref = erlang:monitor(process, Epp),
	R = epp_request(Epp, close),
	receive {'DOWN',Ref,_,_,_} -> ok end,
	R.

scan_erl_form(Epp) ->
	epp_request(Epp, scan_erl_form).

-spec parse_erl_form(Epp) ->
	{'ok', AbsForm} | {error, ErrorInfo} |
	{'warning',WarningInfo} | {'eof',Line} when
	  Epp :: epp_handle(),
	  AbsForm :: erl_parse:abstract_form(),
	  Line :: erl_anno:line(),
	  ErrorInfo :: erl_scan:error_info() | erl_parse:error_info(),
	  WarningInfo :: warning_info().

parse_erl_form(Epp) ->
	case epp_request(Epp, scan_erl_form) of
	{ok,Toks} ->
		erl_parse:parse_form(Toks);
	Other ->
		Other
	end.

macro_defs(Epp) ->
	epp_request(Epp, macro_defs).

%% format_error(ErrorDescriptor) -> String
%%  Return a string describing the error.

-spec format_error(ErrorDescriptor) -> io_lib:chars() when
	  ErrorDescriptor :: term().

format_error(cannot_parse) ->
	io_lib:format("cannot parse file, giving up", []);
format_error({bad,W}) ->
	io_lib:format("badly formed '~s'", [W]);
format_error(missing_parenthesis) ->
	io_lib:format("badly formed define: missing closing right parenthesis",[]);
format_error(premature_end) ->
	"premature end";
format_error({call,What}) ->
	io_lib:format("illegal macro call '~s'",[What]);
format_error({undefined,M,none}) ->
	io_lib:format("undefined macro '~s'", [M]);
format_error({undefined,M,A}) ->
	io_lib:format("undefined macro '~s/~p'", [M,A]);
format_error({depth,What}) ->
	io_lib:format("~s too deep",[What]);
format_error({mismatch,M}) ->
	io_lib:format("argument mismatch for macro '~s'", [M]);
format_error({arg_error,M}) ->
	io_lib:format("badly formed argument for macro '~s'", [M]);
format_error({redefine,M}) ->
	io_lib:format("redefining macro '~s'", [M]);
format_error({redefine_predef,M}) ->
	io_lib:format("redefining predefined macro '~s'", [M]);
format_error({circular,M,none}) ->
	io_lib:format("circular macro '~s'", [M]);
format_error({circular,M,A}) ->
	io_lib:format("circular macro '~s/~p'", [M,A]);
format_error({include,W,F}) ->
	io_lib:format("can't find include ~s \"~s\"", [W,F]);
format_error({illegal,How,What}) ->
	io_lib:format("~s '-~s'", [How,What]);
format_error({illegal_function,Macro}) ->
	io_lib:format("?~s can only be used within a function", [Macro]);
format_error({illegal_function_usage,Macro}) ->
	io_lib:format("?~s must not begin a form", [Macro]);
format_error({'NYI',What}) ->
	io_lib:format("not yet implemented '~s'", [What]);
format_error({error,Term}) ->
	io_lib:format("-error(~p).", [Term]);
format_error({warning,Term}) ->
	io_lib:format("-warning(~p).", [Term]);
format_error(E) -> file:format_error(E).

-spec parse_file(FileName, IncludePath, PredefMacros) ->
				{'ok', [Form]} | {error, OpenError} when
	  FileName :: file:name(),
	  IncludePath :: [DirectoryName :: file:name()],
	  Form :: erl_parse:abstract_form() | {'error', ErrorInfo} | {'eof',Line},
	  PredefMacros :: macros(),
	  Line :: erl_anno:line(),
	  ErrorInfo :: erl_scan:error_info() | erl_parse:error_info(),
	  OpenError :: file:posix() | badarg | system_limit.

parse_file(Ifile, Path, Predefs) ->
	parse_file(Ifile, [{includes, Path}, {macros, Predefs}]).

-spec parse_file(FileName, Options) ->
		{'ok', [Form]} | {'ok', [Form], Extra} | {error, OpenError} when
	  FileName :: file:name(),
	  Options :: [{'includes', IncludePath :: [DirectoryName :: file:name()]} |
		  {'macros', PredefMacros :: macros()} |
		  {'default_encoding', DefEncoding :: source_encoding()} |
		  'extra'],
	  Form :: erl_parse:abstract_form() | {'error', ErrorInfo} | {'eof',Line},
	  Line :: erl_anno:line(),
	  ErrorInfo :: erl_scan:error_info() | erl_parse:error_info(),
	  Extra :: [{'encoding', source_encoding() | 'none'}],
	  OpenError :: file:posix() | badarg | system_limit.

parse_file(Ifile, Options) ->
	case internal_open([{name, Ifile} | Options], #epp{}) of
	{ok,Epp} ->
		Forms = parse_file(Epp),
		close(Epp),
		{ok,Forms};
	{ok,Epp,Extra} ->
		Forms = parse_file(Epp),
		close(Epp),
		{ok,Forms,Extra};
	{error,E} ->
		{error,E}
	end.

-spec parse_file(Epp) -> [Form] when
	  Epp :: epp_handle(),
	  Form :: erl_parse:abstract_form() | {'error', ErrorInfo} |
		  {'warning',WarningInfo} | {'eof',Line},
	  Line :: erl_anno:line(),
	  ErrorInfo :: erl_scan:error_info() | erl_parse:error_info(),
	  WarningInfo :: warning_info().

parse_file(Epp) ->
	case parse_erl_form(Epp) of
	{ok,Form} ->
			[Form|parse_file(Epp)];
	{error,E} ->
		[{error,E}|parse_file(Epp)];
	{warning,W} ->
		[{warning,W}|parse_file(Epp)];
	{eof,Location} ->
		[{eof,erl_anno:new(Location)}]
	end.

-spec default_encoding() -> source_encoding().

default_encoding() ->
	?DEFAULT_ENCODING.

-spec encoding_to_string(Encoding) -> string() when
	  Encoding :: source_encoding().

encoding_to_string(latin1) -> "coding: latin-1";
encoding_to_string(utf8) -> "coding: utf-8".

-spec read_encoding(FileName) -> source_encoding() | none when
	  FileName :: file:name().

read_encoding(Name) ->
	read_encoding(Name, []).

-spec read_encoding(FileName, Options) -> source_encoding() | none when
	  FileName :: file:name(),
	  Options :: [Option],
	  Option :: {in_comment_only, boolean()}.

read_encoding(Name, Options) ->
	InComment = proplists:get_value(in_comment_only, Options, true),
	case file:open(Name, [read]) of
		{ok,File} ->
			try read_encoding_from_file(File, InComment)
			after ok = file:close(File)
			end;
		_Error ->
			none
	end.

-spec set_encoding(File) -> source_encoding() | none when
	  File :: io:device(). % pid(); raw files don't work

set_encoding(File) ->
	set_encoding(File, ?DEFAULT_ENCODING).

-spec set_encoding(File, Default) -> source_encoding() | none when
	  Default :: source_encoding(),
	  File :: io:device(). % pid(); raw files don't work

set_encoding(File, Default) ->
	Encoding = read_encoding_from_file(File, true),
	Enc = case Encoding of
			  none -> Default;
			  Encoding -> Encoding
		  end,
	ok = io:setopts(File, [{encoding, Enc}]),
	Encoding.

-spec read_encoding_from_binary(Binary) -> source_encoding() | none when
	  Binary :: binary().

-define(ENC_CHUNK, 32).
-define(N_ENC_CHUNK, 16). % a total of 512 bytes

read_encoding_from_binary(Binary) ->
	read_encoding_from_binary(Binary, []).

-spec read_encoding_from_binary(Binary, Options) ->
									   source_encoding() | none when
	  Binary :: binary(),
	  Options :: [Option],
	  Option :: {in_comment_only, boolean()}.

read_encoding_from_binary(Binary, Options) ->
	InComment = proplists:get_value(in_comment_only, Options, true),
	try
		com_nl(Binary, fake_reader(0), 0, InComment)
	catch
		throw:no ->
			none
	end.

fake_reader(N) ->
	fun() when N =:= ?N_ENC_CHUNK ->
			throw(no);
	   () ->
			{<<>>, fake_reader(N+1)}
	end.

-spec read_encoding_from_file(File, InComment) -> source_encoding() | none when
	  File :: io:device(),
	  InComment :: boolean().

read_encoding_from_file(File, InComment) ->
	{ok, Pos0} = file:position(File, cur),
	Opts = io:getopts(File),
	Encoding0 = lists:keyfind(encoding, 1, Opts),
	Binary0 = lists:keyfind(binary, 1, Opts),
	ok = io:setopts(File, [binary, {encoding, latin1}]),
	try
		{B, Fun} = (reader(File, 0))(),
		com_nl(B, Fun, 0, InComment)
	catch
		throw:no ->
			none
	after
		{ok, Pos0} = file:position(File, Pos0),
		ok = io:setopts(File, [Binary0, Encoding0])
	end.

reader(Fd, N) ->
	fun() when N =:= ?N_ENC_CHUNK ->
			throw(no);
	   () ->
			case file:read(Fd, ?ENC_CHUNK) of
				eof ->
					{<<>>, reader(Fd, N+1)};
				{ok, Bin} ->
					{Bin, reader(Fd, N+1)};
				{error, _} ->
					throw(no) % ignore errors
			end
	end.

com_nl(_, _, 2, _) ->
	throw(no);
com_nl(B, Fun, N, false=Com) ->
	com_c(B, Fun, N, Com);
com_nl(B, Fun, N, true=Com) ->
	com(B, Fun, N, Com).

com(<<"\n",B/binary>>, Fun, N, Com) ->
	com_nl(B, Fun, N+1, Com);
com(<<"%", B/binary>>, Fun, N, Com) ->
	com_c(B, Fun, N, Com);
com(<<_:1/unit:8,B/binary>>, Fun, N, Com) ->
	com(B, Fun, N, Com);
com(<<>>, Fun, N, Com) ->
	{B, Fun1} = Fun(),
	com(B, Fun1, N, Com).

com_c(<<"c",B/binary>>, Fun, N, Com) ->
	com_oding(B, Fun, N, Com);
com_c(<<"\n",B/binary>>, Fun, N, Com) ->
	com_nl(B, Fun, N+1, Com);
com_c(<<_:1/unit:8,B/binary>>, Fun, N, Com) ->
	com_c(B, Fun, N, Com);
com_c(<<>>, Fun, N, Com) ->
	{B, Fun1} = Fun(),
	com_c(B, Fun1, N, Com).

com_oding(<<"oding",B/binary>>, Fun, N, Com) ->
	com_sep(B, Fun, N, Com);
com_oding(B, Fun, N, Com) when byte_size(B) >= length("oding") ->
	com_c(B, Fun, N, Com);
com_oding(B, Fun, N, Com) ->
	{B1, Fun1} = Fun(),
	com_oding(list_to_binary([B, B1]), Fun1, N, Com).

com_sep(<<":",B/binary>>, Fun, N, Com) ->
	com_space(B, Fun, N, Com);
com_sep(<<"=",B/binary>>, Fun, N, Com) ->
	com_space(B, Fun, N, Com);
com_sep(<<"\s",B/binary>>, Fun, N, Com) ->
	com_sep(B, Fun, N, Com);
com_sep(<<>>, Fun, N, Com) ->
	{B, Fun1} = Fun(),
	com_sep(B, Fun1, N, Com);
com_sep(B, Fun, N, Com) ->
	com_c(B, Fun, N, Com).

com_space(<<"\s",B/binary>>, Fun, N, Com) ->
	com_space(B, Fun, N, Com);
com_space(<<>>, Fun, N, Com) ->
	{B, Fun1} = Fun(),
	com_space(B, Fun1, N, Com);
com_space(B, Fun, N, _Com) ->
	com_enc(B, Fun, N, [], []).

com_enc(<<C:1/unit:8,B/binary>>, Fun, N, L, Ps) when C >= $a, C =< $z;
													 C >= $A, C =< $Z;
													 C >= $0, C =< $9 ->
	com_enc(B, Fun, N, [C | L], Ps);
com_enc(<<>>, Fun, N, L, Ps) ->
	case Fun() of
		{<<>>, _} ->
			com_enc_end([L | Ps]);
		{B, Fun1} ->
			com_enc(B, Fun1, N, L, Ps)
	end;
com_enc(<<"-",B/binary>>, Fun, N, L, Ps) ->
	com_enc(B, Fun, N, [], [L | Ps]);
com_enc(_B, _Fun, _N, L, Ps) ->
	com_enc_end([L | Ps]).

com_enc_end(Ps0) ->
	Ps = lists:reverse([lists:reverse(string:to_lower(P)) || P <- Ps0]),
	com_encoding(Ps).

com_encoding(["latin","1"|_]) ->
	latin1;
com_encoding(["utf","8"|_]) ->
	utf8;
com_encoding(_) ->
	throw(no). % Don't try any further

normalize_typed_record_fields([]) ->
	{typed, []};
normalize_typed_record_fields(Fields) ->
	normalize_typed_record_fields(Fields, [], false).

normalize_typed_record_fields([], NewFields, Typed) ->
	case Typed of
	true -> {typed, lists:reverse(NewFields)};
	false -> not_typed
	end;
normalize_typed_record_fields([{typed_record_field,Field,_}|Rest],
				  NewFields, _Typed) ->
	normalize_typed_record_fields(Rest, [Field|NewFields], true);
normalize_typed_record_fields([Field|Rest], NewFields, Typed) ->
	normalize_typed_record_fields(Rest, [Field|NewFields], Typed).

restore_typed_record_fields([]) ->
	[];
restore_typed_record_fields([{attribute,La,record,{Record,_NewFields}},
							 {attribute,La,type,{{record,Record},Fields,[]}}|
							 Forms]) ->
	[{attribute,La,record,{Record,Fields}}|
	 restore_typed_record_fields(Forms)];
restore_typed_record_fields([{attribute,La,type,{{record,Record},Fields,[]}}|
							 Forms]) ->
	%% This clause is due to the compiler's 'E' option.
	%% Record information kept by erl_expand_records.
	[{attribute,La,record,{Record,Fields}}|
	 restore_typed_record_fields(Forms)];
restore_typed_record_fields([Form|Forms]) ->
	[Form|restore_typed_record_fields(Forms)].

server(Pid, Name, Options, #epp{pre_opened=PreOpened}=St) ->
	process_flag(trap_exit, true),
	case PreOpened of
		false ->
			case file:open(Name, [read]) of
				{ok,File} ->
					init_server(Pid, Name, Options, St#epp{file = File});
				{error,E} ->
					epp_reply(Pid, {error,E})
			end;
		true ->
			init_server(Pid, Name, Options, St)
	end.

init_server(Pid, Name, Options, St0) ->
	Pdm = proplists:get_value(macros, Options, []),
	Ms0 = predef_macros(Name),
	case user_predef(Pdm, Ms0) of
	{ok,Ms1} ->
		#epp{file = File, location = AtLocation} = St0,
			DefEncoding = proplists:get_value(default_encoding, Options,
											  ?DEFAULT_ENCODING),
			Encoding = set_encoding(File, DefEncoding),
			epp_reply(Pid, {ok,self(),Encoding}),
			%% ensure directory of current source file is
			%% first in path
			Path = [filename:dirname(Name) |
					proplists:get_value(includes, Options, [])],
			St = St0#epp{delta=0, name=Name, name2=Name,
			 path=Path, macs=Ms1,
			 default_encoding=DefEncoding},
			From = wait_request(St),
			Anno = erl_anno:new(AtLocation),
			enter_file_reply(From, file_name(Name), Anno,
				 AtLocation, code),
			wait_req_scan(St);
	{error,E} ->
		epp_reply(Pid, {error,E})
	end.

%% predef_macros(FileName) -> Macrodict
%%  Initialise the macro dictionary with the default predefined macros,
%%  FILE, LINE, MODULE as undefined, MACHINE and MACHINE value.

predef_macros(File) ->
	Machine = list_to_atom(erlang:system_info(machine)),
	Anno = line1(),
	Defs = [{'FILE', 	           {none,[{string,Anno,File}]}},
		{'FUNCTION_NAME',      undefined},
		{'FUNCTION_ARITY',     undefined},
		{'LINE',		   {none,[{integer,Anno,1}]}},
		{'MODULE',	           undefined},
		{'MODULE_STRING',      undefined},
		{'BASE_MODULE',	   undefined},
		{'BASE_MODULE_STRING', undefined},
		{'MACHINE',	           {none,[{atom,Anno,Machine}]}},
		{Machine,	           {none,[{atom,Anno,true}]}}
	   ],
	maps:from_list(Defs).

%% user_predef(PreDefMacros, Macros) ->
%%	{ok,MacroDict} | {error,E}
%%  Add the predefined macros to the macros dictionary. A macro without a
%%  value gets the value 'true'.

user_predef([{M,Val,redefine}|Pdm], Ms) when is_atom(M) ->
	Exp = erl_parse:tokens(erl_parse:abstract(Val)),
	user_predef(Pdm, Ms#{M=>{none,Exp}});
user_predef([{M,Val}|Pdm], Ms) when is_atom(M) ->
	case Ms of
	#{M:=Defs} when is_list(Defs) ->
		 %% User defined macros.
		{error,{redefine,M}};
	#{M:=_Defs} ->
		%% Predefined macros.
		{error,{redefine_predef,M}};
	_ ->
		Exp = erl_parse:tokens(erl_parse:abstract(Val)),
		user_predef(Pdm, Ms#{M=>[{none,{none,Exp}}]})
	end;
user_predef([M|Pdm], Ms) when is_atom(M) ->
	user_predef([{M,true}|Pdm], Ms);
user_predef([Md|_Pdm], _Ms) -> {error,{bad,Md}};
user_predef([], Ms) -> {ok,Ms}.

%% wait_request(EppState) -> RequestFrom
%% wait_req_scan(EppState)
%% wait_req_skip(EppState, SkipIstack)
%%  Handle requests, processing trivial requests directly. Either return
%%  requestor or scan/skip tokens.

wait_request(St) ->
	receive
	{epp_request,From,scan_erl_form} -> From;
	{epp_request,From,macro_defs} ->
		%% Return the old format to avoid any incompability issues.
		Defs = [{{atom,K},V} || {K,V} <- maps:to_list(St#epp.macs)],
		epp_reply(From, Defs),
		wait_request(St);
	{epp_request,From,close} ->
		close_file(St),
		epp_reply(From, ok),
		exit(normal);
	{'EXIT',_,R} ->
		exit(R);
	Other ->
		io:fwrite("Epp: unknown '~w'\n", [Other]),
		wait_request(St)
	end.

close_file(#epp{pre_opened = true}) ->
	ok;
close_file(#epp{pre_opened = false, file = File}) ->
	ok = file:close(File).

wait_req_scan(St) ->
	From = wait_request(St),
	scan_toks(From, St).

enter_file_reply(From, Name, LocationAnno, AtLocation, Where) ->
	Anno0 = loc_anno(AtLocation),
	Anno = case Where of
			   code -> Anno0;
			   generated -> erl_anno:set_generated(true, Anno0)
		   end,
	Rep = {ok, [{'-',Anno},{atom,Anno,file},{'(',Anno},
		{string,Anno,Name},{',',Anno},
		{integer,Anno,get_line(LocationAnno)},{')',LocationAnno},
				{dot,Anno}]},
	epp_reply(From, Rep).

%% Flatten filename to a string. Must be a valid filename.

file_name([C | T]) when is_integer(C), C > 0 ->
	[C | file_name(T)];
file_name([H|T]) ->
	file_name(H) ++ file_name(T);
file_name([]) ->
	[];
file_name(N) when is_atom(N) ->
	atom_to_list(N).

leave_file(From, St) ->
	case St#epp.istk of
	[I|Cis] ->
		epp_reply(From,
			  {error,{St#epp.location,epp,
							  {illegal,"unterminated",I}}}),
		leave_file(wait_request(St),St#epp{istk=Cis});
	[] ->
		case St#epp.sstk of
		[OldSt|Sts] ->
			close_file(St),
					#epp{location=OldLoc, delta=Delta, name=OldName,
						 name2=OldName2} = OldSt,
					CurrLoc = add_line(OldLoc, Delta),
					Anno = erl_anno:new(CurrLoc),
			Ms0 = St#epp.macs,
			Ms = Ms0#{'FILE':={none,[{string,Anno,OldName2}]}},
					NextSt = OldSt#epp{sstk=Sts,macs=Ms,uses=St#epp.uses},
			enter_file_reply(From, OldName, Anno, CurrLoc, code),
					case OldName2 =:= OldName of
						true ->
							ok;
						false ->
							NFrom = wait_request(NextSt),
							OldAnno = erl_anno:new(OldLoc),
							enter_file_reply(NFrom, OldName2, OldAnno,
											 CurrLoc, generated)
						end,
					wait_req_scan(NextSt);
		[] ->
			epp_reply(From, {eof,St#epp.location}),
			wait_req_scan(St)
		end
	end.

%% scan_toks(From, EppState)
%% scan_toks(Tokens, From, EppState)

scan_toks(From, St) ->
	case io:scan_erl_form(St#epp.file, '', St#epp.location) of
	{ok,Toks,Cl} ->
		scan_toks(Toks, From, St#epp{location=Cl});
	{error,E,Cl} ->
		epp_reply(From, {error,E}),
		wait_req_scan(St#epp{location=Cl});
	{eof,Cl} ->
		leave_file(From, St#epp{location=Cl});
	{error,_E} ->
			epp_reply(From, {error,{St#epp.location,epp,cannot_parse}}),
		leave_file(wait_request(St), St)	%This serious, just exit!
	end.

keep_token({'?', _}) -> false;
keep_token(_) -> true.

scan_toks(Toks0, From, St) ->
	Toks1 = [ T || T <- Toks0, keep_token(T) ],
	epp_reply(From, {ok,Toks1}),
	wait_req_scan(St#epp{macs=scan_module(Toks1, St#epp.macs)}).

scan_module([{'-',_Lh},{atom,_Lm,module},{'(',_Ll}|Ts], Ms) ->
	scan_module_1(Ts, Ms);
scan_module([{'-',_Lh},{atom,_Lm,extends},{'(',_Ll}|Ts], Ms) ->
	scan_extends(Ts, Ms);
scan_module(_Ts, Ms) -> Ms.

scan_module_1([{atom,_,_}=A,{',',L}|Ts], Ms) ->
	%% Parameterized modules.
	scan_module_1([A,{')',L}|Ts], Ms);
scan_module_1([{atom,Ln,A}=ModAtom,{')',_Lr}|_Ts], Ms0) ->
	ModString = atom_to_list(A),
	Ms = Ms0#{'MODULE':={none,[ModAtom]}},
	Ms#{'MODULE_STRING':={none,[{string,Ln,ModString}]}};
scan_module_1(_Ts, Ms) -> Ms.

scan_extends([{atom,Ln,A}=ModAtom,{')',_Lr}|_Ts], Ms0) ->
	ModString = atom_to_list(A),
	Ms = Ms0#{'BASE_MODULE':={none,[ModAtom]}},
	Ms#{'BASE_MODULE_STRING':={none,[{string,Ln,ModString}]}};
scan_extends(_Ts, Ms) -> Ms.

epp_request(Epp) ->
	wait_epp_reply(Epp, erlang:monitor(process, Epp)).

epp_request(Epp, Req) ->
	Epp ! {epp_request,self(),Req},
	wait_epp_reply(Epp, erlang:monitor(process, Epp)).

epp_reply(From, Rep) ->
	From ! {epp_reply,self(),Rep},
	ok.

wait_epp_reply(Epp, Mref) ->
	receive
	{epp_reply,Epp,Rep} ->
		erlang:demonitor(Mref, [flush]),
		Rep;
	{'DOWN',Mref,_,_,E} ->
		receive {epp_reply,Epp,Rep} -> Rep
		after 0 -> exit(E)
		end
	end.

%% The line only. (Other tokens may have the column and text as well...)
loc_anno(Line) when is_integer(Line) ->
	erl_anno:new(Line);
loc_anno({Line,_Column}) ->
	erl_anno:new(Line).

add_line(Line, Offset) when is_integer(Line) ->
	Line+Offset;
add_line({Line, Column}, Offset) ->
	{Line+Offset, Column}.

line1() ->
	erl_anno:new(1).

get_line(Anno) ->
	erl_anno:line(Anno).

%% epp has always output -file attributes when entering and leaving
%% included files (-include, -include_lib). Starting with R11B the
%% -file attribute is also recognized in the input file. This is
%% mainly aimed at yecc, the parser generator, which uses the -file
%% attribute to get correct lines in messages referring to code
%% supplied by the user (actions etc in .yrl files).
%%
%% In a perfect world (read: perfectly implemented applications such
%% as Xref, Cover, Debugger, etc.) it would not be necessary to
%% distinguish -file attributes from epp and the input file. The
%% Debugger for example could have one window for each referred file,
%% each window with its own set of breakpoints etc. The line numbers
%% of the abstract code would then point into different windows
%% depending on the -file attribute. [Note that if, as is the case for
%% yecc, code has been copied into the file, then it is possible that
%% the copied code differ from the one referred to by the -file
%% attribute, which means that line numbers can mismatch.] In practice
%% however it is very rare with Erlang functions in included files, so
%% only one window is used per module. This means that the line
%% numbers of the abstract code have to be adjusted to refer to the
%% top-most source file. The function interpret_file_attributes/1
%% below interprets the -file attribute and returns forms where line
%% numbers refer to the top-most file. The -file attribute forms that
%% have been output by epp (corresponding to -include and
%% -include_lib) are kept, but the user's -file attributes are
%% removed. This seems sufficient for now.
%%
%% It turns out to be difficult to distinguish -file attributes in the
%% input file from the ones added by epp unless some action is taken.
%% The (less than perfect) solution employed is to let epp assign
%% negative line numbers to user supplied -file attributes.

%% Note: it is assumed that the second element is a line or a key-list
%% where 'line' can be found.

interpret_file_attribute(Forms) ->
	interpret_file_attr(Forms, 0, []).

interpret_file_attr([{attribute,Anno,file,{File,Line}}=Form | Forms],
					Delta, Fs) ->
	L = get_line(Anno),
	Generated = erl_anno:generated(Anno),
	if
		Generated ->
			%% -file attribute
			interpret_file_attr(Forms, (L + Delta) - Line, Fs);
		not Generated ->
			%% -include or -include_lib
			% true = L =:= Line,
			case Fs of
				[_, File | Fs1] -> % end of included file
					[Form | interpret_file_attr(Forms, 0, [File | Fs1])];
				_ -> % start of included file
					[Form | interpret_file_attr(Forms, 0, [File | Fs])]
			end
	end;
interpret_file_attr([Form0 | Forms], Delta, Fs) ->
	F = fun(Anno) ->
				Line = erl_anno:line(Anno),
				erl_anno:set_line(Line + Delta, Anno)
		end,
	Form = erl_parse:map_anno(F, Form0),
	[Form | interpret_file_attr(Forms, Delta, Fs)];
interpret_file_attr([], _Delta, _Fs) ->
	[].

