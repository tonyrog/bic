%% File    : bic.erl
%%% Author  : Tony Rogvall <tony@iMac.local>
%%% Description : BEAM interpreted C (ode)
%%% Created : 27 Dec 2005 by Tony Rogvall <tony@iMac.local>

-module(bic).

-export([main/1]).
-export([file/1, file/2]).
-export([string/1, string/2]).
-export([parse/1, parse/2]).
-export([print/1, print/2]).
-export([unique/1, unique/2]).

-export([is_atomic/1, is_expr/1]).
-export([is_typespec/1, is_type/1]).
-export([is_statement/1]).
-export([is_compare_op/1]).
-export([is_logical_op/1]).
-export([is_bitwise_op/1]).
-export([is_shift_op/1]).
-export([is_integer_op/1]).
-export([is_float_op/1]).
-export([is_pointer_op/1]).

-export([combine_types/2]).
-export([complete_type/1]).
-export([token_to_integer/1, token_to_integer/2]).
-export([token_to_float/1]).
-export([token_to_string/1]).
-export([token_to_char/1]).

-compile(export_all).

-import(lists, [map/2]).

-include("../include/bic.hrl").

%% -define(dbg(F,A), io:format((F),(A))).
-define(dbg(F,A), ok).

-type cpp_option() :: {define,Name::string(),Value::term()} |
		      {include, Path::string()} |
		      {qinclude, Path::string()}.
-type cpp_options() :: [cpp_option()].

-type bic_options() :: 
	#{ model => undefined | 32 | 64,
	   cpp_only => boolean(),
	   cpp_dump => boolean(),
	   lint => boolean(),
	   unique => boolean(),
	   unroll => boolean(),
	   cpp => cpp_options()
	 }.

-type bic() :: [#bic_function{} | #bic_decl{}].

main(Args) ->
    main(Args, #{ defines => [] }).

main(["-m32" | Args], Opts) ->
    main(Args, Opts#{ model => 32 });
main(["-m64" | Args], Opts) ->
    main(Args, Opts#{ model => 64 });
main(["-cpp" | Args], Opts) ->
    main(Args, Opts#{ cpp_only => true });
main(["-cdd" | Args], Opts) ->
    main(Args, Opts#{ cpp_dump => true });
main(["-nolint" | Args], Opts) ->
    main(Args, Opts#{ lint => false });
main(["-unique" | Args], Opts) ->
    main(Args, Opts#{ unique => true });
main(["-unroll" | Args], Opts) ->
    main(Args, Opts#{ unroll => true });
main(["-D",MacroValue|Args], Opts) ->
    case string:split(MacroValue,"=") of
	[Macro,Value] ->
	    Defs = maps:get(cpp, Opts, []),
	    Defs1 = [{defined,Macro,Value}|Defs],
	    main(Args, Opts#{ cpp => Defs1 });
	[Macro] ->
	    Defs = maps:get(cpp, Opts, []),
	    Defs1 = [{defined,Macro,1}|Defs],
	    main(Args, Opts#{ cpp => Defs1 })
    end;
main(["-D"++MacroValue|Args], Opts) ->
    case string:split(MacroValue,"=") of
	[Macro,Value] ->
	    Defs = maps:get(cp, Opts, []),
	    Defs1 = [{defined,Macro,Value}|Defs],
	    main(Args, Opts#{ cpp => Defs1 });
	[Macro] ->
	    Defs = maps:get(cpp, Opts, []),
	    Defs1 = [{defined,Macro,1}|Defs],
	    main(Args, Opts#{ cpp => Defs1 })
    end;
main(["-h"|_Args], _Opts) -> usage();
main(["-help"|_Args], _Opts) -> usage();
main([],_) ->
    io:format("bic: no input files\n"),
    halt(1);
main(Files, Opts) ->
    case run(Files, Opts) of
	ok -> halt(0);
	{error,_} -> halt(1)
    end.

usage() ->
    io:format("usage: bic [-m32|-m64] options -Dmacro=value files\n"),
    io:format("OPTIONS\n"),
    io:format("  -cpp      cpp only\n"),
    io:format("  -nolint   skip lint\n"),
    io:format("  -unique   make variables unique\n"),
    io:format("  -unroll   unroll functions\n"),
    halt(0).

run([Filename|Files], Opts) ->
    case file(Filename, Opts) of
	{ok,Forms} ->
	    Forms1 =
		case maps:get(unique, Opts, false) of
		    true ->
			bic_transform:unique(Forms);
		    false ->
			Forms
		end,
	    io:put_chars(bic_format:definitions(Forms1)),
	    run(Files, Opts);
	_Err ->
	    halt(1)
    end;
run([], _Opts) ->
    halt(0).

string(String) ->
    string(String, #{}).

-spec string(String::string(), Env::cpp_options()) ->
		    {ok, bic()} | {error, Reason::term()}.

string(String, Opts) ->
    CppOpts = maps:get(cpp, Opts, []),
    CppOpts1 = [{define,"__bic__", 1},
	     {define,"__bic_extension_bitfield__", 1} | CppOpts],
    case bic_cpp:string(String, CppOpts1) of
	{ok,Fd} ->
	    bic_scan:init(),  %% setup some dictionay stuff
	    bic_parse:init(), %% setup some dictionay stuff
	    Res = (catch bic_parse:parse_and_scan({bic, scan, [Fd]})),
	    case maps:get(cpp_dump, Opts, false) of
		true ->
		    dump_cpp_variables(Fd);
		false ->
		    ok
	    end,
	    bic_cpp:close(Fd),
	    case Res of 
		{error,{{Fn,Ln},Mod,Message}} when is_integer(Ln) ->
		    %% io:format("Message: ~w\n", [Message]),
		    io:format("~s:~w: ~s\n",
			      [Fn,Ln,Mod:format_error(Message)]),
		    {error,parse_error};
		{error,{Ln,Mod,Message}} when is_integer(Ln) ->
		    %% io:format("Message: ~w\n", [Message]),
		    io:format("~s:~w: ~s\n",
			      ["*string*",Ln,Mod:format_error(Message)]),
		    {error,parse_error};
		{ok,Forms} ->
		    {ok,Forms}
	    end;
	Error ->
	    Error
    end.

%% parse and lint file
file(File) ->
    file(File,#{}).

-spec file(Filename::string(), Opts::bic_options()) ->
		  {ok, bic()} | {error, Reason::term()}.

file(Filename,Opts) ->
    case parse(Filename, Opts) of
	{ok,Defs} ->
	    case maps:get(cpp_only, Opts, false) of
		true ->
		    {ok,Defs};
		false ->
		    case maps:get(lint, Opts, true) of
			true ->
			    case bic_lint:definitions(Filename,Defs) of
				{ok,LintDefs} ->
				    {ok,LintDefs};
				Err={error,_} ->
				    Err
			    end;
			false ->
			    {ok,Defs}
		    end
	    end;
	Error ->
	    Error
    end.

print(Filename) ->
    print(Filename,#{}).

print(Filename, Opts) ->
    case file(Filename, Opts) of
	{ok,Forms} ->
	    io:put_chars(bic_format:definitions(Forms));
	Error ->
	    Error
    end.

unique(Filename) ->
    unique(Filename,#{}).

unique(Filename, Opts) ->
    case file(Filename, Opts) of
	{ok,Forms} ->
	    Forms1 = bic_transform:unique(Forms),
	    io:put_chars(bic_format:definitions(Forms1));
	Error ->
	    Error
    end.

-spec parse(Filename::string(), Env::bic_options()) ->
		   {ok, bic()} | {error, Reason::term()}.
%% parse file, not lint
parse(File) ->
    parse(File, #{}).

parse(Filename, Opts) ->
    CppOpts = maps:get(cpp, Opts, []),
    CppOpts1 = [{define,"__bic__", 1},
		{define,"__bic_extension_bitfield__", 1} | CppOpts],
    case bic_cpp:open(Filename, CppOpts1) of
	{ok,Fd} ->
	    bic_scan:init(),  %% setup some dictionay stuff
	    bic_parse:init(), %% setup some dictionay stuff
	    Res = (catch bic_parse:parse_and_scan({bic, scan, [Fd]})),
	    case maps:get(cpp_dump, Opts, false) of
		true ->
		    dump_cpp_variables(Fd);
		false ->
		    ok
	    end,
	    bic_cpp:close(Fd),
	    case Res of 
		{error,{{Fn,Ln},Mod,Message}} when is_integer(Ln) ->
		    %% io:format("Message: ~w\n", [Message]),
		    io:format("~s:~w: ~s\n",
			      [Fn,Ln,Mod:format_error(Message)]),
		    {error,parse_error};
		{error,{Ln,Mod,Message}} when is_integer(Ln) ->
		    %% io:format("Message: ~w\n", [Message]),
		    io:format("~s:~w: ~s\n",
			      [Filename,Ln,Mod:format_error(Message)]),
		    {error,parse_error};
		{ok,Forms} ->
		    {ok,Forms}
	    end
    end.

dump_cpp_variables(Fd) ->
    lists:foreach(
      fun({Name,[{string,_,Value}]}) ->
	      io:format("~s: \"~s\"\n", [Name,Value]);
	 ({Name, Value}) ->
	      io:format("~s: ~p\n", [Name,Value])
      end, bic_cpp:values(Fd)).
    
%% Scanner wrapper (simplify debugging)
scan(Fd) ->
    Res = bic_scan:scan(Fd),
    %% io:format("Scan: ~p\n", [Res]),
    Res.


is_atomic(Const) when is_number(Const) -> true; %% maybe remove
is_atomic(#bic_id{}) -> true;
is_atomic(#bic_constant{}) -> true;
is_atomic(_) -> false.

is_expr(#bic_unary{}) -> true;
is_expr(#bic_binary{}) -> true;
is_expr(#bic_call{}) -> true;
is_expr(#bic_ifexpr{}) -> true;
is_expr(#bic_assign{}) -> true;
is_expr(X) -> is_atomic(X).

is_typespec(#bic_type{}) -> true;
is_typespec(#bic_struct{}) -> true;
is_typespec(#bic_union{}) -> true;
is_typespec(#bic_typeid{}) -> true;
is_typespec(#bic_enum{}) -> true;
is_typespec(_) -> false.

is_type(#bic_pointer{}) -> true;
is_type(#bic_array{}) -> true;
is_type(#bic_fn{}) -> true;
is_type(T) -> is_typespec(T).

is_statement(#bic_decl{}) -> true;
is_statement(#bic_for{}) -> true;
is_statement(#bic_while{}) -> true;
is_statement(#bic_do{}) -> true;
is_statement(#bic_if{}) -> true;
is_statement(#bic_switch{}) -> true;
is_statement(#bic_case{}) -> true;
is_statement(#bic_default{}) -> true;
is_statement(#bic_label{}) -> true;
is_statement(#bic_goto{}) -> true;
is_statement(#bic_continue{}) -> true;
is_statement(#bic_break{}) -> true;
is_statement(#bic_return{}) -> true;
is_statement(#bic_empty{}) -> true;
is_statement(#bic_compound{}) -> true;
is_statement(#bic_expr_stmt{}) -> true;
is_statement(_) -> false.

is_compare_op('<') -> true;
is_compare_op('<=') -> true;
is_compare_op('>') -> true;
is_compare_op('>=') -> true;
is_compare_op('==') -> true;
is_compare_op('!=') -> true;
is_compare_op(_) -> false.

is_logical_op('&&') -> true;
is_logical_op('||') -> true;
is_logical_op('!') -> true;
is_logical_op(_) -> false.

is_bitwise_op('&') -> true;
is_bitwise_op('|') -> true;
is_bitwise_op('^') -> true;
is_bitwise_op('~') -> true;
is_bitwise_op(_) -> false.

is_shift_op('>>') -> true;
is_shift_op('<<') -> true;
is_shift_op(_) -> false.
    
is_integer_op('+') -> true;
is_integer_op('-') -> true;
is_integer_op('*') -> true; 
is_integer_op('/') -> true; 
is_integer_op('%') -> true; 
is_integer_op(Op) ->
    is_bitwise_op(Op) orelse
	is_shift_op(Op) orelse
	is_logical_op(Op) orelse
	is_compare_op(Op).

is_float_op('+') -> true;
is_float_op('-') -> true;
is_float_op('*') -> true;
is_float_op('/') -> true;
is_float_op(_) -> false.
    
is_pointer_op('+') -> true;
is_pointer_op('-') -> true;
is_pointer_op('++') -> true;
is_pointer_op('--') -> true;
is_pointer_op(Op) -> is_compare_op(Op).

%% fill in some blanks in types!
%% long => long int
%% unsigned => unsigned int
%% etc

complete_type(T=#bic_type{type=undefined}) ->
    if T#bic_type.sign =/= undefined;
       T#bic_type.size =/= undefined -> T#bic_type{type=int};
       true -> T  %% probably an error
    end;
complete_type(A=#bic_pointer{type=T}) ->
    A#bic_pointer{type=complete_type(T)};
complete_type(A=#bic_fn{type=T,params=Ds}) ->
    A#bic_fn{type=complete_type(T),
	     params=[D#bic_decl{type=complete_type(D#bic_decl.type)}||D<-Ds]};
complete_type(A=#bic_array{type=T}) ->
    A#bic_array{type=complete_type(T)};
complete_type(A=#bic_struct{elems=Ds}) ->
    A#bic_struct{elems=[D#bic_decl{type=complete_type(D#bic_decl.type)} 
			|| D <- Ds]};
complete_type(A=#bic_union{elems=Ds}) ->
    A#bic_union{elems=[D#bic_decl{type=complete_type(D#bic_decl.type)} 
		       || D <- Ds]};
complete_type(A) -> A.
    

combine_types(T1,T2) ->
    ?dbg("combine_types: [ ~s ]  [ ~s ] => ",[format_type(T1),format_type(T2)]),
    %% ?dbg("combine_types: [ ~w ]  [ ~w ] => ", [ T1, T2]),
    R = combine_types_(T1,T2),
    ?dbg("[ ~s ]\n", [format_type(R)]),
    %% ?dbg("[ ~w ]\n", [R]),
    R.

combine_types_(A=#bic_pointer{type=T1=#bic_type{type='_'}},
	       B=#bic_fn{type=T2}) ->
    A#bic_pointer{type=B#bic_fn{type=combine_types_(T1,T2)}};

combine_types_(undefined, B=#bic_fn{type='_'}) -> B;
combine_types_(A, B=#bic_fn{type='_'}) -> B#bic_fn{type=A};

combine_types_(A=#bic_pointer{type=T1}, B=#bic_fn{type=T2}) ->
    A#bic_pointer{type=B#bic_fn{type=combine_types_(T1,T2)}};

combine_types_(A=#bic_pointer{type=T1}, B=#bic_array{type=T2}) -> 
    B#bic_array{type=A#bic_pointer{type=combine_types_(T1,T2)}};

combine_types_(T1=#bic_pointer{line=L,type='_'}, undefined) ->
    T1#bic_pointer{type=#bic_type{line=L,type='_'}}; %%???

combine_types_(A=#bic_pointer{type=T1}, T2) ->
    A#bic_pointer{type=combine_types_(T1,T2)};

combine_types_(T1=#bic_type{}, B=#bic_pointer{type=Const=#bic_type{const=true,type=undefined}}) ->
    Const#bic_type{type=B#bic_pointer{type=T1}};

combine_types_(A=#bic_array{type=T1}, T2) ->
    A#bic_array{type=combine_types_(T1,T2)};

combine_types_(undefined,T2) -> T2;
combine_types_(T1,undefined) -> T1;

combine_types_(T1, B=#bic_pointer{type=T2}) ->
    B#bic_pointer{type=combine_types_(T1,T2)};
combine_types_(T1, B=#bic_array{type=T2}) ->
    B#bic_array{type=combine_types_(T1,T2)};

combine_types_(T1, B=#bic_fn{type=T2}) ->
    B#bic_fn{type=combine_types_(T1,T2)};

combine_types_('_',T2) -> T2;
combine_types_(T1,'_') -> T1;
combine_types_(T1=#bic_type{},T2=#bic_type{}) ->
    T2#bic_type { sign     = defined(T1#bic_type.sign, T2#bic_type.sign),
		  const    = defined(T1#bic_type.const, T2#bic_type.const),
		  volatile = defined(T1#bic_type.volatile, T2#bic_type.volatile),
		  size     = defined(T1#bic_type.size, T2#bic_type.size),
		  type     = defined(T1#bic_type.type, T2#bic_type.type) };
combine_types_(T1=#bic_type{const=true,type=undefined},T2) ->
    T1#bic_type{type=T2};
combine_types_(A, #bic_type{type='_'}) -> A;
combine_types_(#bic_type{type=undefined}, B) -> B.


defined('_', V) -> V;
defined(V, '_') -> V;
defined(undefined, V) -> V;
defined(V, undefined) -> V;
defined(long, long) ->  long_long;
defined(V, V) ->  V;
defined(V, _W) ->
    %% FIXME: actually a lint! error...
    io:format("error: merge values ~p and ~p\n", [V, _W]),
    V.

%% convert constants
token_to_integer([$0,$x|Value]) -> token_to_integer_(Value, 16);
token_to_integer([$0,$X|Value]) -> token_to_integer_(Value, 16);
token_to_integer([$0,$b|Value]) -> token_to_integer_(Value, 2);
token_to_integer([$0,$B|Value]) -> token_to_integer_(Value, 2);
token_to_integer([$0|Value]) -> token_to_integer_(Value, 8);
token_to_integer(Value) -> token_to_integer_(Value, 10).

token_to_integer([$0,$x|Value], 16) -> token_to_integer_(Value, 16);
token_to_integer([$0,$X|Value], 16) -> token_to_integer_(Value, 16);
token_to_integer([$0,$b|Value], 2) -> token_to_integer_(Value, 2);
token_to_integer([$0,$B|Value], 2) -> token_to_integer_(Value, 2);
token_to_integer([$0|Value], 8) -> token_to_integer_(Value, 8);
token_to_integer(Value, 10) -> token_to_integer_(Value, 10).

token_to_integer_(Ds, Base) ->
    token_to_integer_(Ds, Base, 0).

token_to_integer_([D|Ds], Base, Val) when 
      Base =< 20, (D-$0) >= 0, (D-$0) < 10 ->
    token_to_integer_(Ds, Base, Val*Base + (D-$0));
token_to_integer_([D|Ds], Base, Val) when 
      Base =<  20, (D-$A) >= 0, (D-$A) < (Base-10) ->
    token_to_integer_(Ds, Base, Val*Base + (D-$A)+10);
token_to_integer_([D|Ds], Base, Val) when 
      Base =<  20, (D-$a) >= 0, (D-$a) < (Base-10) ->
    token_to_integer_(Ds, Base, Val*Base + (D-$a)+10);
token_to_integer_([], _Base, Val) -> 
    {Val, #bic_type { const=true, type=int}};
token_to_integer_(Suffix, _Base, Val) -> 
    T =
	case string:to_lower(Suffix) of
	    "l" -> 
		#bic_type { const=true,size=long,type=int};
	    "ll" ->
		#bic_type { const=true,size=long_long,type=int};
	    "u" ->
		#bic_type { const=true,sign=unsigned,type=int};
	    "lu" -> 
		#bic_type { const=true,sign=unsigned,size=long,type=int};
	    "ul" ->
		#bic_type { const=true,sign=unsigned,size=long,type=int};
	    "llu" -> 
		#bic_type { const=true,sign=unsigned,size=long_long,type=int};
	    "ull" ->
		#bic_type { const=true,sign=unsigned,size=long_long,type=int};
	    "z" ->
		#bic_typeid { name="ssize_t" };
	    "uz" ->
		#bic_typeid { name="size_t" };
	    "zu" ->
		#bic_typeid { name="size_t" }
	end,
    {Val, T}.

token_to_float(Cs) ->
    Cs2 = case Cs of
	      [$.|Cs1] -> [$0,$.|Cs1];
	      _ -> Cs
	  end,
    float_suffix(lists:reverse(string:to_lower(Cs2)),
		 #bic_type{const=true,type=double}).

float_suffix([$f|Cs], Type) ->
    float_suffix(Cs, Type#bic_type{type=float});
float_suffix([$l|Cs], Type) ->
    float_suffix(Cs, Type#bic_type{size=long});
float_suffix(Cs, Type) ->
    {list_to_float(lists:reverse(Cs)), Type}.

%% fixme wchar_t
token_to_char([$',$\\|Cs]) ->
    {Val,[$']} = escape(Cs),
    {Val, #bic_type{ const=true, type=char} };
token_to_char([$',C,$']) ->
    {C, #bic_type{ const=true, type=char} }.

%% fixme wchar_t
token_to_string(Token) ->
    token_to_string_(Token, [], #bic_type{ const=true, type=char}).

token_to_string_([$\\|Cs], Acc, Type) ->
    {C,Cs1} = escape(Cs),
    token_to_string_(Cs1, [C|Acc], Type);
token_to_string_([C|Cs], Acc, Type) ->
    token_to_string_(Cs, [C|Acc], Type);
token_to_string_([], Acc, Type) ->
    {lists:reverse(Acc), Type}.

-define(is_hex(C), 
	((((C)>=$0) andalso ((C)=<$9)) 
	 orelse
	   (((C)>=$a) andalso ((C)=<$f))
	 orelse
	   (((C)>=$A) andalso ((C)=<$F)))).

%% after \ 
escape([$a|Cs]) -> {16#07, Cs};
escape([$b|Cs]) -> {16#08, Cs};
escape([$e|Cs]) -> {16#1B, Cs};
escape([$f|Cs]) -> {16#0C, Cs};
escape([$n|Cs]) -> {16#0A, Cs};
escape([$r|Cs]) -> {16#0D, Cs};
escape([$t|Cs]) -> {16#09, Cs};
escape([$v|Cs]) -> {16#0B, Cs};
escape([$\\|Cs]) -> {16#5C, Cs};
escape([$\'|Cs]) -> {16#27, Cs};
escape([$\"|Cs]) -> {16#22, Cs};
escape([$\?|Cs]) -> {16#3F, Cs};
escape([N2,N1,N0|Cs]) when 
      (N2>=$0),(N2=<$3),(N1>=$0),(N1=<$7),(N0>=$0),(N0=<$7) ->
    {(N2-$0)*64+(N1-$0)*8+(N0-$0), Cs};
escape([N1,N0|Cs]) when 
      (N1>=$0),(N1=<$7),(N0>=$0),(N0=<$7) ->
    {(N1-$0)*8+(N0-$0), Cs};
escape([N0|Cs]) when 
      (N0>=$0),(N0=<$7) ->
    {(N0-$0), Cs};
escape([$x|Cs]) ->
    escape_hex(Cs);
escape([$u,X1,X2,X3,X4|Cs]) when 
      ?is_hex(X1),?is_hex(X2),?is_hex(X3),?is_hex(X4) ->
    {list_to_integer([X1,X2,X3,X4],16), Cs};
escape([$U,X1,X2,X3,X4,X5,X6,X7,X8|Cs]) when 
      ?is_hex(X1),?is_hex(X2),?is_hex(X3),?is_hex(X4),
      ?is_hex(X5),?is_hex(X6),?is_hex(X7),?is_hex(X8) ->
    {list_to_integer([X1,X2,X3,X4,X5,X6,X7,X8],16), Cs};
escape([C|Cs]) ->
    {C,Cs}.

escape_hex([C|Cs]) when ?is_hex(C) ->
    escape_hex(Cs, [C]).

escape_hex([C|Cs],Acc) when ?is_hex(C) ->
    escape_hex(Cs,[C|Acc]);
escape_hex(Cs, Acc) ->
    {list_to_integer(lists:reverse(Acc),16), Cs}.



    

