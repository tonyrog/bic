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

-export([format_type/1]).
-export([format_expr/1]).
-export([format_exprs/1]).
-export([format_decl/1]).
-export([format_statement/1, format_statement/2]).
-export([format_statements/2]).
-export([format_definition/1]).
-export([format_definitions/1]).
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
	    io:put_chars(format_definitions(Forms1)),
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
	{ok,Forms} ->
	    case maps:get(cpp_only, Opts, false) of
		true ->
		    {ok,Forms};
		false ->
		    case maps:get(lint, Opts, true) of
			true ->
			    case bic_lint:forms(Filename,Forms) of
				{ok,LintForms} ->
				    {ok,LintForms};
				Err={error,_} ->
				    Err
			    end;
			false ->
			    {ok,Forms}
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
	    io:put_chars(format_definitions(Forms));
	Error ->
	    Error
    end.

unique(Filename) ->
    unique(Filename,#{}).

unique(Filename, Opts) ->
    case file(Filename, Opts) of
	{ok,Forms} ->
	    Forms1 = bic_transform:unique(Forms),
	    io:put_chars(format_definitions(Forms1));
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

format_type(undefined) -> "N";
format_type('_') -> "_";
format_type(#bic_typeid{name=Name}) -> Name;
format_type(#bic_type{sign=S,const=C,volatile=V,size=Z,type=T}) ->
    lists_join_no_empty(
      " ",
      [if C -> "const"; true -> "" end,
       if V -> "volatile"; true -> "" end,
       case S of
	   undefined -> "";
	   signed -> "signed";
	   unsigned -> "unsigned"
       end,
       case Z of
	   undefined -> "";
	   short-> "short";
	   long -> "long";
	   long_long -> "long long"
       end,
       case T of
	   undefined when S =/= undefined; Z =/= undefined -> "";
	   undefined -> "U";
	   '_' -> "T";
	   void -> "void";
	   char -> "char";
	   int -> "int";
	   float -> "float";
	   double -> "double";
	   #bic_struct{} -> format_type(T);
	   #bic_union{} -> format_type(T);
	   #bic_typeid{name=Name} -> [Name];
	   #bic_enum{} -> format_type(T);
	   #bic_pointer{} -> ["(",format_type(T),")"]
       end]);
format_type(#bic_enum{name=Name,elems=Es}) ->
    ["enum ",optional(Name)," {", format_enums(Es), "}"];

%% fixme: pointer to function pointer?
format_type(#bic_pointer{type=Type=#bic_fn{}}) -> 
    ["(*)",format_type(Type)];

format_type(#bic_pointer{type=Type}) ->
    [format_type(Type),"*"];
format_type(#bic_array{type=Type,dim=_D}) ->
    [format_type(Type)];
format_type(#bic_fn{type=Type,params=Ps}) ->
    [format_type(Type),"(",format_params(Ps),")"];
format_type(#bic_struct{name=Name,elems=undefined}) ->
    ["struct ",optional(Name)];
format_type(#bic_struct{name=Name,elems=Es}) ->
    ["struct ",optional(Name),"{", format_decls(Es),"}"];
format_type(#bic_union{name=Name,elems=undefined}) ->
    ["union ",optional(Name)];
format_type(#bic_union{name=Name,elems=Es}) ->
    ["union ",optional(Name),"{", format_decls(Es),"}"].


format_dim(#bic_type{type=T}) when not is_atom(T) ->
    format_dim(T);
format_dim(#bic_array{type=T,dim=[]}) ->
    [format_dim(T), "[]"];
format_dim(#bic_array{type=T,dim=D}) ->
    [format_dim(T),["[",format_expr(D),"]"]];
format_dim(_) ->
    [].

optional(undefined) -> "";
optional(String) when is_list(String) -> String.

%% join strings but remove empty elements first
lists_join_no_empty(Sep,Es) ->
    lists:join(Sep, [A || A <- Es, A =/= ""]).

-type enum() :: {Name::string(),Line::integer(),Value::bic_expr()}.
-spec format_enum(E::enum()) -> string().

format_enum({Name,_Ln,Value}) ->
    [Name,"=",format_expr(Value)].

-spec format_enums(Enums::[enum()]) -> string().

format_enums([]) -> [];
format_enums([X]) -> [format_enum(X)];
format_enums([X|Xs]) -> [format_enum(X),",",format_enums(Xs)].

-spec format_decl(Decl::bic_decl()) -> string().

format_decl(#bic_decl{name=undefined,storage=Storage,type=Type,size=Size,value=Init}) ->
    [format_storage(Storage),format_type(Type),format_dim(Type),format_size(Size),format_init(Init)];
format_decl(#bic_decl{name=Name,storage=Storage,type=Type,size=Size,value=Init}) ->
    [format_storage(Storage),format_type(Type)," ",Name,format_dim(Type),format_size(Size),format_init(Init)].

format_size(undefined) -> "";
format_size(Size) -> [":",format_expr(Size)].

format_init(undefined) -> "";
format_init(Init) -> ["=",format_expr(Init)].
    
format_storage(undefined) -> "";
format_storage(Storage) -> [atom_to_list(Storage)," "].

-spec format_decls(Decls::[bic_decl()]) -> string().

format_decls([]) -> [];
format_decls([X|Xs]) -> [format_decl(X),";",format_decls(Xs)].

-spec format_params(Decls::[bic_decl()]) -> string().

format_params([]) -> [];
format_params([X]) -> [format_decl(X)];
format_params([X|Xs]) -> [format_decl(X),",",format_params(Xs)].

-spec prio(Decl::bic_decl()|number()) -> integer();
	  (Other::term()) -> false.

%% check if tuple is and expression 
%% return priority on success and false on failure
prio(Const) when is_number(Const) -> 0;
prio(#bic_constant{}) -> 0;
prio(#bic_id{}) -> 0;
prio(#bic_unary{op=Op}) ->
    case Op of
	'+++' -> 0;  %% postfix ++
	'---' -> 0;  %% postfix --
	'+' -> 5;
	'-' -> 5;
	'!' -> 5;
	'~' -> 5;
	'++' -> 6;   %% prefix
	'--' -> 6;   %% prefix
	'&' -> 7;
	'*' -> 7
    end;
prio(#bic_binary{op=Op}) ->
    case Op of
	'[]' -> 0;
	'*' -> 10;
	'/' -> 10;
	'%' -> 10;
	'+' -> 20;
	'-' -> 20;
	'<<' -> 30;
	'>>' -> 30;
	'<' -> 40;
	'<=' -> 40;
	'>' -> 40;
	'>=' -> 40;
	'==' -> 50;
	'!=' -> 50;
	'&' -> 60;
	'^' -> 70;
	'|' -> 80;
	'&&' -> 90;
	'||' -> 100;
	'='  -> 120;
	'+='  -> 120;
	'-='  -> 120;
	'*='  -> 120;
	'/='  -> 120;
	'%='  -> 120;
	'>>='  -> 120;
	'<<='  -> 120;
	'&='  -> 120;
	'^='  -> 120;
	'|='  -> 120;
	','  -> 130
    end;
prio(#bic_ifexpr{}) -> 
    110;
prio(_) ->
    false.

-spec format_expr(Expr::bic_expr()) -> string().

format_expr(X) when is_integer(X) -> integer_to_list(X);
format_expr(#bic_id{name=Name}) -> Name;
format_expr(#bic_constant{token=Value}) -> Value;

format_expr(#bic_unary{op='+++',arg=Arg}) -> 
    [format_expr(Arg),"++"];
format_expr(#bic_unary{op='---',arg=Arg}) -> 
    [format_expr(Arg),"--"];
format_expr(#bic_unary{op=Op,arg=Arg}) -> 
    [atom_to_list(Op),format_expr(Arg)];
format_expr(#bic_binary{op='[]',arg1=Arg1,arg2=Arg2}) -> 
    [format_expr(Arg1),"[",format_expr(Arg2),"]"];
format_expr(#bic_binary{op=cast,arg1=Type,arg2=Arg2}) -> 
    ["(",format_type(Type),")",format_expr(Arg2)];    
format_expr(X=#bic_binary{op=Op,arg1=Arg1,arg2=Arg2}) -> 
    P  = prio(X),
    P1 = prio(Arg1),
    P2 = prio(Arg1),
    [if is_number(P),is_number(P1),P1 > P -> 
	     ["(",format_expr(Arg1), ")"];
	true -> format_expr(Arg1)
     end,
     atom_to_list(Op),
     if is_number(P),is_number(P2),P2 > P ->
	     ["(",format_expr(Arg2), ")"];
	true ->
	     format_expr(Arg2)
     end];
format_expr(#bic_call{func=F, args=Exprs}) ->
    [format_expr(F),"(",format_exprs(Exprs),")"];
format_expr(#bic_ifexpr{test=C,then=T,else=E}) ->
    [format_expr(C),"?",format_expr(T),":",format_expr(E)];
format_expr(#bic_assign{op=Op,lhs=L,rhs=R}) ->
    [format_expr(L),atom_to_list(Op),format_expr(R)];
format_expr(Array) when is_list(Array) -> %% array init?
    ["{", format_exprs(Array), "}"].

-spec format_exprs(ExprList::[bic_expr()]) -> string().

format_exprs([]) -> [];
format_exprs([X]) -> [format_expr(X)];
format_exprs([X|Xs]) -> [format_expr(X),",",format_exprs(Xs)].

format_definitions([]) ->
    [];
format_definitions([D|Ds]) ->
    [format_definition(D) |
     format_definitions(Ds)];
format_definitions(D) ->
    format_definition(D).

format_definition(D) ->
    case D of
	#bic_function{name=Name,storage=Storage,type=Type,
		      params=Params,body=Body} ->
	    [format_storage(Storage),format_type(Type),format_dim(Type)," ",Name,
	     "(",format_params(Params),")", "\n",
	     format_statement(Body, 0)];
	#bic_typedef{name=Name,storage=Storage,type=Type,size=Size,value=_Init} ->
	    ["typedef ", format_storage(Storage),format_type(Type),
	     format_size(Size)," ",Name,format_dim(Type),";\n"];
	#bic_decl{} ->
	    [format_decl(D),";\n"]
    end.

indent(I) when I =< 0 -> 
    "";
indent(I) ->
    lists:duplicate(2*I, $\s).

format_statement(Stmt) ->
    format_statement(Stmt, 0).

format_statement(#bic_compound{code=Stmts}, I) when is_list(Stmts) ->
    format_icompound(Stmts,I);
format_statement(Stmts, I) when is_list(Stmts) -> %% allow list as compound?
    format_icompound(Stmts,I);
format_statement(Stmt, I) ->
    [indent(I),
     case Stmt of
	 #bic_for{init=Init,test=Test,update=Update,body=Body} ->
	     ["for ", "(", 
	      format_expr(Init), ";", 
	      format_expr(Test), ";",
	      format_expr(Update), ") ",
	      format_body(Body,I+1)];
	 #bic_while{test=Test,body=Body} ->
	     ["while ", "(", 
	      format_expr(Test), ") ",
	      format_body(Body,I+1)];
	 #bic_do{body=Body, test=Test} ->
	     ["do ", format_body(Body,I+1),
	      "(", format_expr(Test), ")", ";\n" ];
	 #bic_if{test=Test,then=Then,else=undefined} ->
	     ["if ", "(", format_expr(Test), ")",
	      format_body(Then,I+1)];
	 #bic_if{test=Test,then=Then,else=Else} ->
	     ["if ", "(", format_expr(Test), ")",
	      format_body(Then,I+1),
	      " else ", 
	      format_body(Else,I+1)];
	 #bic_switch{expr=Expr,body=Body} ->
	     ["switch ", "(", format_expr(Expr), ")",
	      format_statement(Body,I+1)];
	 #bic_case{expr=Expr, code=Code} ->
	     ["case ", format_expr(Expr), ": ",
	      format_statement(Code,I+1)];
	 #bic_default{code=Code} ->
	     ["default", ": ",
	      format_statement(Code,I+1)];
	 #bic_label{name=Label, code=Code} ->
	     [Label, ": ", format_statement(Code,I+1)];
	 #bic_continue{} ->
	     ["continue", ";\n"];
	 #bic_break{} ->
	     ["break", ";\n"];
	 #bic_return{expr=undefined} ->
	     ["return", ";\n"];
	 #bic_return{expr=Expr} ->
	     ["return", " ", format_expr(Expr), ";\n"];
	 #bic_empty{} ->
	     [";\n"];
	 #bic_decl{name=Name,type=Type,size=Size,value=Init} ->
	     [format_type(Type)," ",Name,format_dim(Type),
	      format_size(Size),format_init(Init),";\n"];
	 Expr ->
	     [format_expr(Expr), ";\n"]
     end].

format_body(#bic_compound{code=Stmts}, I) when is_list(Stmts) ->
    format_compound(Stmts, I);
format_body(Stmts, I) when is_list(Stmts) ->
    format_compound(Stmts, I);
format_body(Stmt, I) ->
    ["\n", format_statement(Stmt,I)].

%% compound body for for/while/...
format_compound(Stmts, I) when is_list(Stmts) ->
    ["{","\n",
     format_statements(Stmts, I+1),
     indent(I-1),"}\n"].

%% compound body for block code, functions ...
format_icompound(Stmts, I) when is_list(Stmts) ->
    [indent(I),"{","\n",
     format_statements(Stmts, I+1),
     indent(I),"}\n"].

format_statements([], _I) ->    
    [];
format_statements([Stmt], I) ->    
    [format_statement(Stmt, I)];
format_statements([Stmt|Stmts], I) ->
    [format_statement(Stmt, I) |
     format_statements(Stmts, I)].

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

%% fixme: handle suffix
token_to_float(String) ->
    {list_to_float(string:trim(String,trailing,"fFlL")),
     #bic_type { const=true, type=double }}.

%% return char/wchar_t
token_to_char([$',Val,$']) ->
    {Val, #bic_type{ const=true, type=char} }.

%% fixme: check for utf8 interpret escape seqeunces
token_to_string(Token) ->
    {Token, #bic_type{ const=true, type=char}}.
