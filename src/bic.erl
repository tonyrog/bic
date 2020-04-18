%% File    : bic.erl
%%% Author  : Tony Rogvall <tony@iMac.local>
%%% Description : BEAM interpreted C (ode)
%%% Created : 27 Dec 2005 by Tony Rogvall <tony@iMac.local>

-module(bic).

-export([file/1, file/2]).
-export([string/1, string/2]).
-export([parse/1, parse/2]).
-export([print/1, print/2]).

-export([format_type/1]).
-export([format_expr/1]).
-export([format_exprs/1]).
-export([format_decl/1]).
-export([format_statement/2]).
-export([format_statements/2]).
-export([format_definitions/1]).
-export([combine_types/2]).
-export([complete_type/1]).
-export([constant_to_integer/2]).
-export([constant_to_float/1]).

-compile(export_all).

-import(lists, [map/2]).

-include("../include/bic.hrl").

%% -define(dbg(F,A), io:format((F),(A))).
-define(dbg(F,A), ok).

-type cpp_option() :: {define,Name::string(),Value::term()} |
		      {include, Path::string()} |
		      {qinclude, Path::string()}.

-type cpp_options() :: [cpp_option()].

-type bic() :: [#bic_function{} | #bic_decl{}].

command() ->
    io:format("bic: no input files\n"),
    halt(1).

command([]) ->
    io:format("bic: no input files\n"),
    halt(1);
command([Filename]) when is_atom(Filename) ->
    case file(atom_to_list(Filename)) of
	{ok,List} ->
	    io:format("~p\n", [List]),
	    halt(0);
	_Err ->
	    halt(1)
    end.

string(String) ->
    string(String, []).

-spec string(String::string(), Env::cpp_options()) ->
		    {ok, bic()} | {error, Reason::term()}.

string(String, Env) ->
    Env1 = [{define,"__bic__", 1},
	    {define,"__bic_extension_bitfield__", 1} | Env],
    case bic_cpp:string(String, Env1) of
	{ok,Fd} ->
	    bic_scan:init(),  %% setup some dictionay stuff
	    bic_parse:init(), %% setup some dictionay stuff
	    Res = (catch bic_parse:parse_and_scan({bic, scan, [Fd]})),
	    %% maybe_dump_cpp_variables(Fd),
	    bic_cpp:close(Fd),
	    case Res of 
		{error,{{Fn,Ln},Mod,Message}} when is_integer(Ln) ->
		    io:format("Message: ~w\n", [Message]),
		    io:format("~s:~w: ~s\n",
			      [Fn,Ln,Mod:format_error(Message)]),
		    {error,parse_error};
		{error,{Ln,Mod,Message}} when is_integer(Ln) ->
		    io:format("Message: ~w\n", [Message]),
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
    file(File,[]).

-spec file(Filename::string(), Env::cpp_options()) ->
		  {ok, bic()} | {error, Reason::term()}.

file(Filename,Env) ->
    case parse(Filename, Env) of
	{ok,Forms} ->
	    case bic_lint:forms(Filename,Forms) of
		{ok,_LintForms} ->
		    {ok,Forms};
		Err={error,_} ->
		    Err
	    end;
	Error ->
	    Error
    end.

print(Filename) ->
    print(Filename,[]).

print(Filename, Env) ->
    case file(Filename, Env) of
	{ok,Forms} ->
	    io:put_chars(format_definitions(Forms));
	Error ->
	    Error
    end.

-spec parse(Filename::string(), Env::cpp_options()) ->
		   {ok, bic()} | {error, Reason::term()}.
%% parse file, not lint
parse(File) ->
    parse(File, []).

parse(Filename, Env) ->
    Env1 = [{define,"__bic__", 1},
	    {define,"__bic_extension_bitfield__", 1} | Env],
    case bic_cpp:open(Filename, Env1) of
	{ok,Fd} ->
	    bic_scan:init(),  %% setup some dictionay stuff
	    bic_parse:init(), %% setup some dictionay stuff
	    Res = (catch bic_parse:parse_and_scan({bic, scan, [Fd]})),
	    %% maybe_dump_cpp_variables(Fd),
	    bic_cpp:close(Fd),
	    case Res of 
		{error,{{Fn,Ln},Mod,Message}} when is_integer(Ln) ->
		    io:format("Message: ~w\n", [Message]),
		    io:format("~s:~w: ~s\n",
			      [Fn,Ln,Mod:format_error(Message)]),
		    {error,parse_error};
		{error,{Ln,Mod,Message}} when is_integer(Ln) ->
		    io:format("Message: ~w\n", [Message]),
		    io:format("~s:~w: ~s\n",
			      [Filename,Ln,Mod:format_error(Message)]),
		    {error,parse_error};
		{ok,Forms} ->
		    {ok,Forms}
	    end
    end.
    

maybe_dump_cpp_variables(Fd) ->
    io:format("Variable dump:\n"),
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
    [if C -> "const "; true -> "" end,
     if V -> "volatile "; true -> "" end,
     case S of
	 undefined -> "";
	 signed -> "signed ";
	 unsigned -> "unsigned "
     end,
     case Z of
	 undefined -> "";
	 short-> "short ";
	 long -> "long ";
	 long_long -> "long long "
     end,
     case T of
	 undefined when S =/= undefined; Z =/= undefined -> "";
	 undefined -> "U";
	 '_' -> "T";
	 void -> "void ";
	 char -> "char";
	 int -> "int";
	 float -> "float";
	 double -> "double";
	 #bic_struct{} -> format_type(T);
	 #bic_union{} -> format_type(T);
	 #bic_typeid{name=Name} -> [Name];
	 #bic_enum{} -> format_type(T);
	 #bic_pointer{} -> ["(",format_type(T),")"]
     end];
format_type(#bic_enum{name=Name,elems=Es}) ->
    ["enum ",optional(Name)," {", format_enums(Es), "}"];

%% fixme: pointer to function pointer?
format_type(#bic_pointer{type=Type=#bic_fn{}}) -> 
    ["(*)",format_type(Type)];

format_type(#bic_pointer{type=Type}) ->
    [format_type(Type),"*"];
format_type(#bic_array{type=Type,dim=D}) ->
    if D =:= [] ->
	    [format_type(Type),"[]"];
       true ->
	    [format_type(Type),"[",format_expr(D),"]"]
    end;
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

optional(undefined) -> "";
optional(String) when is_list(String) -> String.
    

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
    [format_storage(Storage),format_type(Type),format_size(Size),format_init(Init)];
format_decl(#bic_decl{name=Name, storage=Storage,type=Type,size=Size,value=Init}) ->
    [format_storage(Storage),format_type(Type)," ",Name,format_size(Size),format_init(Init)].

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

-spec format_expr(Expr::bic_expr()) -> string().

format_expr(#bic_id{name=Name}) -> Name;
format_expr(#bic_constant{value=Value}) -> Value;

format_expr(#bic_unary{op='+++',arg=Arg}) -> 
    [format_expr(Arg),"++"];
format_expr(#bic_unary{op='---',arg=Arg}) -> 
    [format_expr(Arg),"--"];
format_expr(#bic_unary{op=Op,arg=Arg}) -> 
    [atom_to_list(Op),format_expr(Arg)];
format_expr(#bic_binary{op='[]',arg1=Arg1,arg2=Arg2}) -> 
    [format_expr(Arg1),"[",format_expr(Arg2),"]"];    
format_expr(#bic_binary{op=Op,arg1=Arg1,arg2=Arg2}) -> 
    [format_expr(Arg1),atom_to_list(Op),format_expr(Arg2)];
format_expr(#bic_call{func=F, args=Exprs}) ->
    [format_expr(F),"(",format_exprs(Exprs),")"];
format_expr(#bic_ifexpr{test=C,then=T,else=E}) ->
    [format_expr(C),"?",format_expr(T),":",format_expr(E)];
format_expr(#bic_assign{op=Op,lhs=L,rhs=R}) ->
    [format_expr(L),atom_to_list(Op),format_expr(R)];
format_expr(Array) when is_list(Array) -> %% init?
    ["{", format_exprs(Array), "}"].

-spec format_exprs(ExprList::[bic_expr()]) -> string().

format_exprs([]) -> [];
format_exprs([X]) -> [format_expr(X)];
format_exprs([X|Xs]) -> [format_expr(X),",",format_exprs(Xs)].

format_definitions([]) ->
    [];
format_definitions([D|Ds]) ->
    [case D of
	#bic_function{name=Name,storage=Storage,type=Type,params=Params,body=Body} ->
	     [format_storage(Storage),format_type(Type)," ",Name,
	      "(",format_params(Params),")", "\n",
	      format_statement(Body, 0)];
	 #bic_typedef{name=Name,storage=Storage,type=Type,size=Size,value=_Init} ->
	     ["typedef ", format_storage(Storage),format_type(Type),format_size(Size),
	      " ",Name,";\n"];
	 #bic_decl{} ->
	     [format_decl(D),";\n"]
     end | format_definitions(Ds)].

indent(I) when I =< 0 -> 
    "";
indent(I) ->
    lists:duplicate(2*I, $\s).

format_statement(Stmts, I) when is_list(Stmts) ->
    ["{","\n",
     format_statements(Stmts, I+1),
     indent(I-1),"}\n"];
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
	     [format_type(Type)," ",Name,format_size(Size),format_init(Init),";\n"];
	 Expr ->
	     [format_expr(Expr), ";\n"]
     end].

format_body(Stmts, I) when is_list(Stmts) ->
    ["{","\n",
     format_statements(Stmts, I+1),
     indent(I-1),"}\n"];
format_body(Stmt, I) ->
    ["\n", format_statement(Stmt,I)].

format_statements([], _I) ->    
    [];
format_statements([Stmt], I) ->    
    [format_statement(Stmt, I)];
format_statements([Stmt|Stmts], I) ->
    [format_statement(Stmt, I) |
     format_statements(Stmts, I)].

%% convert constants
constant_to_integer(String, Base) ->
    list_to_integer(string:trim(String,trailing,"uUlL"), Base).

constant_to_float(String) ->
    list_to_float(string:trim(String,trailing,"fFlL")).


