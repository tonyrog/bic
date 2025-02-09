%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    Format bic definitions / statements / expression and types
%%% @end
%%% Created :  8 Dec 2020 by Tony Rogvall <tony@rogvall.se>

-module(bic_format).

-export([definition/1]).
-export([definitions/1]).
-export([statement/1, statement/2]).
-export([statements/2]).
-export([decl/1]).
-export([type/1]).
-export([expr/1]).
-export([exprs/1]).

-include("../include/bic.hrl").

-spec definitions(bic_definitions()) -> 
	  iolist().

definitions([]) ->
    [];
definitions([D|Ds]) ->
    [definition(D) |
     definitions(Ds)];
definitions(D) ->
    definition(D).

-spec definition(bic_definition()) -> 
	  iolist().

definition(D) ->
    case D of
	#bic_function{name=Name,storage=Storage,type=Type,
		      params=Params,body=Body} ->
	    [storage(Storage),type(Type),dim(Type)," ",Name,
	     "(",params(Params),")", "\n",
	     function_body(Body)];
	#bic_typedef{name=Name,storage=Storage,type=Type,size=Size,value=_Init} ->
	    ["typedef ", storage(Storage),type(Type),
	     field_size(Size)," ",Name,dim(Type),";\n"];
	#bic_decl{} ->
	    [decl(D),";\n"]
    end.

-spec type(undefined|'_'|bic_expr()) -> iolist().

type(undefined) -> "N";
type('_') -> "_";
type(#bic_typeid{name=Name}) -> Name;
type(#bic_type{sign=S,const=C,volatile=V,size=Z,type=T}) ->
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
	   #bic_struct{} -> type(T);
	   #bic_union{} -> type(T);
	   #bic_typeid{name=Name} -> [Name];
	   #bic_enum{} -> type(T);
	   #bic_pointer{} -> ["(",type(T),")"]
       end]);
type(#bic_enum{name=Name,elems=Es}) ->
    ["enum ",optional(Name)," {", enums(Es), "}"];
%% fixme: pointer to function pointer?
type(#bic_pointer{type=Type=#bic_fn{}}) -> 
    ["(*)",type(Type)];
type(#bic_pointer{type=Type}) ->
    [type(Type),"*"];
type(#bic_array{type=Type,dim=_D}) ->
    [type(Type)];
type(#bic_fn{type=Type,params=Ps}) ->
    [type(Type),"(",params(Ps),")"];
type(#bic_struct{name=Name,elems=undefined}) ->
    ["struct ",optional(Name)];
type(#bic_struct{name=Name,elems=Es}) ->
    ["struct ",optional(Name),"{\n", decls(Es,"  ","\n"),"}"];
type(#bic_union{name=Name,elems=undefined}) ->
    ["union ",optional(Name)];
type(#bic_union{name=Name,elems=Es}) ->
    ["union ",optional(Name),"{", decls(Es,"  ","\n"),"}"].

dim(#bic_type{type=T}) when not is_atom(T) ->
    dim(T);
dim(#bic_array{type=T,dim=[]}) ->
    [dim(T), "[]"];
dim(#bic_array{type=T,dim=D}) ->
    [dim(T),["[",expr(D),"]"]];
dim(_) ->
    [].

optional(undefined) -> "";
optional(String) when is_list(String) -> String.

%% join strings but remove empty elements first
lists_join_no_empty(Sep,Es) ->
    lists:join(Sep, [A || A <- Es, A =/= ""]).

-type enum() :: {Name::string(),Line::integer(),Value::bic_expr()}.
-spec enum(E::enum()) -> string().

enum({Name,_Ln,Value}) ->
    [Name,"=",expr(Value)].

-spec enums(Enums::[enum()]) -> string().

enums([]) -> [];
enums([X]) -> [enum(X)];
enums([X|Xs]) -> [enum(X),",",enums(Xs)].

-spec decl(Decl::bic_decl()) -> string().

decl(#bic_decl{name=Name,storage=Storage,type=Type=#bic_fn{}}) ->
    func_decl(Name,Storage,Type);
decl(#bic_decl{name=undefined,storage=Storage,type=Type,size=Size,value=Init}) ->
    [storage(Storage),type(Type),dim(Type),field_size(Size),init(Init)];
decl(#bic_decl{name=Name,storage=Storage,type=Type,size=Size,value=Init}) ->
    [storage(Storage),type(Type)," ",Name,dim(Type),
     field_size(Size),init(Init)].

func_decl(undefined,Storage,#bic_fn{type=Type,params=Ps}) ->
    [storage(Storage),type(Type)," (*)", "(", params(Ps), ")"];
func_decl(Name,Storage,#bic_fn{type=Type,params=Ps}) ->
    [storage(Storage),type(Type)," ",Name, "(", params(Ps), ")"].

field_size(undefined) -> "";
field_size(Size) -> [":",expr(Size)].

init(undefined) -> "";
init(Init) -> ["=",expr(Init)].
    
storage(undefined) -> "";
storage(Storage) -> [atom_to_list(Storage)," "].

-spec decls(Decls::[bic_decl()], Prefix::string(), Sep::string()) -> string().

decls([],_Pref,_Sep) -> [];
decls([X|Xs],Pref,Sep) -> [Pref,decl(X),";",Sep,decls(Xs,Pref,Sep)].

-spec params(Decls::[bic_decl()]) -> string().

params([]) -> [];
params([X]) -> [decl(X)];
params([X|Xs]) -> [decl(X),",",params(Xs)].

-spec prio(Expr::bic_expr()|number()) -> integer();
	  (Other::term()) -> false.

%% check priority of expression
prio(#bic_unary{op=Op}) ->
    case Op of
	sizeof -> 0;
	typeof -> 0;
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
	cast -> 0;
	'[]' -> 0;
	'.'  -> 0;
	'->'  -> 0;
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

-spec expr(Expr::bic_expr()) -> string().

expr(X) when is_integer(X) -> integer_to_list(X);
expr(X) when is_float(X) -> io_lib_format:fwrite_g(X);
expr(#bic_id{name=Name}) -> Name;
expr(#bic_constant{token=Value}) -> Value;

expr(#bic_unary{op='+++',arg=Arg}) -> 
    [expr(Arg),"++"];
expr(#bic_unary{op='---',arg=Arg}) -> 
    [expr(Arg),"--"];
expr(#bic_unary{op=sizeof,arg=Arg}) -> 
    %% fixme: Arg is either expression or type!
    ["sizeof(", 
     case bic:is_expr(Arg) of
	 true -> expr(Arg);
	 false -> type(Arg)
     end, ")"];
expr(#bic_unary{op=Op,arg=Arg}) -> 
    [atom_to_list(Op),expr(Arg)];
expr(#bic_binary{op='[]',arg1=Arg1,arg2=Arg2}) -> 
    [expr(Arg1),"[",expr(Arg2),"]"];
expr(#bic_binary{op=cast,arg1=Type,arg2=Arg2}) -> 
    ["(",type(Type),")",expr(Arg2)];    
expr(X=#bic_binary{op=Op,arg1=Arg1,arg2=Arg2}) -> 
    P  = prio(X),
    P1 = prio(Arg1),
    P2 = prio(Arg2),
    %% io:format("~w (~w ~s ~w)\n", [P,P1,Op,P2]),
    %% io:format("(arg1=~p op=~s arg2=~p)\n", [Arg1, Op, Arg2]),
    [if is_number(P),is_number(P1),P1 > P -> 
	     ["(",expr(Arg1), ")"];
	true -> expr(Arg1)
     end,
     atom_to_list(Op),
     if is_number(P),is_number(P2),P2 > P ->
	     ["(",expr(Arg2), ")"];
	true ->
	     expr(Arg2)
     end];
expr(#bic_call{func=F, args=Exprs}) ->
    [expr(F),"(",exprs(Exprs),")"];
expr(#bic_ifexpr{test=C,then=T,'else'=E}) ->
    [expr(C),"?",expr(T),":",expr(E)];
expr(#bic_assign{op=Op,lhs=L,rhs=R}) ->
    [expr(L),atom_to_list(Op),expr(R)];
expr(Array) when is_list(Array) -> %% array init?
    ["{", exprs(Array), "}"].

-spec exprs(ExprList::[bic_expr()]) -> string().

exprs([]) -> [];
exprs([X]) -> [expr(X)];
exprs([X|Xs]) -> [expr(X),",",exprs(Xs)].


indent(I) when I =< 0 -> 
    "";
indent(I) ->
    lists:duplicate(2*I, $\s).

statement(Stmt) ->
    statement(Stmt, 0).

statement(undefined, _I) -> 
    "";
statement(#bic_compound{code=Stmts}, I) when is_list(Stmts) ->
    [indent(I),"{","\n",
     statements(Stmts, I+1),
     indent(I),"}\n"];
statement(#bic_label{name=Label, code=Code}, I) ->
    [indent(0),Label, ": ","\n", statement(Code,I+1)];
statement(Stmt, I) ->
    [indent(I),
     case Stmt of
	 #bic_for{init=Init,test=Test,update=Update,body=Body} ->
	     ["for ", "(", 
	      expr(Init), ";", 
	      expr(Test), ";",
	      expr(Update), ") ",
	      body(Body,I,0,"\n")];
	 #bic_while{test=Test,body=Body} ->
	     ["while ", "(", expr(Test), ") ",
	      body(Body,I,0,"\n")];
	 #bic_do{body=Body, test=Test} ->
	     ["do ", body(Body,I,0,""),
	      " while (", expr(Test), ")", ";\n" ];
	 #bic_if{test=Test,then=Then,'else'=undefined} ->
	     ["if ", "(", expr(Test), ") ",
	      body(Then,I+1,-1,"\n")];
	 #bic_if{test=Test,then=Then,'else'=Else} ->
	     ["if ", "(", expr(Test), ") ",
	      body(Then,I+1,-1,"\n"),
	      [indent(I),"else "], 
	      body(Else,I+1,-1,"\n")];
	 #bic_switch{expr=Expr,body=Body} ->
	     ["switch ", "(", expr(Expr), ") ",
	      body(Body,I-1,1,"\n")];
	 #bic_case{expr=Expr, code=Code} ->
	     ["case ", expr(Expr), ":", "\n", statement(Code,I+1)];
	 #bic_default{code=Code} ->
	     ["default", ": ", "\n", statement(Code,I+1)];
	 %% #bic_label{name=Label, code=Code} ->
	 %%  [Label, ": ","\n", statement(Code,I+1)];
	 #bic_continue{} ->
	     [indent(1),"continue", ";\n"];
	 #bic_break{} ->
	     [indent(1),"break", ";\n"];
	 #bic_return{expr=undefined} ->
	     ["return", ";\n"];
	 #bic_return{expr=Expr} ->
	     ["return", " ", expr(Expr), ";\n"];
	 #bic_goto{label=Label} ->
	     ["goto", " ", expr(Label), ";\n"];
	 #bic_empty{} ->
	     [";\n"];
	 #bic_decl{} ->
	     [decl(Stmt),";\n"];
	 #bic_expr_stmt{expr=Expr} ->
	     [expr(Expr), ";\n"]
     end].

body(#bic_compound{code=Stmts}, I, IE, NL) when is_list(Stmts) ->
    ["{","\n",
     statements(Stmts, I+1),
     indent(I+IE),"}", NL];
body(Stmt, I, _IE, _NL) ->
    ["\n", statement(Stmt,I+1)].

function_body(Stmts) when is_list(Stmts) ->
    ["{","\n",
     statements(Stmts, 1),
     "}\n"].

statements([], _I) ->    
    [];
statements([Stmt], I) ->    
    [statement(Stmt, I)];
statements([Stmt|Stmts], I) ->
    [statement(Stmt, I) |
     statements(Stmts, I)].
