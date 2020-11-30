%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%    BIC linter 
%%% @end
%%% Created : 14 Sep 2013 by Tony Rogvall <tony@rogvall.se>

-module(bic_lint).

-compile(export_all).

-include("../include/bic.hrl").

-record(cconst,    { line, value, type }).
-record(cvar,      { line, id, type }).
-record(cbinary,   { line, op, type, arg1, arg2 }).
-record(cunary,    { line, op, type, arg }).
-record(ccall,     { line, func, type, args }).
-record(cassign,   { line, op, type, lhs, rhs }).
-record(cifexpr,   { line, test, type, then, else}).

-type cexpr() :: #cassign{} | #cifexpr{} | #cbinary{} | 
		 #cunary{} | #ccall{} | #cconst{}.
-type cdecl() :: any(). %% forward - #cdecl{}.

-record(cstruct,
	{
	  line,
	  name   = undefined :: undefined | string(), %% optional name
	  elems  = [] :: [cdecl()]       %% declarations
	}).

-record(cunion,
	{
	  line,
	  name   = undefined :: undefined | string(), %% optional name
	  elems  = [] :: [cdecl()]       %% declarations
	}).

-record(cenum,
	{
	  line,
	  name   = undefined :: undefined | string(), %% optional name
	  elems  = [] :: [{string(),integer()}]       %% value
	}).

-record(cfunction,
	{
	  line,
	  name = undefined :: undefined | string(), %% optional name
	  return,
	  args,
	  body
	}).

-record(cempty,    {line}).
-record(cbreak,    {line}).
-record(ccontinue, {line}).
-record(cdefault,  {line}).
-record(creturn, {line,expr}).
-record(cfor,     {line,init,test,update,body}).
-record(cwhile,   {line,test,body}).
-record(cdo,      {line,body,test}).
-record(clabel,	  {line,name,code}).
-record(cgoto,    {line,label}).
-record(ccase,    {line, expr, code}).
-record(cswitch,  {line, expr, body}).
-record(cif, {line, test,then,else}).

-record(ctype,
	{
	 line       = 0 :: integer(), %% where type was first defined!
	 sign       = default :: default | signed | unsigned,
	 const      = default :: default | boolean(),
	 volatile   = default :: default | boolean(),
	 size       = default :: default | none | short | long | long_long,
	 type       = default :: default | void | char | int | float | double
	}).

-record(cdecl,
       {
	line,
	name = undefined :: undefined | string(),
	type = undefined :: undefined | #ctype{},
	size = undefined :: undefined | integer(),
	value = undefined :: undefined | cexpr()
       }).

-record(ctypedef,
	{
	 line,
	 name = undefined :: undefined | string(),
	 type = undefined :: undefined | #ctype{},
	 size = undefined :: undefined | integer(),
	 value = undefined :: undefined | cexpr()
	}).

-type scopetype() :: compound | for | while | do | switch.

-type typemap() :: #{ string() => #ctypedef{}}.
-type declmap() :: #{ string() => #cdecl{}}.

-record(scope,
	{
	 filename = "" :: string(),  %% filename 
	 type     = [] :: [scopetype()],
	 typedefs = [] :: [typemap()], %% stack of typedefs
	 decls    = [] :: [declmap()], %% stack of declarations
	 labels   = [] :: [string()],
	 errors   = [],
	 warnings = []
	}).

forms(Filename,Forms) ->
    %% io:format("Forms:\n~p\n", [Forms]),
    {CForms,Scope} = forms_(Forms, [], new_scope(Filename)),
    if Scope#scope.errors =:= [] ->
	    report(Scope#scope.filename, Scope#scope.warnings),
	    {ok,CForms};
       true ->
	    report(Scope#scope.filename,
		   Scope#scope.warnings++Scope#scope.errors),
	    {error, lint}
    end.

forms_(undefined, _Acc, S) ->
    {undefined, S};
forms_(Stmt, _Acc, S) when is_tuple(Stmt) ->
    statement(Stmt, S);
forms_([F=#bic_typedef { line=Ln, name=Name } | Fs], Acc, S0) ->
    {CTypedef,S1} = typedef(F,S0),
    case find_local_type(Name, S1) of
	error ->
	    S2 = store_type(Name, CTypedef, S1),
	    forms_(Fs, [CTypedef|Acc], S2);
	{ok,_} ->
	    S2 = add_warning(S1, Ln, "type ~s already defined", [Name]),
	    S3 = store_type(Name, CTypedef, S2),
	    forms_(Fs, [CTypedef|Acc], S3)
    end;
forms_([D=#bic_decl{}| Fs], Acc, S0) ->
    {CDecl,S1} = decl(D, S0),
    forms_(Fs, [CDecl|Acc], S1);
forms_([F=#bic_struct{}|Fs], Acc, S0) ->
    {CStruct, S1} = struct(F, S0),
    forms_(Fs, [CStruct|Acc], S1);
forms_([F=#bic_union{}|Fs], Acc, S0) ->
    {CUnion, S1} = union(F, S0),
    forms_(Fs, [CUnion|Acc], S1);
forms_([F=#bic_enum{}|Fs], Acc, S0) ->
    {CEnum, S1} = enum(F, S0),
    forms_(Fs, [CEnum|Acc], S1);
forms_([F=#bic_function {line=Ln,name=Name} | Fs], Acc, S0) ->
    %% io:format("return = ~p\n", [F#bic_function.type]),
    {CType,S1} = type(F#bic_function.type,Ln,S0),
    S2 = push_scope(S1),
    {CArgs,S3} = decls(F#bic_function.params, S2),

    %% declare for recursive functions
    Func0 = #cfunction { line=Ln, name=Name, return = CType,
			 args = CArgs, body = undefined },
    CDecl0 = #cdecl { line = Ln, name = Name, type = CType,
		      value = Func0 },
    S40 = store_decl(Name, CDecl0, S3),

    %% {CParams,S3} = decls(F#bic_functions.params, S2),  %% merge?
    {CBody,S4} = forms_(F#bic_function.body, [], clear_labels(S40)),
    %% CBody1 = resolve_labels(CBody, S4)
    S5 = pop_scope(S4),
    Func = #cfunction { line=Ln, name=Name, return = CType,
			args = CArgs, body = CBody },
    CDecl = #cdecl { line = Ln, name = Name, type = CType,
		     value = Func },
    S6 = store_decl(Name, CDecl, S5),
    forms_(Fs, [Func | Acc], S6);
forms_([F | Fs], Acc, S) ->
    {CStmt, S1} = statement(F, S),
    forms_(Fs, [CStmt|Acc], S1);
forms_([], Acc, S) ->
    {lists:reverse(Acc), S}.

statement(#bic_empty{line=Ln}, S) ->
    {#cempty{line=Ln}, S};
statement(#bic_if { line=Ln,test=Test,then=Then,else=Else},S0) ->
    {CTest,S1} = expr(Test,S0),
    {CThen,S2} = forms_(Then,[],S1),
    {CElse,S3} = forms_(Else,[],S2),
    CIf = #cif { line=Ln,test=CTest, then=CThen, else=CElse },
    {CIf,S3};
statement(#bic_for { line=Ln,init=Init,test=Test,update=Update,body=Body},S0) ->
    {CInit,S1} = expr(Init,S0),
    {CTest,S2} = expr(Test,S1),
    {CUpdate,S3} = expr(Update,S2),
    {CBody,S4} = forms_(Body,[],push_scope(S3,for)),
    CFor = #cfor { line=Ln,init=CInit,test=CTest,update=CUpdate,body=CBody},
    {CFor,pop_scope(S4)};
statement(#bic_while { line=Ln, test=Test, body=Body },S0) ->
    {CTest,S1} = expr(Test,S0),
    {CBody,S2} = forms_(Body,[],push_scope(S1,while)),
    CWhile = #cwhile { line=Ln,test=CTest,body=CBody},
    {CWhile,pop_scope(S2)};
statement(#bic_do { line=Ln, body=Body, test=Test },S0) ->
    {CTest,S1} = expr(Test,S0),
    {CBody,S2} = forms_(Body,[],push_scope(S1,do)),
    CWhile = #cdo { line=Ln, body=CBody, test=CTest },
    {CWhile,pop_scope(S2)};
statement(#bic_switch { line=Ln, expr=Expr, body=Body },S0) ->
    {CExpr,S1} = expr(Expr,S0),
    {CBody,S2} = forms_(Body,[],push_scope(S1,switch)),
    CSwitch = #cswitch { line=Ln, expr=CExpr, body=CBody },
    {CSwitch,pop_scope(S2)};
statement(#bic_label { line=Ln, name = Name, code = Code }, S0) ->
    {CCode,S1} = forms_(Code,[],S0),
    S1 = add_label(Name, S0),
    CLabel = #clabel { line=Ln, name=Name, code=CCode },
    {CLabel, S1};
statement(#bic_case { line=Ln, expr = Expr, code = Code }, S0) ->
    {CExpr,S1} = expr(Expr,S0),
    {CCode,S2} = forms_(Code,[],S1),
    CCase = #ccase { line=Ln, expr=CExpr, code=CCode },
    {CCase, S2};
statement(#bic_goto { line=Ln, label=Name }, S0) ->
    %% FIXME: resolve goto's later, may have forward refs!
    CGoto = #cgoto { line=Ln, label=Name },
    {CGoto, S0};
statement(#bic_continue {line=Ln}, S0) ->
    S1 = case is_scope_type([for,while,do],S0) of
	     false ->
		 add_error(S0,Ln,"continue statement not within a loop", []);
	     true -> S0
	 end,
    CContinue = #ccontinue {line=Ln}, 
    {CContinue, S1};
statement(#bic_break {line=Ln}, S0) ->
    S1 = case is_scope_type([for,while,do,switch],S0) of
	     false -> 
		 add_error(S0,Ln,
			   "break statement not within loop or switch", []);
	     true -> S0
	 end,
    CBreak = #cbreak {line=Ln},
    {CBreak, S1};
statement(#bic_default {line=Ln}, S0) ->
    S1 = case is_scope_type([switch],S0) of
	     false -> 
		 add_error(S0, Ln,
			   "'default' label not within a switch statement",
			   []);
	     true -> S0
	 end,
    CDefault = #cdefault {line=Ln},
    {CDefault, S1};
statement(#bic_return { line=Ln,expr = Expr}, S0) ->
    {CExpr,S1} = expr(Expr,S0),
    %% type_check(CExpr, Function Return Type)
    CReturn = #creturn { line=Ln, expr = CExpr },
    {CReturn, S1};
statement(Expr, S0) when is_tuple(Expr) ->
    {CExpr,S1} = expr(Expr, S0),
    {CExpr,S1};
statement(StmtList, S0) when is_list(StmtList) ->
    {CList,S1} = statement_list(StmtList, [], push_scope(S0)),
    {CList,pop_scope(S1)};
statement([], S0) ->
    {[], S0}.

statement_list([Stmt|StmtList], Acc, S0) ->
    {CStmt, S1} = statement(Stmt, S0),
    statement_list(StmtList, [CStmt|Acc], S1);
statement_list([], Acc, S0) ->
    {lists:reverse(Acc), S0}.

report(Filename,Reports) ->
    [ begin io:format("~s:~w: "++Fmt++"\n", 
		      [Filename,Line|Args]) end || {Line,Fmt,Args} <- Reports ],
    ok.

typedef(D=#bic_typedef{line=Ln}, S0) ->
    {CType,S1} = type(D#bic_typedef.type,Ln,S0),
    {Size,S2}  = expr(D#bic_typedef.size, S1),
    {Value,S3} = expr(D#bic_typedef.value,S2),
    {#ctypedef { line=Ln,name=D#bic_typedef.name,
		 type=CType,size=Size,value=Value},S3}.

decl(D=#bic_decl{line=Ln}, S0) ->
    {Type,S1}  = type(D#bic_decl.type,Ln,S0),
    {Size,S2}  = expr(D#bic_decl.size, S1),
    {Value,S3} = expr(D#bic_decl.value,S2),
    Name = D#bic_decl.name,
    CDecl = #cdecl { line=Ln,name=Name, type=Type, size=Size, value=Value },
    S4 = if Name =:= undefined -> 
		 S3;
	    true ->
		 case find_local_decl(Name, S0) of
		     error ->
			 store_decl(Name, CDecl, S3);
		     {ok,_} ->
			 S31 = store_decl(Name, CDecl, S3),
			 add_error(S31,Ln,"~s already declared", [Name])
		 end
	 end,
    {CDecl,S4}.

decls([D|Ds], S0) ->
    {D1, S1} = decl(D, S0),
    {Ds1, S2} = decls(Ds, S1),
    {[D1|Ds1],S2};
decls([], S0) ->
    {[], S0};
decls(undefined, S0) ->
    {undefined, S0}.
	  
struct(X=#bic_struct{line=Ln},S0) ->
    {Elems,S00} = decls(X#bic_struct.elems, push_scope(S0)),
    S1 = pop_scope(S00),
    Name = X#bic_struct.name,
    CStruct = #cstruct { line=Ln, name=Name, elems=Elems},
    if Name =:= undefined ->
	    {CStruct, S1};
       true ->
	    case find_local_type({struct,Name}, S1) of
		error ->
		    S2 = store_type({struct,Name}, CStruct, S1),
		    {CStruct, S2};
		{ok,_} ->
		    S2 = add_warning(S1,Ln,"struct ~s already defined", [Name]),
		    S3 = store_type({struct,Name}, CStruct, S2),
		    {CStruct, S3}
	    end
    end.


union(X=#bic_union{line=Ln},S0) ->
    {Elems,S00} = decls(X#bic_union.elems, push_scope(S0)),
    S1 = pop_scope(S00),
    Name = X#bic_union.name,
    CUnion = #cunion { line=Ln,name=Name, elems=Elems},
    if Name =:= undefined ->
	    {CUnion, S1};
       true ->
	    case find_local_type({union,Name}, S1) of
		error ->
		    S2 = store_type({union,Name}, CUnion, S1),
		    {CUnion, S2};
		{ok,_} ->
		    S2 = add_warning(S1,Ln,"union ~s already defined", [Name]),
		    S3 = store_type({union,Name}, CUnion, S2),
		    {CUnion, S3}
	    end
    end.

enum(X=#bic_enum{line=Ln},S0) ->
    {Elems,S1} = enums(X#bic_enum.elems, S0),
    Name = X#bic_enum.name,
    CEnum = #cenum { line=Ln,name=Name, elems=Elems },
    S2 = store_enumerators(Elems, S1),  %% install enumerators
    if Name =:= undefined ->
	    {CEnum, S2};
       true ->
	    case find_local_type({enum,Name}, S2) of
		error ->
		    S3 = store_type({enum,Name}, CEnum, S2),
		    {CEnum, S3};
		{ok,_} ->
		    S3 = add_warning(S2,Ln,
				     "enum '~s' already defined", [Name]),
		    S4 = store_type({enum,Name}, CEnum, S3),
		    {CEnum, S4}
	    end
    end.

store_enumerators([{ID,Ln,Value}|Elems], S0) ->
    CType = #ctype { type = int, const = true },
    case find_decl(ID, S0) of
	error ->
	    S1 = store_decl(ID, #cdecl { line=Ln,name=ID,
					 type=CType,value=Value },S0),
	    store_enumerators(Elems, S1);
	{ok,_} ->
	    S1 = store_decl(ID, #cdecl { line=Ln,name=ID,
					 type=CType,value=Value },S0),
	    S2 = add_error(S1,Ln,"~s already declared",[ID]),
	    store_enumerators(Elems, S2)
    end;
store_enumerators([], S0) ->
    S0.

enums(Elems, S0) ->
    enums(Elems, [], S0).

enums([{ID,Ln,Value}|Elems], Acc, S0) ->
    {CValue,S1} = expr(Value, S0),
    case lists:keyfind(ID, 1, Acc) of
	false ->
	    enums(Elems, [{ID,Ln,CValue}|Acc], S0);
	_ ->
	    S1 = add_error(S0, Ln, "enum '~s' already defined", [ID]),
	    Acc1 = lists:keydelete(ID, 1, Acc),
	    enums(Elems, [{ID,Ln,CValue}|Acc1], S0)
    end;
enums([], Acc, S0) ->
    Acc1 = enumerate(lists:reverse(Acc), [], 0),
    {Acc1, S0}.

enumerate([{ID,Ln,undefined}|Enums], Acc, I) ->
    enumerate(Enums, [{ID,Ln,I}|Acc], I+1);
enumerate([E={ID,Ln,Value}|Enums], Acc, I) ->
    case Value of
	#cconst { value = Value1 } when is_integer(Value1) ->
	    enumerate(Enums, [{ID,Ln,Value1}|Acc], I);
	_ -> %% fixme eval
	    enumerate(Enums, [E|Acc], I)
    end;
enumerate([], Acc, _I) ->
    lists:reverse(Acc).

type(undefined,Ln,S) ->
    {#ctype{line=Ln}, S};
type(T,Ln,S) ->
    type_(T,Ln,S).

type_(#bic_pointer{type=T},Ln,S) -> 
    {T1,S1} = type_(T,Ln,S),
    {{pointer,T1},S1};
type_(#bic_array{type=T,dim=D},Ln,S) ->
    {T1,S1} = type_(T,Ln,S),
    {{array,T1,D},S1};
type_(#bic_fn{type=T,params=Ps},Ln,S) -> 
    {T1,S1} = type_(T,Ln,S),
    S2 = push_scope(S1),
    {Ps1,S3} = decls(Ps,S2),
    S4 = pop_scope(S3),
    {{fn,T1,Ps1},S4};

type_(T=#bic_typeid {name="..."},_Ln,S) ->
    {T,S};
type_(#bic_typeid { line=Ln,name=Name},_Ln,S) ->
    case find_type(Name, S) of
	error ->
	    S1 = add_error(S,Ln,"type '~s' not found", [Name]),
	    {#ctype { type=int }, S1};
	{ok,Typedef} ->
	    %% fixme: check interaction between T and T1
	    {Typedef#ctypedef.type,S}
    end;
type_(X=#bic_enum{},_Ln,S)  ->
    enum(X, S);
type_(X=#bic_struct{},_Ln,S) -> 
    struct(X, S);
type_(X=#bic_union{},_Ln,S) ->
    union(X, S);
type_(X=#bic_type{},Ln,S) ->
    %% FIXME: check combinations!
    {#ctype{line=Ln,
	    sign=X#bic_type.sign,
	    const=X#bic_type.const,
	    volatile=X#bic_type.volatile,
	    size=X#bic_type.size,
	    type=X#bic_type.type}, S}.

expr(undefined, S0) ->
    {undefined, S0};
expr(C=#bic_constant { }, S0) ->
    constant(C,S0);
expr(#bic_id { line=Ln, name=Name }, S0) ->
    case find_decl(Name, S0) of
	error ->
	    S1 = add_error(S0,Ln,"identifier '~s' not declared", [Name]),
	    { #cvar { line=Ln, id=Name, type=#ctype { type=int } }, S1 };
	{ok,CDecl} ->
	    %% io:format("decl of ~s = ~p\n", [Name, CDecl]),
	    CVar = #cvar { line=Ln, id=Name, type=CDecl#cdecl.type },
	    {CVar, S0}
    end;
expr(#bic_unary { line=Ln, op=Op, arg=Arg}, S0) ->
    {CArg,S1} = expr(Arg,S0),
    {CType,S2} = check_type(Ln,Op,typeof(CArg),S1),
    {#cunary { line=Ln, op=Op, type=CType, arg=CArg }, S2};
expr(#bic_binary { line=Ln, op=cast, arg1=Type, arg2=Arg2}, S0) ->
    {CArg2,S1} = expr(Arg2,S0),
    {#cbinary { line=Ln, op=cast,type=Type,arg1=Type,arg2=CArg2}, S1};
expr(#bic_binary { line=Ln, op=Op, arg1=Arg1, arg2=Arg2}, S0) ->
    {CArg1,S1} = expr(Arg1,S0),
    {CArg2,S2} = expr(Arg2,S1),
    {CType,S3} = check_type(Ln,Op,typeof(CArg1),typeof(CArg2),S2),
    {#cbinary { line=Ln, op=Op,type=CType,arg1=CArg1,arg2=CArg2}, S3};
expr(#bic_call { line=Ln, func=Func, args=Args }, S0) ->
    {CFunc, S1} = expr(Func, S0),
    {CArgs, S2} = expr(Args, S1),
    %% check actal args with formal arguments
    {CType,S3} = check_type(Ln, call, typeof(CFunc), typeof(CArgs), S2),
    {#ccall { line=Ln, func = CFunc, type=CType, args = CArgs }, S3};
expr(#bic_assign { line=Ln, op=Op, lhs=Lhs, rhs=Rhs}, S0) ->
    {CLhs,S1} = expr(Lhs,S0), %% lhs!!
    {CRhs,S2} = expr(Rhs,S1),
    {CType,S3} = check_assign(Ln, Op, typeof(CLhs), typeof(CRhs), S2),
    {#cassign { line=Ln, op=Op, type=CType, lhs=CLhs, rhs=CRhs}, S3};
expr(#bic_ifexpr { line=Ln, test=Test, then=Then, else=Else}, S0) ->
    {CTest,S1} = expr(Test,S0),
    {CThen,S2} = expr(Then,S1),
    {CElse,S3} = expr(Else,S2),
    %% CThen and CElse must be compatible
    %% {CType,S3} = check_type(Op,CTest,CElse,S3),
    {CType,S4} = check_type(Ln,'==',CThen,CElse,S3),
    {#cifexpr { line=Ln, test=CTest, type=CType, then=CThen, else=CElse}, S4};
expr([X | Xs], S0) ->
    {CX,S1}  = expr(X,S0),
    {CXs,S2} = expr(Xs,S1),
    {[CX|CXs], S2};
expr([], S0) ->
    {[], S0}.

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

check_type(Ln, Op, T, S) ->
    if Op =:= '~'; Op =:= '!' ->
	    case is_int_type(T) of
		true -> {T#ctype{line=Ln}, S};
		false ->
		    %% coerce pointer to integer
		    S1 = add_error(S,Ln,"operator ~s expect integer arguments",
				   [Op]),
		    {#ctype{line=Ln,type=int}, S1}
	    end;
       Op =:= '&' ->
	    {{pointer,T#ctype{line=Ln}}, S};
       Op =:= '*' ->
	    case T of
		{pointer,Type} ->
		    {Type, S};  %% fixme set line number of Type
		_ ->
		    S1 = add_error(S,Ln,"argument is not a pointer type",
				   []),
		    {#ctype{line=Ln,type=int}, S1}
	    end;
       Op =:= '-'; Op =:= '+' ->
	    case is_number_type(T) of
		true ->
		    {T#ctype{line=Ln},S};
		_ ->
		    S1 = add_error(S,Ln,"operator ~s expect scalar arguments",
				   [Op]),
		    {T#ctype{line=Ln}, S1}
	    end;
       Op =:= '++'; Op =:= '--'; Op =:= '+++'; Op =:= '---' ->
	    case is_pointer_type(T) of
		true ->
		    {T,S};
		false ->
		    case is_int_type(T) of
			true ->
			    {T#ctype{line=Ln},S};
			false ->
			    S1 = add_error(S,Ln,"operator ~s expect scalar arguments",
					   [Op]),
			    {T#ctype{line=Ln}, S1}
		    end
	    end;
       true ->
	    S1 = add_error(S,Ln,"operator ~s not expected",[Op]),
	    io:format("T = ~w, S1 = ~w\n", [T, S1]),
	    {T#ctype{line=Ln}, S1}
    end.

check_type(Ln, '[]', T1, T2, S) ->
    %% io:format("check_type ~p ~w ~w\n", ['[]',T1,T2]),
    case is_int_type(T2) of
	false ->
	    S1 = add_error(S,Ln,"index expression is not integer type",[]),
	    {base_type(T1), S1};
	true ->
	    case is_address_type(T1) of
		true ->
		    {base_type(T1), S};
		false ->
		    S1 = add_error(S,Ln,"subscript is not an array",[]),
		    {#ctype{line=Ln,type=int}, S1}
	    end
    end;
check_type(Ln, '+', T1, T2, S) ->
    %% io:format("check_type ~p ~w ~w\n", ['+',T1,T2]),
    case is_number_type(T1) andalso is_number_type(T2) of
	true ->
	    {coerce(T1,T2), S};
	false ->
	    case (is_pointer_type(T1) andalso is_int_type(T2)) of
		true -> {T1,S};
		false  ->
		case (is_pointer_type(T2) andalso is_int_type(T1)) of
		    true -> {T2,S};
		    false ->
			S1 = add_error(S,Ln,"bad argument to '+' operator",[]),
			{T1, S1}
		end
	    end
    end;
check_type(_Ln, _Op, T1, _T2, S) ->
    %% io:format("check_type ~p ~w ~w\n", [_Op,T1,_T2]),
    {T1, S}.

coerce(T1,_T2) ->
    T1.

check_assign(_Ln, _Op, Lhs, _Rhs, S0) ->
    {Lhs, S0}.

is_int_type(Ct=#ctype{}) ->
    case Ct#ctype.type of
	char -> true;
	int -> true;
	_ -> false
    end;
is_int_type(#cenum{}) -> true;
is_int_type(_) -> false.

is_number_type(#ctype{}) -> true;
is_number_type(#cenum{}) -> true;
is_number_type(_) -> false.

base_type({pointer,Type}) -> Type;
base_type({array,Type,_Dim}) -> Type.

is_address_type(Type) ->
    is_array_type(Type) orelse is_pointer_type(Type).

is_array_type({array,_,_}) -> true;
is_array_type(_) -> false.

is_pointer_type({pointer,_}) -> true;
is_pointer_type(_) -> false.

typeof(#cconst  {type=Type}) -> Type;
typeof(#cvar    {type=Type}) -> Type;
typeof(#cunary  {type=Type}) -> Type;
typeof(#cbinary {type=Type}) -> Type;
typeof(#ccall   {type=Type}) -> Type;
typeof(#cassign {type=Type}) -> Type;
typeof(#cifexpr {type=Type}) -> Type;
typeof([X|Xs]) -> [typeof(X) | typeof(Xs)];
typeof([]) -> [].


lineof(#cconst {line=Line}) -> Line;
lineof(#cvar {line=Line}) -> Line;
lineof(#cunary {line=Line}) -> Line;
lineof(#cbinary {line=Line}) -> Line;
lineof(#ccall {line=Line}) -> Line;
lineof(#cassign {line=Line}) -> Line;
lineof(#cifexpr {line=Line}) -> Line.


constant(#bic_constant { line=Ln, base=float, value=Val },S0) ->
    Const = #cconst { line=Ln, value=bic:constant_to_float(Val),
		      type=#ctype { const=true, type=double }},
    {Const, S0};
constant(#bic_constant { line=Ln, base=char, value=[$',Val,$'] },S0) ->
    Const = #cconst { line=Ln, value=Val,
		      type=#ctype { const=true, type=char }},
    {Const, S0};
constant(#bic_constant { line=Ln, base=string, value=Val },S0) ->
    Const = #cconst { line=Ln, value=Val,
		      type={array,#ctype{const=true,type=char},[]} },
    {Const, S0};
constant(#bic_constant { line=Ln, base=16, value=[$0,$x|Val] }, S0) ->
    Const = #cconst { line=Ln, value=bic:constant_to_integer(Val, 16),
		      type=#ctype { const=true, type=int }},
    {Const, S0};
constant(#bic_constant { line=Ln, base=B, value=Val },S0) when is_integer(B) ->
    Const = #cconst { line=Ln, value=bic:constant_to_integer(Val, B),
		      type=#ctype { const=true, type=int }},
    {Const, S0}.

add_warning(S, Ln, Fmt, Args) ->
    S#scope { warnings = [{Ln,Fmt,Args} | S#scope.warnings] }.

add_error(S, Ln, Fmt, Args) ->
    S#scope { errors = [{Ln,Fmt,Args} | S#scope.errors] }.


add_label(Name, S) ->
    S#scope { labels = [Name | S#scope.labels] }.

del_label(Name, S) ->
    S#scope { labels = lists:delete(Name, S#scope.labels) }.

%% start of a new function 
clear_labels(S) ->
    S#scope { labels = [] }.
    

new_scope(Filename) ->
    #scope { 
       filename = Filename,
       typedefs = [#{}],
       decls    = [#{}]
      }.

push_scope(S) ->
    push_scope(S, compound).

push_scope(S,Type) ->
    S#scope {
      type     = [Type | S#scope.type],
      typedefs = [#{} | S#scope.typedefs],
      decls    = [#{} | S#scope.decls]
     }.

pop_scope(S) ->
    S#scope {
      type     = tl(S#scope.type),
      typedefs = tl(S#scope.typedefs),
      decls    = tl(S#scope.decls)
     }.

%% check if any of the scope Types are present (for,do,while,switch)
is_scope_type(Types, S) ->
    is_scope_type_(Types, S#scope.type).

is_scope_type_(Types, [Type|Ts]) ->
    case lists:member(Type, Types) of
	true -> true;
	false -> is_scope_type_(Types, Ts)
    end;
is_scope_type_(_Types, []) ->
    false.

find_type(Name, S) ->
    find_in_dicts_(Name, S#scope.typedefs).

find_local_type(Name, S) ->
    maps:find(Name, hd(S#scope.typedefs)).

store_type(Name, Type, S) ->
    S#scope { typedefs = store_in_dicts_(Name, Type, S#scope.typedefs) }.

find_decl(Name, S) ->
    find_in_dicts_(Name, S#scope.decls).

find_local_decl(Name, S) ->
    maps:find(Name, hd(S#scope.decls)).

store_decl(Name, Decl, S) ->
    %% io:format("store ~p = ~p\n", [Name, Decl]),
    S#scope { decls = store_in_dicts_(Name, Decl, S#scope.decls) }.

erase_in_dicts_(Key, [D|Ds]) ->
    [maps:remove(Key, D) | Ds].

store_in_dicts_(Key, Value, [D|Ds]) ->
    [maps:put(Key, Value, D) | Ds].

find_in_dicts_(Key, [D|Ds]) ->
    case maps:find(Key, D) of
	error -> find_in_dicts_(Key, Ds);
	Res -> Res
    end;
find_in_dicts_(_Key, []) ->
    error.
