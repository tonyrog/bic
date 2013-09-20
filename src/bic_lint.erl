%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%    BIC linter 
%%% @end
%%% Created : 14 Sep 2013 by Tony Rogvall <tony@rogvall.se>

-module(bic_lint).

-compile(export_all).

-include("../include/bic.hrl").

-record(cbinary, { op, arg1, arg2 }).
-record(cunary,  { op, arg }).
-record(ccall,   { func, args }).
-record(cassign, { op, lhs, rhs }).
-record(cifexpr, { test, then, else}).

-type cexpr() :: #cbinary{} | #cunary{} | #ccall{} | integer() | float().
-type cdecl() :: any(). %% forward - #cdecl{}.

-record(cstruct,
	{
	  name   = undefined :: undefined | string(), %% optional name
	  elems  = [] :: [cdecl()]       %% declarations
	}).

-record(cunion,
	{
	  name   = undefined :: undefined | string(), %% optional name
	  elems  = [] :: [cdecl()]       %% declarations
	}).

-record(cenum,
	{
	  name   = undefined :: undefined | string(), %% optional name
	  elems  = [] :: [{string(),integer()}]       %% value
	}).

-record(cfun,
	{
	  name = undefined :: undefined | string(), %% optional name
	  return,
	  args
	}).

-record(cfunction,
	{
	  name = undefined :: undefined | string(), %% optional name
	  return,
	  args,
	  body
	}).

-record(cempty,   {}).
-record(cbreak,    {}).
-record(ccontinue, {}).
-record(cdefault,  {}).
-record(creturn, {expr}).
-record(cfor,     {init,test,update,body}).
-record(cwhile,   {test,body}).
-record(cdo,      {body,test}).
-record(clabel,	  {name,code}).
-record(cgoto,    {label}).
-record(ccase,    {expr, code}).
-record(cswitch,  {expr, body}).
-record(cif, {test,then,else}).

-record(ctype,
	{
	  sign       = default :: default | void | signed | unsigned,
	  const      = default :: default | boolean(),
	  volatile   = default :: default | boolean(),
	  storage    = default :: default |  auto | static | register | extern,
	  size       = default :: default | none | short | long | long_long,
	  type       = default :: default | char | int | float | double |
				  #cstruct{} | #cunion{},
	  dimension  = default :: default | [integer() | cexpr()]
	}).

-record(cdecl,
       {
	 name = undefined :: undefined | string(),
	 type = undefined :: undefined | #ctype{},
	 size = undefined :: undefined | integer(),
	 value = undefined :: undefined | cexpr()
       }).

-record(ctypedef,
       {
	 name = undefined :: undefined | string(),
	 type = undefined :: undefined | #ctype{},
	 size = undefined :: undefined | integer(),
	 value = undefined :: undefined | cexpr()
       }).

-type scopetype() :: compound | for | while | do | switch.

-record(scope,
	{
	  type     = [] :: [scopetype()],
	  typedefs = [] :: [dict()],   %% stack of dictionary (for each level)
	  decls    = [] :: [dict()],   %% stack of declared data
	  labels   = [] :: [string()],
	  errors   = [],
	  warnings = []
	}).

forms(Forms) ->
    io:format("Forms:\n~p\n", [Forms]),
    {CForms,Scope} = forms_(Forms, [], new_scope()),
    if Scope#scope.errors =:= [] ->
	    report(Scope#scope.warnings),
	    {ok,CForms};
       true ->
	    report(Scope#scope.warnings++Scope#scope.errors),
	    {error, lint}
    end.

forms_(undefined, _Acc, S) ->
    {undefined, S};
forms_(Stmt, _Acc, S) when is_tuple(Stmt) ->
    statement(Stmt, S);
forms_([F=#bic_typedef { name=Name } | Fs], Acc, S0) ->
    {CTypedef,S1} = do_typedef(F,S0),
    case find_local_type(Name, S1) of
	error ->
	    S2 = store_type(Name, CTypedef, S1),
	    forms_(Fs, [CTypedef|Acc], S2);
	{ok,_} ->
	    S2 = add_warning(S1, "type ~s already defined", [Name]),
	    S3 = store_type(Name, CTypedef, S2),
	    forms_(Fs, [CTypedef|Acc], S3)
    end;
forms_([D=#bic_decl{}| Fs], Acc, S0) ->
    {CDecl,S1} = do_decl(D, S0),
    forms_(Fs, [CDecl|Acc], S1);
forms_([F=#bic_struct{}|Fs], Acc, S0) ->
    {CStruct, S1} = do_struct(F, S0),
    forms_(Fs, [CStruct|Acc], S1);
forms_([F=#bic_union{}|Fs], Acc, S0) ->
    {CUnion, S1} = do_union(F, S0),
    forms_(Fs, [CUnion|Acc], S1);
forms_([F=#bic_enum{}|Fs], Acc, S0) ->
    {CEnum, S1} = do_enum(F, S0),
    forms_(Fs, [CEnum|Acc], S1);
forms_([F=#bic_function {name=Name} | Fs], Acc, S0) ->
    {CType,S1} = do_type(F#bic_function.storage, S0),
    [{fn,Args}] = F#bic_function.type,
    S2 = push_scope(S1),
    {CArgs,S3} = do_decls(Args, S2),
    %% {CParams,S3} = do_decls(F#bic_functions.params, S2),  %% merge?
    {CBody,S4} = forms_(F#bic_function.body, [], S3),
    C5 = pop_scope(S4),
    forms_(Fs, [#cfunction { name=Name,
			     return = CType,
			     args = CArgs,
			     body = CBody } | Acc], C5);
forms_([F | Fs], Acc, S) ->
    {CStmt, S1} = statement(F, S),
    forms_(Fs, [CStmt|Acc], S1);
forms_([], Acc, S) ->
    {lists:reverse(Acc), S}.

statement(#bic_empty{}, S) ->
    {#cempty{}, S};
statement(#bic_if { test=Test,then=Then,else=Else},S0) ->
    {CTest,S1} = do_expr(Test,S0),
    {CThen,S2} = forms_(Then,[],S1),
    {CElse,S3} = forms_(Else,[],S2),
    CIf = #cif { test=CTest, then=CThen, else=CElse },
    {CIf,S3};
statement(#bic_for { init=Init,test=Test,update=Update,body=Body},S0) ->
    {CInit,S1} = do_expr(Init,S0),
    {CTest,S2} = do_expr(Test,S1),
    {CUpdate,S3} = do_expr(Update,S2),
    {CBody,S4} = forms_(Body,[],push_scope(S3,for)),
    CFor = #cfor { init=CInit,test=CTest,update=CUpdate,body=CBody},
    {CFor,pop_scope(S4)};
statement(#bic_while { test=Test, body=Body },S0) ->
    {CTest,S1} = do_expr(Test,S0),
    {CBody,S2} = forms_(Body,[],push_scope(S1,while)),
    CWhile = #cwhile { test=CTest,body=CBody},
    {CWhile,pop_scope(S2)};
statement(#bic_do { body=Body, test=Test },S0) ->
    {CTest,S1} = do_expr(Test,S0),
    {CBody,S2} = forms_(Body,[],push_scope(S1,do)),
    CWhile = #cdo { body=CBody, test=CTest },
    {CWhile,pop_scope(S2)};
statement(#bic_switch { expr=Expr, body=Body },S0) ->
    {CExpr,S1} = do_expr(Expr,S0),
    {CBody,S2} = forms_(Body,[],push_scope(S1,switch)),
    CSwitch = #cswitch { expr=CExpr, body=CBody },
    {CSwitch,pop_scope(S2)};
statement(#bic_label { name = Name, code = Code }, S0) ->
    {CCode,S1} = forms_(Code,[],S0),
    S1 = add_label(Name, S0),
    CLabel = #clabel { name=Name, code=CCode },
    {CLabel, S1};
statement(#bic_case { expr = Expr, code = Code }, S0) ->
    {CExpr,S1} = do_expr(Expr,S0),
    {CCode,S2} = forms_(Code,[],S1),
    CCase = #ccase { expr=CExpr, code=CCode },
    {CCase, S2};
statement(#bic_goto { label=Name }, S0) ->
    %% FIXME: resolve goto's later, may have forward refs!
    CGoto = #cgoto { label=Name },
    {CGoto, S0};
statement(#bic_continue {}, S0) ->
    S1 = case is_scope_type([for,while,do],S0) of
	     false ->
		 add_error(S0, "continue statement not within a loop", []);
	     true -> S0
	 end,
    CContinue = #ccontinue {}, 
    {CContinue, S1};
statement(#bic_break {}, S0) ->
    S1 = case is_scope_type([for,while,do,switch],S0) of
	     false -> 
		 add_error(S0, "break statement not within loop or switch", []);
	     true -> S0
	 end,
    CBreak = #cbreak {},
    {CBreak, S1};
statement(#bic_default {}, S0) ->
    S1 = case is_scope_type([switch],S0) of
	     false -> 
		 add_error(S0, "'default' label not within a switch statement",
			   []);
	     true -> S0
	 end,
    CDefault = #cdefault {},
    {CDefault, S1};
statement(#bic_return { expr = Expr }, S0) ->
    {CExpr,S1} = do_expr(Expr,S0),
    CReturn = #creturn { expr = CExpr },
    {CReturn, S1};
statement(Expr, S0) when is_tuple(Expr) ->
    {CExpr,S1} = do_expr(Expr, S0),
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


report(Reports) ->
    [ begin io:format(Fmt++"\n", Args) end || {Fmt,Args} <- Reports ],
    ok.

do_typedef(D=#bic_typedef{}, S0) ->
    {CType,S1} = do_type(D#bic_typedef.type, S0),
    {Size,S2}  = do_expr(D#bic_typedef.size, S1),
    {Value,S3} = do_expr(D#bic_typedef.value,S2),
    {#ctypedef { name=D#bic_typedef.name,type=CType,size=Size,value=Value},S3}.

do_decl(D=#bic_decl{}, S0) ->
    {Type,S1}  = do_type(D#bic_decl.type, S0),
    {Size,S2}  = do_expr(D#bic_decl.size, S1),
    {Value,S3} = do_expr(D#bic_decl.value,S2),
    Name = D#bic_decl.name,
    CDecl = #cdecl { name=Name, type=Type, size=Size, value=Value },
    S4 = if Name =:= undefined -> 
		 S3;
	    true ->
		 case find_local_decl(Name, S0) of
		     error ->
			 store_decl(Name, CDecl, S3);
		     {ok,_} ->
			 S31 = store_decl(Name, CDecl, S3),
			 add_error(S31, "~s already declared", [Name])
		 end
	 end,
    {CDecl,S4}.

do_decls([D|Ds], S0) ->
    {D1, S1} = do_decl(D, S0),
    {Ds1, S2} = do_decls(Ds, S1),
    {[D1|Ds1],S2};
do_decls([], S0) ->
    {[], S0}.
	  

do_struct(X,S0) ->
    {Elems,S00} = do_decls(X#bic_struct.elems, push_scope(S0)),
    S1 = pop_scope(S00),
    Name = X#bic_struct.name,
    CStruct = #cstruct { name=Name, elems=Elems},
    if Name =:= undefined ->
	    {CStruct, S1};
       true ->
	    case find_local_type({struct,Name}, S1) of
		error ->
		    S2 = store_type({struct,Name}, CStruct, S1),
		    {CStruct, S2};
		{ok,_} ->
		    S2 = add_warning(S1, "struct ~s already defined", [Name]),
		    S3 = store_type({struct,Name}, CStruct, S2),
		    {CStruct, S3}
	    end
    end.


do_union(X,S0) ->
    {Elems,S00} = do_decls(X#bic_union.elems, push_scope(S0)),
    S1 = pop_scope(S00),
    Name = X#bic_union.name,
    CUnion = #cunion { name=Name, elems=Elems},
    if Name =:= undefined ->
	    {CUnion, S1};
       true ->
	    case find_local_type({union,Name}, S1) of
		error ->
		    S2 = store_type({union,Name}, CUnion, S1),
		    {CUnion, S2};
		{ok,_} ->
		    S2 = add_warning(S1, "union ~s already defined", [Name]),
		    S3 = store_type({union,Name}, CUnion, S2),
		    {CUnion, S3}
	    end
    end.

do_enum(X,S0) ->
    {Elems,S1} = do_enums(X#bic_enum.elems, S0),
    Name = X#bic_enum.name,
    CEnum = #cenum { name=Name, elems=Elems },
    S2 = store_enumerators(Elems, S1),  %% install enumerators
    if Name =:= undefined ->
	    {CEnum, S2};
       true ->
	    case find_local_type({enum,Name}, S2) of
		error ->
		    S3 = store_type({enum,Name}, CEnum, S2),
		    {CEnum, S3};
		{ok,_} ->
		    S3 = add_warning(S2, "enum '~s' already defined", [Name]),
		    S4 = store_type({enum,Name}, CEnum, S3),
		    {CEnum, S4}
	    end
    end.

store_enumerators([{ID,Value}|Elems], S0) ->
    CType = #ctype { type = int, const = true },
    case find_decl(ID, S0) of
	error ->
	    S1 = store_decl(ID, #cdecl { name=ID,type=CType,value=Value },S0),
	    store_enumerators(Elems, S1);
	{ok,_} ->
	    S1 = store_decl(ID, #cdecl { name=ID,type=CType,value=Value },S0),
	    S2 = add_error(S1, "~s already declared", [ID]),
	    store_enumerators(Elems, S2)
    end;
store_enumerators([], S0) ->
    S0.


do_enums(Elems, S0) ->
    do_enums(Elems, [], S0).

do_enums([{ID,Value}|Elems], Acc, S0) ->
    {CValue,S1} = do_expr(Value, S0),
    case lists:keyfind(ID, 1, Acc) of
	false ->
	    do_enums(Elems, [{ID,CValue}|Acc], S0);
	_ ->
	    S1 = add_error(S0, "enum '~s' already defined", [ID]),
	    Acc1 = lists:keydelete(ID, 1, Acc),
	    do_enums(Elems, [{ID,CValue}|Acc1], S0)
    end;
do_enums([], Acc, S0) ->
    Acc1 = do_enumerate(lists:reverse(Acc), [], 0),
    {Acc1, S0}.

do_enumerate([{ID,undefined}|Enums], Acc, I) ->
    do_enumerate(Enums, [{ID,I}|Acc], I+1);
do_enumerate([E={_ID,Value}|Enums], Acc, _I) ->
    do_enumerate(Enums, [E|Acc], Value+1);
do_enumerate([], Acc, _I) ->
    lists:reverse(Acc).
    

do_type(undefined, S) ->
    {#ctype{}, S};
do_type(T, S) ->
    do_type_(T, #ctype{}, S).

do_type_([X|Xs], T, S) ->
    case X of
	char     -> set_basic_type(char, T, Xs, S);
	int      -> set_basic_type(int, T, Xs, S);
	float    -> set_basic_type(float, T, Xs, S);
	double   -> set_basic_type(double, T, Xs, S);
	void     -> set_basic_type(void, T, Xs, S);
	signed   -> set_basic_sign(signed, T, Xs, S);
	unsigned -> set_basic_sign(unsigned, T, Xs, S);
	short    -> set_basic_size(short, T, Xs, S);
	long     -> set_basic_size(long, T, Xs, S);
	auto     -> set_basic_storage(auto, T, Xs, S);
	static   -> set_basic_storage(static, T, Xs, S);
	register -> set_basic_storage(register, T, Xs, S);
	extern   -> set_basic_storage(extern, T, Xs, S);
	const    -> do_type_(Xs, T#ctype {const=true}, S);
	volatile -> do_type_(Xs, T#ctype {volatile=true}, S);
	{array,Dim} -> add_dimension(Dim, T, Xs, S);
	{fn,Args} -> 
	    {CArgs,S1} = do_decls(Args, push_scope(S)),
	    CFun = #cfun { return = T#ctype.type,
			   args   = CArgs },
	    do_type_(Xs, T#ctype { type=CFun }, pop_scope(S1));
	#bic_typeid { name=Name} ->
	    case find_type(Name, S) of
		error ->
		    S1 = add_error(S,"type '~s' not found", [Name]),
		    do_type_(Xs, T#ctype { type=int }, S1);
		{ok,Typedef} ->
		    %% fixme: check interaction between T and T1
		    T1 = Typedef#ctypedef.type,
		    do_type_(Xs, T1, S)
	    end;
	#bic_enum{}  ->
	    {CEnum,S1} = do_enum(X, S),
	    do_type_(Xs, T#ctype { type=CEnum }, S1);
	#bic_struct{} -> 
	    {CStruct,S1} = do_struct(X, S),
	    do_type_(Xs, T#ctype { type=CStruct }, S1);
	#bic_union{}  ->
	    {CUnion,S1} = do_union(X, S),
	    do_type_(Xs, T#ctype { type=CUnion }, S1)
    end;
do_type_([],T,S) ->
    {T,S}.

do_expr(undefined, S0) ->
    {undefined, S0};
do_expr(C=#bic_constant { }, S0) ->
    do_constant(C,S0);
do_expr(#bic_id { name=Name }, S0) ->
    case find_decl(Name, S0) of
	error ->
	    S1 = add_error(S0,"identifier '~s' not declared", [Name]),
	    { #cdecl { name=Name, type=#ctype { type=int } }, S1 };
	{ok,CDecl} ->
	    
	    { CDecl, S0}
    end;
do_expr(#bic_unary { op=Op, arg=Arg}, S0) ->
    {CArg,S1} = do_expr(Arg,S0),
    {#cunary { op=Op, arg=CArg }, S1};
do_expr(#bic_binary { op=Op, arg1=Arg1, arg2=Arg2}, S0) ->
    {CArg1,S1} = do_expr(Arg1,S0),
    {CArg2,S2} = do_expr(Arg2,S1),
    {#cbinary { op=Op, arg1=CArg1, arg2=CArg2}, S2};
do_expr(#bic_call { func=Func, args=Args }, S0) ->
    {CFunc, S1} = do_expr(Func, S0),
    {CArgs, S2} = do_expr(Args, S1),
    {#ccall { func = CFunc, args = CArgs }, S2};
do_expr(#bic_assign { op=Op, lhs=Lhs, rhs=Rhs}, S0) ->
    {CLhs,S1} = do_expr(Lhs,S0), %% do_lhs!!
    {CRhs,S2} = do_expr(Rhs,S1),
    {#cassign { op=Op, lhs=CLhs, rhs=CRhs}, S2};
do_expr(#bic_ifexpr { test=Test, then=Then, else=Else}, S0) ->
    {CTest,S1} = do_expr(Test,S0),
    {CThen,S2} = do_expr(Then,S1),
    {CElse,S3} = do_expr(Else,S2),
    {#cifexpr { test=CTest, then=CThen, else=CElse}, S3};
do_expr([X | Xs], S0) ->
    {CX,S1}  = do_expr(X,S0),
    {CXs,S2} = do_expr(Xs,S1),
    {[CX|CXs], S2};
do_expr([], S0) ->
    {[], S0}.








do_constant(#bic_constant { base=float, value=Val },S0) ->
    {list_to_float(Val), S0};
do_constant(#bic_constant { base=char, value=[$',Val,$'] },S0) ->
    {Val, S0};
do_constant(#bic_constant { base=string, value=Val },S0) ->
    {Val, S0};
do_constant(#bic_constant { base=16, value=[$0,$x|Val] }, S0) ->
    {list_to_integer(Val, 16), S0};
do_constant(#bic_constant { base=B, value=Val },S0) when is_integer(B) ->
    {list_to_integer(Val, B), S0}.

%% only 
add_dimension(C=#bic_constant { }, T, Xs, S) ->
    {Dim, S1} = do_constant(C, S),
    Dims = if T#ctype.dimension =:= default -> [];
	      is_list(T#ctype.dimension) -> T#ctype.dimension
	   end,
    T1 = T#ctype { dimension = [Dim | Dims ] },
    do_type_(Xs, T1, S1);
add_dimension(Expr, T, Xs, S) ->
    {CExpr,S1} = do_expr(Expr, S),
    Dims = if T#ctype.dimension =:= default -> [];
	      is_list(T#ctype.dimension) -> T#ctype.dimension
	   end,
    T1 = T#ctype { dimension = [CExpr | Dims ] },
    do_type_(Xs, T1, S1).


set_basic_sign(Sign, T, Xs, S) ->
    if T#ctype.sign =:= default ->
	    do_type_(Xs, T#ctype { sign=Sign }, S);
       T#ctype.sign =:= Sign ->
	    S1 = add_error(S,"duplicate '~s'", [Sign]),
	    do_type_(Xs, T, S1);
       true ->
	    S1 = add_error(S,"both '~s' and '~s' in declaration specifiers", 
			   [Sign, T#ctype.sign]),
	    do_type_(Xs, T, S1)
    end.

set_basic_storage(Storage, T, Xs, S) ->
    if T#ctype.storage =:= default ->
	    do_type_(Xs, T#ctype { storage=Storage }, S);
       T#ctype.storage =:= Storage ->
	    S1 = add_error(S,"duplicate '~s'", [Storage]),
	    do_type_(Xs, T, S1);
       true ->
	    S1 = add_error(S,"both '~s' and '~s' in declaration specifiers", 
			   [Storage, T#ctype.storage]),
	    do_type_(Xs, T, S1)
    end.
	    
set_basic_size(Size, T, Xs, S) ->
    if T#ctype.size =:= default ->
	    do_type_(Xs, T#ctype { size=Size }, S);
       Size =:= long, T#ctype.size =:= long ->
	    do_type_(Xs, T#ctype { size=long_long }, S);
       Size =:= T#ctype.size ->
	    S1 = add_error(S,"duplicate '~s'", [Size]),
	    do_type_(Xs, T, S1);
       Size =/= T#ctype.size ->
	    S1 = add_error(S,"both '~s' and '~s' in declaration specifiers", 
			   [Size, T#ctype.size]),
	    do_type_(Xs, T, S1)
    end.

set_basic_type(Basic, T, Xs, S) ->
    if T#ctype.type =:= default, Basic =:= void ->
	    if T#ctype.sign =/= default;
	       T#ctype.size =/= default ->
		    S1 = add_error(S,"void has no size nor sign", []),
		    do_type_(Xs, T#ctype { type=Basic,
					   sign=default,
					   size=default }, S1);
	       true ->
		    do_type_(Xs, T#ctype { type=Basic }, S)
	    end;
       T#ctype.type =:= default ->
	    if Basic =:= void ->
		    if T#ctype.sign =/= default;
		       T#ctype.size =/= default ->
			    S1 = add_error(S,"void has no size nor sign", []),
			    do_type_(Xs, T#ctype { type=Basic,
						   sign=default,
						   size=default }, S1);
		       true ->
			    do_type_(Xs, T#ctype { type=Basic }, S)
		    end;
	       Basic =:= float; Basic =:= double ->
		    if T#ctype.sign =/= default;
		       T#ctype.size =/= default ->
			    S1 = add_error(S,"'~s' has fixed size and sign",
					   [Basic]),
			    do_type_(Xs, T#ctype { type=Basic }, S1);
		       true ->
			    do_type_(Xs, T#ctype { type=Basic }, S)
		    end;
	       true ->
		    do_type_(Xs, T#ctype { type=Basic }, S)
	    end;
       true -> 
	    S1 = add_error(S,"both '~s' and '~s' in declaraion specifiers",
			   [Basic,T#ctype.type]),
	    do_type_(Xs, T#ctype { type=Basic }, S1)
    end.
    

add_warning(S, Fmt, Args) ->
    S#scope { warnings = [{Fmt,Args} | S#scope.warnings] }.

add_error(S, Fmt, Args) ->
    S#scope { errors = [{Fmt,Args} | S#scope.errors] }.


add_label(Name, S) ->
    S#scope { labels = [Name | S#scope.labels] }.

del_label(Name, S) ->
    S#scope { labels = lists:delete(Name, S#scope.labels) }.

%% start of a new function 
clear_labels(S) ->
    S#scope { labels = [] }.
    

new_scope() ->
    #scope { 
       typedefs = [dict:new()],
       decls    = [dict:new()]
      }.

push_scope(S) ->
    push_scope(S, compound).

push_scope(S,Type) ->
    S#scope {
      type     = [Type | S#scope.type],
      typedefs = [dict:new() | S#scope.typedefs],
      decls    = [dict:new() | S#scope.decls]
     }.

pop_scope(S) ->
    S#scope {
      type     = tl(S#scope.type),
      typedefs = tl(S#scope.typedefs),
      decls    = tl(S#scope.decls)
     }.

%% check if any of the scope Types are present (for,do,while,switch)
is_scope_type(Types, S) ->
    is_scope_type_(Types, S#s.type).

is_scope_type_(Types, [Type|Ts]) ->
    case lists:member(Type, Types) of
	true -> true;
	false -> is_scope_type_(Types, Ts)
    end;
is_scope_type_(Types, []) ->
    false.


find_type(Name, S) ->
    find_in_dicts_(Name, S#scope.typedefs).

find_local_type(Name, S) ->
    dict:find(Name, hd(S#scope.typedefs)).

store_type(Name, Type, S) ->
    io:format("store ~p = ~p\n", [Name, Type]),
    S#scope { typedefs = store_in_dicts_(Name, Type, S#scope.typedefs) }.

find_decl(Name, S) ->
    find_in_dicts_(Name, S#scope.decls).

find_local_decl(Name, S) ->
    dict:find(Name, hd(S#scope.decls)).

store_decl(Name, Decl, S) ->
    io:format("store ~p = ~p\n", [Name, Decl]),
    S#scope { decls = store_in_dicts_(Name, Decl, S#scope.decls) }.

erase_in_dicts_(Key, [D|Ds]) ->
    [dict:erase(Key, D) | Ds].

store_in_dicts_(Key, Value, [D|Ds]) ->
    [dict:store(Key, Value, D) | Ds].

find_in_dicts_(Key, [D|Ds]) ->
    case dict:find(Key, D) of
	error -> find_in_dicts_(Key, Ds);
	Res -> Res
    end;
find_in_dicts_(_Key, []) ->
    error.
