%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%    BIC linter 
%%% @end
%%% Created : 14 Sep 2013 by Tony Rogvall <tony@rogvall.se>

-module(bic_lint).

-compile(export_all).

-include("../include/bic.hrl").

-type scopetype() :: compound | for | while | do | switch.

-type typemap() :: #{ string() => bic_typedef()}.
-type declmap() :: #{ string() => bic_decl()}.

-record(scope,
	{
	 filename = "" :: string(),  %% filename 
	 func          :: undefined | bic_function(),
	 type     = [] :: [scopetype()],
	 typedefs = [] :: [typemap()], %% stack of typedefs
	 decls    = [] :: [declmap()], %% stack of declarations
	 labels   = [] :: [string()],
	 errors   = [],
	 warnings = []
	}).

definitions(Filename,Defs) ->
    %% io:format("Defs:\n~p\n", [Defs]),
    {CDefs,Scope} = definitions_(Defs, [], new_scope(Filename)),
    if Scope#scope.errors =:= [] ->
	    report(Scope#scope.filename, Scope#scope.warnings),
	    {ok,CDefs};
       true ->
	    report(Scope#scope.filename,
		   Scope#scope.warnings++Scope#scope.errors),
	    {error, lint}
    end.

definitions_([F=#bic_typedef { line=Ln, name=Name } | Fs], Acc, S0) ->
    {CTypedef,S1} = typedef(F,S0),
    case find_local_type(Name, S1) of
	error ->
	    S2 = store_type(Name, CTypedef, S1),
	    definitions_(Fs, [CTypedef|Acc], S2);
	{ok,_} ->
	    S2 = add_warning(S1, Ln, "type ~s already defined", [Name]),
	    S3 = store_type(Name, CTypedef, S2),
	    definitions_(Fs, [CTypedef|Acc], S3)
    end;
definitions_([D=#bic_decl{}| Fs], Acc, S0) ->
    {CDecl,S1} = decl(D, S0),
    definitions_(Fs, [CDecl|Acc], S1);
definitions_([F=#bic_function {line=Ln,name=Name} | Fs], Acc, S0) ->
    {CType,S1} = type(F#bic_function.type,Ln,S0),
    S2 = push_scope(S1),
    {CArgs,S3} = decls(F#bic_function.params, S2),

    %% declare F for recursive functions
    Func0 = F#bic_function { type=CType, params=CArgs, body=undefined },
    CDecl0 = #bic_decl { line = Ln, name = Name, type = CType, value = Func0 },
    S400 = store_decl(Name, CDecl0, S3),
    S40 = S400#scope{ func = Func0 }, %% current function
    %% {CParams,S3} = decls(F#bic_functions.params, S2),  %% merge?
    S41 = push_scope(S40),
    {CBody,S42} = statement_list(F#bic_function.body, [], clear_labels(S41)),
    %% CBody1 = resolve_labels(CBody, S4)
    S43 = pop_scope(S42),
    S5 = pop_scope(S43),
    Func = F#bic_function { type=CType, params=CArgs, body=CBody },
    CDecl = #bic_decl { line=Ln, name=Name, type=CType, value=Func },
    S6 = store_decl(Name, CDecl, S5),
    definitions_(Fs, [Func | Acc], S6#scope{ func=undefined} );
definitions_([], Acc, S) ->
    {lists:reverse(Acc), S}.

statement(undefined, S) -> %% empty else for example
    {undefined, S};
statement(Y=#bic_expr_stmt{expr=Expr}, S0) ->
    {CExpr,S1} = expr(Expr, S0),
    {Y#bic_expr_stmt{expr=CExpr}, S1};
statement(Y=#bic_decl{}, S0) ->
    decl(Y, S0);
statement(#bic_if { line=Ln,test=Test,then=Then,else=Else},S0) ->
    {CTest,S1} = expr(Test,S0),
    {CThen,S2} = statement(Then,S1),
    {CElse,S3} = statement(Else,S2),
    CIf = #bic_if { line=Ln,test=CTest, then=CThen, else=CElse },
    {CIf,S3};
statement(Y=#bic_for{ init=Init,test=Test,update=Update,body=Body},S0) ->
    {CInit,S1} = expr(Init,S0),
    {CTest,S2} = expr(Test,S1),
    {CUpdate,S3} = expr(Update,S2),
    {CBody,S4} = statement(Body,push_scope(S3,for)),
    CFor = Y#bic_for { init=CInit,test=CTest,update=CUpdate,body=CBody},
    {CFor,pop_scope(S4)};
statement(Y=#bic_while { test=Test, body=Body },S0) ->
    {CTest,S1} = expr(Test,S0),
    {CBody,S2} = statement(Body,push_scope(S1,while)),
    CWhile = Y#bic_while { test=CTest,body=CBody},
    {CWhile,pop_scope(S2)};
statement(Y=#bic_do { body=Body, test=Test },S0) ->
    {CTest,S1} = expr(Test,S0),
    {CBody,S2} = statement(Body,push_scope(S1,do)),
    CWhile = Y#bic_do { body=CBody, test=CTest },
    {CWhile,pop_scope(S2)};
statement(Y=#bic_switch { expr=Expr, body=Body },S0) ->
    {CExpr,S1} = expr(Expr,S0),
    {CBody,S2} = statement(Body,push_scope(S1,switch)),
    CSwitch = Y=#bic_switch { expr=CExpr, body=CBody },
    {CSwitch,pop_scope(S2)};
statement(Y=#bic_label { name = Name, code = Code }, S0) ->
    {CCode,S1} = statement(Code,S0),
    S1 = add_label(Name, S0),
    CLabel = Y#bic_label { code=CCode },
    {CLabel, S1};
statement(Y=#bic_case { expr = Expr, code = Code }, S0) ->
    {CExpr,S1} = expr(Expr,S0),
    {CCode,S2} = statement(Code,S1),
    CCase = Y#bic_case { expr=CExpr, code=CCode },
    {CCase, S2};
statement(Y=#bic_goto { label=_Name }, S0) ->
    %% FIXME: resolve goto's later, may have forward refs!
    CGoto = Y,
    {CGoto, S0};
statement(Y=#bic_continue {line=Ln}, S0) ->
    S1 = case is_scope_type([for,while,do],S0) of
	     false ->
		 add_error(S0,Ln,"continue statement not within a loop", []);
	     true -> S0
	 end,
    CContinue = Y,
    {CContinue, S1};
statement(Y=#bic_break {line=Ln}, S0) ->
    S1 = case is_scope_type([for,while,do,switch],S0) of
	     false -> 
		 add_error(S0,Ln,
			   "break statement not within loop or switch", []);
	     true -> S0
	 end,
    {Y, S1};
statement(Y=#bic_default {line=Ln}, S0) ->
    S1 = case is_scope_type([switch],S0) of
	     false -> 
		 add_error(S0, Ln,
			   "'default' label not within a switch statement",
			   []);
	     true -> S0
	 end,
    {Y, S1};
statement(Y=#bic_return { line=Ln,expr=Expr}, S0) ->
    {CExpr,S1} = expr(Expr,S0),
    F = S0#scope.func,
    S3 = 
	if F =:= undefined ->
		add_error(S1, Ln,
			  "return statement not in a function",
			  []); 
	   true ->
		{_CType,S2}=check_type(Ln,'==',bic:typeof(CExpr),
				       F#bic_function.type,S1),
		S2
	end,
    CReturn = Y#bic_return { expr = CExpr },
    {CReturn, S3};
statement(Y=#bic_empty{}, S) ->
    {Y, S};
statement(Y=#bic_compound{code=Stmts}, S0) ->
    {Stmts1,S1} = compound(Stmts, [], S0),
    {Y#bic_compound{code=Stmts1}, S1}.

compound(Stmts, Acc, S0) ->
    S1 = push_scope(S0),
    {Stmts1,S2} = statement_list(Stmts, Acc, S1),
    S3 = pop_scope(S2),
    {Stmts1,S3}.

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
    {D#bic_typedef { line=Ln, type=CType,size=Size,value=Value},S3}.

decl(D=#bic_decl{line=Ln}, S0) ->
    {Type,S1}  = type(D#bic_decl.type,Ln,S0),
    {Size,S2}  = expr(D#bic_decl.size, S1),
    {Value,S3} = expr(D#bic_decl.value,S2),
    Name = D#bic_decl.name,
    CDecl = D#bic_decl { type=Type, size=Size, value=Value },
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
    CStruct = X#bic_struct { name=Name, elems=Elems},
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
    CUnion = X#bic_union { name=Name, elems=Elems},
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
    CEnum = #bic_enum { elems=Elems },
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
    CType = #bic_type { type = int, const = true },
    case find_decl(ID, S0) of
	error ->
	    S1 = store_decl(ID, #bic_decl { line=Ln,name=ID,
					    type=CType,value=Value },S0),
	    store_enumerators(Elems, S1);
	{ok,_} ->
	    S1 = store_decl(ID, #bic_decl { line=Ln,name=ID,
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
	#bic_constant { value = Value1 } when is_integer(Value1) ->
	    enumerate(Enums, [{ID,Ln,Value1}|Acc], I);
	_ -> %% fixme eval
	    enumerate(Enums, [E|Acc], I)
    end;
enumerate([], Acc, _I) ->
    lists:reverse(Acc).

type(undefined,Ln,S) ->
    {#bic_type{line=Ln}, S};
type(T,Ln,S) ->
    type_(T,Ln,S).

type_(Xt=#bic_pointer{type=T},Ln,S) -> 
    {T1,S1} = type_(T,Ln,S),
    {Xt#bic_pointer{type=T1},S1};
type_(Xt=#bic_array{type=T,dim=D},Ln,S) ->
    {T1,S1} = type_(T,Ln,S),
    {D1,S2} = if D =:= [] -> {[],S1};
		 true -> expr(D, S1)
	      end,
    {Xt#bic_array{type=T1,dim=D1},S2};
type_(Xt=#bic_fn{type=T,params=Ps},Ln,S) -> 
    {T1,S1} = type_(T,Ln,S),
    S2 = push_scope(S1),
    {Ps1,S3} = decls(Ps,S2),
    S4 = pop_scope(S3),
    {Xt#bic_fn{type=T1,params=Ps1},S4};
type_(Xt=#bic_typeid {name="..."},_Ln,S) ->
    {Xt,S};
type_(#bic_typeid { line=Ln,name=Name},_Ln,S) ->
    case find_type(Name, S) of
	error ->
	    S1 = add_error(S,Ln,"type '~s' not found", [Name]),
	    {#bic_type { type=int }, S1};
	{ok,Yt} ->
	    %% fixme: check interaction between T and T1
	    {Yt#bic_typedef.type,S}
    end;
type_(Xt=#bic_enum{},_Ln,S)  ->
    enum(Xt, S);
type_(Xt=#bic_struct{},_Ln,S) -> 
    struct(Xt, S);
type_(Xt=#bic_union{},_Ln,S) ->
    union(Xt, S);
type_(Xt=#bic_type{},_Ln,S) ->
    {Xt, S}.

expr(undefined, S0) ->
    {undefined, S0};
expr(C=#bic_constant { }, S0) ->
    constant(C,S0);
expr(X=#bic_id { line=Ln, name=Name }, S0) ->
    case find_decl(Name, S0) of
	error ->
	    S1 = add_error(S0,Ln,"identifier '~s' not declared", [Name]),
	    { X#bic_id { type=#bic_type { type=int } }, S1 };
	{ok,CDecl} ->
	    %% io:format("decl of ~s = ~p\n", [Name, CDecl]),
	    { X#bic_id { type=CDecl#bic_decl.type }, S0}
    end;
expr(X=#bic_unary { line=Ln, op=sizeof, arg=Arg }, S0) ->
    {SzArg,S2} = case bic:is_expr(Arg) of
		     true -> 
			 {Arg1,S1} = expr(Arg,S0),
			 {bic:typeof(Arg1),S1};
		     false ->
			 type(Arg,Ln,S0)
		 end,
    {X#bic_unary{ type=sizeof_type(Ln), arg=SzArg }, S2};
expr(X=#bic_unary { line=Ln, op=typeof, arg=Arg }, S0) ->
    {Type1,S2} = case bic:is_expr(Arg, S0) of
		     true ->
			 {Arg1,S1} = expr(Arg,S0),
			 {bic:typeof(Arg1),S1};
		     false ->
			 type(Arg,Ln,S0)
		 end,
    {X#bic_unary{ arg=Type1 }, S2};
expr(X=#bic_unary { line=Ln, op=Op, arg=Arg}, S0) ->
    {CArg,S1} = expr(Arg,S0),
    {CType,S2} = check_type(Ln,Op,bic:typeof(CArg),S1),
    {X#bic_unary { type=CType, arg=CArg }, S2};
expr(X=#bic_binary { line=Ln, op=cast, arg1=Type, arg2=Arg2}, S0) ->
    {CArg2,S1} = expr(Arg2,S0),
    {CType,S2} = type(Type,Ln,S1),
    {X#bic_binary { type=CType,arg1=CType,arg2=CArg2}, S2};
expr(X=#bic_binary { line=Ln, op=Op, arg1=Arg1, arg2=Arg2}, S0) ->
    {CArg1,S1} = expr(Arg1,S0),
    {CArg2,S2} = expr(Arg2,S1),
    {CType,S3} = check_type(Ln,Op,bic:typeof(CArg1),bic:typeof(CArg2),S2),
    {X#bic_binary { type=CType,arg1=CArg1,arg2=CArg2}, S3};
expr(X=#bic_call { line=Ln, func=Func, args=Args }, S0) ->
    {CFunc, S1} = expr(Func, S0),
    {CArgs, S2} = expr_list(Args, S1),
    %% check actal args with formal arguments 
    {CType,S3} = check_type(Ln, call, bic:typeof(CFunc),
			    [bic:typeof(T) || T <- CArgs], S2),
    {X#bic_call { func = CFunc, type=CType, args = CArgs }, S3};
expr(X=#bic_assign { line=Ln, op=Op, lhs=Lhs, rhs=Rhs}, S0) ->
    {CLhs,S1} = expr(Lhs,S0), %% fixme check valid lhs!!
    {CRhs,S2} = expr(Rhs,S1), %% fixme check valid rhs (and undefined)
    {CType,S3} = check_assign(Ln, Op, bic:typeof(CLhs), bic:typeof(CRhs), S2),
    {X#bic_assign { type=CType, lhs=CLhs, rhs=CRhs}, S3};
expr(X=#bic_ifexpr { line=Ln, test=Test, then=Then, else=Else}, S0) ->
    {CTest,S1} = expr(Test,S0),
    {CThen,S2} = expr(Then,S1),
    {CElse,S3} = expr(Else,S2),
    %% CThen and CElse must be compatible
    %% {CType,S3} = check_type(Op,CTest,CElse,S3),
    {CType,S4} = check_type(Ln,'==',CThen,CElse,S3),
    {X#bic_ifexpr { type=CType, test=CTest, then=CThen, else=CElse}, S4};
expr(Xs, S0) when is_list(Xs) ->
    expr_list(Xs, S0).


expr_list(Xs,S0) ->
    expr_list_(Xs,S0,[]).

expr_list_([X|Xs],S0,Acc) ->
    {CX,S1}  = expr(X,S0),
    expr_list_(Xs,S1,[CX|Acc]);
expr_list_([],Si,Acc) ->
    {lists:reverse(Acc),Si}.


check_type(Ln, Op, T, S) ->
    if Op =:= '~'; Op =:= '!' ->
	    case bic:is_int_type(T) of
		true -> {T#bic_type{line=Ln}, S};
		false ->
		    %% coerce pointer to integer
		    S1 = add_error(S,Ln,"operator ~s expect integer arguments",
				   [Op]),
		    {#bic_type{line=Ln,type=int}, S1}
	    end;
       Op =:= '&' ->
	    {#bic_pointer{line=Ln,type=T}, S};
       Op =:= '*' ->
	    case T of
		#bic_pointer{type=Type} ->
		    {Type, S};  %% fixme set line number of Type
		_ ->
		    S1 = add_error(S,Ln,"argument is not a pointer type",
				   []),
		    {#bic_type{line=Ln,type=int}, S1}
	    end;
       Op =:= '-'; Op =:= '+' ->
	    case bic:is_number_type(T) of
		true ->
		    {T#bic_type{line=Ln},S};
		_ ->
		    S1 = add_error(S,Ln,"operator ~s expect scalar arguments",
				   [Op]),
		    {T#bic_type{line=Ln}, S1}
	    end;
       Op =:= '++'; Op =:= '--'; Op =:= '+++'; Op =:= '---' ->
	    case bic:is_pointer_type(T) of
		true ->
		    {T,S};
		false ->
		    case bic:is_int_type(T) of
			true ->
			    {T#bic_type{line=Ln},S};
			false ->
			    S1 = add_error(S,Ln,"operator ~s expect scalar arguments",
					   [Op]),
			    {T#bic_type{line=Ln}, S1}
		    end
	    end;
       true ->
	    S1 = add_error(S,Ln,"operator ~s not expected",[Op]),
	    io:format("T = ~w, S1 = ~w\n", [T, S1]),
	    {T#bic_type{line=Ln}, S1}
    end.

check_type(Ln, '[]', T1, T2, S) ->
    %% io:format("check_type ~p ~w ~w\n", ['[]',T1,T2]),
    case bic:is_int_type(T2) of
	false ->
	    S1 = add_error(S,Ln,"index expression is not integer type",[]),
	    {bic:base_typeof(T1), S1};
	true ->
	    case bic:is_address_type(T1) of
		true ->
		    {bic:base_typeof(T1), S};
		false ->
		    S1 = add_error(S,Ln,"subscript is not an array",[]),
		    {#bic_type{line=Ln,type=int}, S1}
	    end
    end;
check_type(Ln, '+', T1, T2, S) ->
    %% io:format("check_type ~p ~w ~w\n", ['+',T1,T2]),
    case bic:is_number_type(T1) andalso bic:is_number_type(T2) of
	true ->
	    {coerce(T1,T2), S};
	false ->
	    case (bic:is_pointer_type(T1) andalso bic:is_int_type(T2)) of
		true -> {T1,S};
		false  ->
		case (bic:is_pointer_type(T2) andalso bic:is_int_type(T1)) of
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

sizeof_type(Ln) ->
    %% #bic_type{ line=Ln, sign=unsigned, const=true, size=long, type=int}.
    #bic_typeid { line=Ln, name="size_t"}.

constant(C=#bic_constant { base=float, token=Val },S0) ->
    {Value, FloatType} = bic:token_to_float(Val),
    Const = C#bic_constant { value=Value, type=FloatType },
    {Const, S0};
constant(C=#bic_constant { base=char, token=Token },S0) ->
    {Value, CharType} = bic:token_to_char(Token),
    Const = C#bic_constant { value=Value, type=CharType},
    {Const, S0};
constant(C=#bic_constant { base=string, token=Token },S0) ->
    {Value, CharType} = bic:token_to_string(Token),
    Const = C#bic_constant { value=Value, 
			     type=#bic_array{type=CharType,dim=[]} },
    {Const, S0};
constant(C=#bic_constant { base=Base, token=Token }, S0) when 
      Base =:= 2; Base =:= 8; Base =:= 10; Base =:= 16 ->
    {Value,Type} = bic:token_to_integer(Token, Base),
    Const = C#bic_constant { value=Value, type=Type },
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
