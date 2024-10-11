%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    Code expansion/unrolling
%%% @end
%%% Created : 25 Nov 2020 by Tony Rogvall <tony@rogvall.se>

-module(bic_unroll).

-export([definitions/1, definitions/3]).
-export([new_env/0, new_env/2, push_env/1, pop_env/1]).

-compile(export_all).

-include_lib("bic/include/bic.hrl").

-define(dbg(F,A), ok).
%%-define(dbg(F,A), io:format((F),(A))).

-type value() :: undefined | integer() | float().
-type vref() :: Var::string() | {Var::string(),Index::[integer()]}.
-type scope() :: #{ {value,vref()} => value(),
		    {type,vref()} => bic_type() }.
-type env() :: #{
		 global => scope(),
		 stack => [scope()],
		 opts => #{ Key::atom() => Value::term() },
		 code => [bic_statement()],  %% some generated code
		 unique => integer()          %% generate uniqe names
		}.

-spec definitions(Ds::bic_definitions()) ->
	  bic_definitions().

definitions(Ds) ->
    definitions_(Ds, [], [], new_env()).

-spec definitions(Ds::bic_definitions(), Options::map(),
		  FunctionNames::[string()]) ->
	  bic_definitions().

definitions(Ds,Opts,Fs) ->
    definitions_(Ds, Fs, [], new_env(Opts)).
    
definitions_([D|Ds],Fs,Acc,E0) ->
    case D of
	#bic_function{name=Name} ->
	    case (Fs =:= []) orelse lists:member(Name,Fs) of
		true ->
		    {D1,E1} = definition(D, E0),
		    definitions_(Ds,Fs,[D1|Acc],E1);
		false ->
		    E1 = set_function(Name, E0, D),
		    definitions_(Ds,Fs,Acc,E1)
	    end;
	_ ->
	    {D1,E1} = definition(D, E0),
	    definitions_(Ds,Fs,[D1|Acc],E1)
    end;
definitions_([], _Fs, Acc, _E) ->
    lists:reverse(Acc).

definition(D=#bic_function{name=Fun, params=Params, body=Code}, E) ->
    E00 = push_env(E),
    E0  = set_current_function(Fun, E00),
    %% declare all parameters
    E1 = lists:foldl(fun(#bic_decl{name=Var,type=Type}, Ei) ->
			     decl(Var,Ei,Type)
		     end, E0, Params),
    %% partial eval body 
    {Code1, E2} = statement_list_(Code, E1),
    E3 = pop_env(E2),
    Fn = #bic_fn{line=D#bic_function.line,
		 type=D#bic_function.type,
		 params=Params},
    E4 = decl(Fun, E3, Fn),
    Function = D#bic_function{params=Params,body=Code1},
    {Function, set_function(Fun, E4, Function)};
definition(D=#bic_decl{name=Var,type=Type,value=Expr}, E) ->
    E1 = decl(Var,E,Type),
    {V1, X1, E2} = expr(Expr, E1),
    if is_number(V1) ->
	    {D#bic_decl{value=V1}, set_value(Var, E2, V1)};
       true ->
	    {D#bic_decl{value=X1}, E2}
    end;
definition(D=#bic_typedef{}, E) ->
    %% FIXME: introduce type ! in env.
    {D, E}.

%% for (init, cond, update)
statement(S=#bic_expr_stmt{expr=Expr}, E) ->
    case expr(Expr, E) of
	{undefined, undefined, E1} -> %% assume generated code?
	    {Stmts, E2} = pop_code(E1),
	    %% fixme: line numbers!
	    {#bic_compound{code=Stmts}, E2};
	{undefined, Expr1, E1} -> %% assume generated code?
	    S1 = S#bic_expr_stmt{expr=Expr1},
	    {Stmts, E2} = pop_code(E1),
	    {#bic_compound{code=Stmts++[S1]}, E2};
	{_V1, Expr1, E1} ->
	    {S#bic_expr_stmt{expr=Expr1}, E1}
    end;
%%statement(Const=#bic_constant{}, E) -> ???
%%    {Const,E};
statement(S=#bic_for{},E) ->
    for(S,E);
statement(S=#bic_while{},E) ->
    while(S,E);
statement(S=#bic_do{},E) ->
    do(S,E);
statement(S=#bic_switch{},E) ->
    switch(S,E);
statement(S=#bic_case{expr=X,code=Code},E) ->
    {V,X1,_E1} = expr(X, E),
    {Code1,_E2} = statement(Code, E),
    if is_integer(V) ->
	    {S#bic_case{expr=V,code=Code1},E};
       true ->
	    {S#bic_case{expr=X1,code=Code1},E}
    end;
statement(S=#bic_default{code=Code},E) ->
    {Code1, E1} = statement(Code, E),
    {S#bic_default{code=Code1},E1};
statement(S=#bic_continue{}, E) ->
    {S, E};
statement(S=#bic_break{}, E) ->
    {S, E};
statement(S=#bic_goto{}, E) ->
    {S, E};
statement(S=#bic_label{code=Code}, E) ->
    {Code1, E1} = statement(Code,E),
    {S#bic_label{code=Code1}, E1};
statement(S=#bic_if {test=Cond,then=Then,'else'=undefined}, E) ->
    {Cond1,C1,E1} = expr(Cond, E),
    if is_integer(Cond1), Cond1 =/= 0 ->
	    statement(Then,E1);
       is_integer(Cond1), Cond1 =:= 0 ->
	    {#bic_empty{},E1};
       true ->
	    {Then1,E2} = statement(Then,E1),
	    {S#bic_if{test=C1,then=Then1},E2}
    end;
statement(S=#bic_if {test=Cond,then=Then,'else'=Else}, E) ->
    {Cond1,C1,E1} = expr(Cond, E),
    if is_integer(Cond1), Cond1 =/= 0 ->
	    statement(Then,E1);
       is_integer(Cond1), Cond1 =:= 0 ->
	    statement(Else,E1);
       true ->
	    {Then1,E2} = statement(Then,E1),
	    {Else1,E2} = statement(Else,E1), %% match E2!
	    {S#bic_if{test=C1,then=Then1,'else'=Else1},E2}
    end;
statement(S=#bic_return{line=Ln,expr=Expr}, E) ->
    %% FuncName = current_function(E),
    ReturnType = bic:typeof(Expr),
    {R, X1, E1} = expr(Expr, E),
    case return_label(E1) of
	undefined ->
	    if is_number(R) ->
		    {S#bic_return{expr=R}, E1};
	       true ->
		    {S#bic_return{expr=X1}, E1}
	    end;
	Label ->
	    ReturnVar = return_variable(E),
	    ReturnId = #bic_id{line=Ln,type=ReturnType,name=ReturnVar},
	    Rhs = if is_number(R) -> R; true -> X1 end,
	    {#bic_compound{
		code=[expr_stmt(#bic_assign{line=Ln,
					    type=ReturnType,
					    op='=',
					    lhs=ReturnId, rhs=Rhs}),
		      #bic_goto{label=Label}]}, E1}
    end;
statement(S=#bic_typedef{}, E) ->
    %% FIXME: introduce type ! in env.
    {S, E};
statement(S=#bic_decl{name=Var,type=Type,value=Expr}, E) ->
    E1 = decl(Var,E,Type),
    {V1, X1, E2} = expr(Expr, E1),
    if is_number(V1) ->
	    {S#bic_decl{value=V1}, set_value(Var, E2, V1)};
       true ->
	    {S#bic_decl{value=X1}, E2}
    end;
statement(A=#bic_compound{code=Stmts},E) ->
    case compound(Stmts,E) of
	{[],E1} ->
	    {#bic_empty{}, E1};
	{[Stmt],E1} ->
	    if is_record(Stmt,bic_typedef);
	       is_record(Stmt,bic_decl) ->
		    {#bic_empty{}, E1};
	       true ->
		    {Stmt,E1}
	    end;
	{Stmts1,E1} when is_list(Stmts1) ->
	    {A#bic_compound{code=Stmts1},E1}
    end.

compound(Code, E) when is_list(Code) ->
    E0 = push_env(E),
    {Code1, E1} = statement_list_(Code, E0),
    {Code1, pop_env(E1)}.

statement_list(List, E) ->
    case statement_list_(List,E) of
	{[], E1} -> {#bic_empty{}, E1};
	{[H],E1} -> {H, E1};
	{List1,E1} -> {List1,E1}
    end.

statement_list_([H|T], E) ->
    case statement(H,E) of
	{#bic_empty{}, E1} ->
	    statement_list_(T,E1);
	{H1,E1} ->
	    {T1,E2} = statement_list_(T,E1),
	    {[H1|T1],E2}
    end;
statement_list_([], E) ->
    {[],E}.

for(For=#bic_for{init=Init, test=Cond, update=Update, body=Code}, E) ->
    {_,Init1,E1} = expr(Init, E),
    for_(For,Cond,Update,Code,E1,[expr_stmt(Init1)]).

for_(For,Cond,Update,Code,E,Acc) ->
    {Cond1,C1,E1} = expr(Cond, E),
    if is_integer(Cond1), Cond1 =/= 0 ->
	    {Code1,E2} = statement(Code,E1),
	    {_U1,Update1,E3} = expr(Update,E2),
	    for_(For,Cond,Update,Code,E3,[expr_stmt(Update1),Code1|Acc]);
       is_integer(Cond1), Cond1 =:= 0 ->
	    {compound_stmt(lists:reverse(Acc)), E1};
       length(Acc) =:= 1 -> %% only init ignore unroll
	    E2 = unset_values(Code,E1),
	    {For, E2};
       true -> %% loop a bit
	    E2 = unset_values(Code,E),
	    Upd = expr_stmt(Update),
	    Code1 = rcat(#bic_while{test=C1,body=cat(Code,Upd)},Acc),
	    {compound_stmt(Code1), E2}
    end.

while(While=#bic_while{test=Cond, body=Code}, E) ->
    while_(While, Cond, Code, E, []).

while_(While, Cond, Code, E, Acc) ->
    {Cond1,C1,E1} = expr(Cond, E),
    if is_integer(Cond1), Cond1 =/= 0 ->
	    {Code1,E2} = statement(Code,E1),
	    while_(While, Cond, Code, E2, [Code1|Acc]);
       is_integer(Cond1), Cond1 =:= 0 ->
	    {compound_stmt(lists:reverse(Acc)), E1};
       Acc =:= [] ->
	    E2 = unset_values(Code,E1),
	    {While, E2};
       true ->
	    E2 = unset_values(Code,E),
	    Code1 = rcat(While#bic_while{test=C1,body=Code},Acc),
	    {compound_stmt(Code1), E2}
    end.

do(Do=#bic_do{body=Code, test=Cond}, E) ->
    do_(Do,Code, Cond, E, []).

do_(Do, Code, Cond, E, Acc) ->
    {Code1,E1} = statement(Code,E),
    {Cond1,C1,E2} = expr(Cond, E1),
    if is_integer(Cond1), Cond1 =/= 0 ->
	    do_(Do, Code, Cond, E2, [Code1|Acc]);
       is_integer(Cond1), Cond1 =:= 0 ->
	    {compound_stmt(lists:reverse([Code1|Acc])), E1};
       Acc =:= [] ->
	    E3 = unset_values(Code,E),
	    {Do, E3};
       true ->
	    E3 = unset_values(Code,E),
	    Code1 = rcat(Do#bic_do{body=Code,test=C1},Acc),
	    {compound_stmt(Code1), E3}
    end.

switch(Sw=#bic_switch{expr=X, body=Code}, E) ->
    switch(Sw, X, Code, E).

switch(Sw, X, Code, E) ->
    {Value,X1,E1} = expr(X, E),
    {Code1,E2} = statement(Code,E1),
    %% scan Code1 and lookfor matching case
    if is_integer(Value) ->
	    Code2 = switch_code(Value, Code1),
	    Code3 = #bic_compound{code=Code2},
	    %% fixme collect declarations from Code1 as well
	    statement(Code3,E1);
       true ->
	    {Sw#bic_switch{expr=X1,body=Code1}, E2}
    end.

switch_code(Value, Stmt) ->
    switch_match_(Value, code_sequence(Stmt), []).

switch_match_(Value, [#bic_case{expr=Value,code=Code}|Stmts], _Default) ->
    switch_break_(code_sequence(Code)++Stmts);
switch_match_(Value, [#bic_default{code=Default}|Stmts], _Default) ->
    switch_match_(Value, Stmts, code_sequence(Default)++Stmts);
switch_match_(Value, [_Stmt|Stmts], Default) ->
    switch_match_(Value, Stmts, Default);
switch_match_(_Value, [], Default) ->
    switch_break_(Default).

%% collect all statements until end/return/continue or break
switch_break_(Stmts) ->
    switch_break_(Stmts, []).

switch_break_([#bic_break{}|_], Acc) ->
    lists:reverse(Acc);
switch_break_([S=#bic_return{}|_], Acc) ->
    lists:reverse([S|Acc]);
%% fixme: handle goto ...
switch_break_([#bic_case{expr=_,code=Code}|Stmts], Acc) ->
    switch_break_(code_sequence(Code)++Stmts, Acc);
switch_break_([S|Stmts], Acc) ->
    switch_break_(Stmts, [S|Acc]);
switch_break_([], Acc) ->
    lists:reverse(Acc).

%% return a list of statments
code_sequence(#bic_compound{code=Code}) ->
    Code;
code_sequence(Stmt) ->
    [Stmt].

cat(Code1,Code2) ->
    if Code1 =:= [] -> Code2;
       Code2 =:= [] -> Code1;
       is_list(Code1) ->
	    if is_list(Code2) ->  Code1++Code2;
	       true -> Code1 ++ [Code2]
	    end;
       true ->
	    if is_list(Code2) ->  [Code1|Code2];
	       true -> [Code1,Code2]
	    end
    end.

rcat(Code1,Code2) ->
    if Code1 =:= [] ->
	    if Code2 =:= [] -> [];
	       is_list(Code2) ->  lists:reverse(Code2);
	       true -> Code2
	    end;
       is_list(Code1) ->
	    if Code2 =:= [] -> lists:reverse(Code1);
	       is_list(Code2) ->  lists:reverse(Code1++Code2);
	       true -> lists:reverse(Code1++[Code2])
	    end;
       true ->
	    if Code2 =:= [] -> Code1;
	       is_list(Code2) ->  lists:reverse([Code1|Code2]);
	       true -> [Code2,Code1]
	    end
    end.

%% convert expression to statement
expr_stmt(Expr) ->
    #bic_expr_stmt{line=bic:lineof(Expr),expr=Expr}.

compound_stmt([]) ->
    #bic_empty{};  %% line number?
compound_stmt(Stmt) when not is_list(Stmt) ->
    Stmt;
compound_stmt(Stmts=[Stmt|_]) ->
    #bic_compound{line=bic:lineof(Stmt),code=Stmts}.

-spec expr(bic_expr(), env()) ->
	  {value(), bic_expr(), env()}.

expr(undefined, E) ->
    {undefined,undefined,E};
expr(Const,E) when is_number(Const) ->
    {Const,Const,E};
expr(X=#bic_constant{base=B,value=V},E) when is_integer(B) -> 
    {V,X,E};
expr(X=#bic_constant{base=float,value=V},E) -> 
    {V,X,E};
expr(X=#bic_id{name=Vx}, E) ->
    {value(Vx, E, X), X, E};
expr(X=#bic_call{func=Func=#bic_id{name=Name},args=Args}, E0) ->
    {Actuals,_AXs,E1} = expr_list(Args, E0),
    Function = get_function(Name,E1),
    case Function of
	#bic_function{type=RType,params=Formals,body=Code} ->
	    F0 = new_env(maps:get(global,E1),maps:get(opts,E1)),
	    F01 = push_env(F0),
	    F1  = set_current_function(Name, F01),
	    %% FIXME move ActualRef to "parent" scope! (push code?)
	    {ActualRef,F20} = unique(Name++"_ref_", F1),
	    {FuncValue,F21} = unique(Name++"_value_", F20),
	    {ReturnLabel,F2} = unique(Name++"_label_", F21),

	    F30 = decl(ActualRef,F2,#bic_pointer{type=RType}),
	    F31 = decl(FuncValue,F30,RType),
	    F32 = set_return_variable(FuncValue, F31),
	    F3 = set_return_label(ReturnLabel, F32),
	    F4 = lists:foldl(fun({#bic_decl{name=Var,type=Type},Value}, G0) ->
				     G1 = decl(Var, G0, Type),
				     if is_number(Value) ->
					     set_value(Var, G1, Value);
					true ->
					     %% maybe declare passed array size?
					     G1
				     end
			     end, F3,
			     lists:zip(Formals,Actuals)),
	    Lhs = #bic_unary{op='*',arg=#bic_id{
					   type=#bic_pointer{type=RType},
					   name=ActualRef}},
	    Rhs = #bic_id{name=FuncValue,type=RType},
	    Assign = #bic_assign{type=RType,op='=',lhs=Lhs,rhs=Rhs},
	    Code0 = 
		[#bic_decl{name=FuncValue,type=RType}] ++ 
		Code ++
		[#bic_label{name=ReturnLabel,code=expr_stmt(Assign)}],
	    {Code1,F5} = compound(Code0, F4),
	    G2 = maps:get(global,F5),  %% preserve global updates
	    E00 = push_code(#bic_compound{code=Code1}, E0),  %% generate code
	    case RType of
		#bic_type{type=void} ->
		    {undefined,undefined,E00#{ global => G2}};
		_ ->
		    {undefined,Rhs,E00#{ global => G2}}
	    end;
	_ ->
	    {undefined,X#bic_call{func=Func, args=Actuals}, E1}
    end;
expr(X=#bic_binary{op=',',arg1=A,arg2=B},E) ->
    {_Av,A1,E1} = expr(A,E),
    {Bv,B1,E2} = expr(B,E1),
    X1 = X#bic_binary{arg1=A1,arg2=B1},
    if is_number(Bv) ->
	    {Bv,X1,E2};
       true ->
	    {undefined,X1,E2}
    end;
expr(X=#bic_binary{op='[]',arg1=Var,arg2=Index},E) ->
    {I,Index1,E1} = expr(Index,E),
    case Var of
	#bic_id{name=Vx} when is_integer(I) ->
	    Value = value({Vx,I}, E, undefined),
	    {Value,X#bic_binary{arg2=I},E1};
	Map when is_map(Map) -> %% FIXME: eval Var to map..
	    {maps:get(Index1, Map),E1};
	_V1 ->
	    {undefined,X#bic_binary{arg2=Index1},E1}
    end;
expr(X=#bic_unary{op='&',arg=A}, E) ->
    case A of
	#bic_binary{op='[]',arg2=Index} ->
	    {I,Index1,E1} = expr(Index,E),
	    if is_integer(I) ->
		    {X#bic_unary{arg=A#bic_binary{arg2=I}}, E1};
	       true ->
		    {X#bic_unary{arg=A#bic_binary{arg2=Index1}}, E1}
	    end;
	_ -> %% fix more lhs expressions
	    {undefined, X, E}
    end;

%% fixme: must be transformed
expr(X=#bic_unary{op='+++',line=Ln,type=Type,arg=A}, E) -> %% postfix x++
    {A1,Ref,E1} = lhs_ref(A, E),
    V = value(Ref, E1, A1),
    if is_number(V) ->
	    E2 = add_value(Ref, E1, 1),
	    {V,#bic_assign{line=Ln,type=Type,op='=',lhs=A1,rhs=V}, E2};
       true ->
	    {undefined,X#bic_unary{arg=A1}, E1}
    end;
expr(X=#bic_unary{op='---',line=Ln,type=Type,arg=A}, E) ->  %% postif x--
    {A1,Ref,E1} = lhs_ref(A, E),
    V = value(Ref, E1, A1),
    if is_number(V) ->
	    E2 = add_value(Ref, E1, -1),
	    {V,#bic_assign{line=Ln,type=Type,op='=',lhs=A1,rhs=V}, E2};
       true ->
	    {undefined,X#bic_unary{arg=A1}, E1}
    end;
expr(X=#bic_unary{op='++',line=Ln,type=Type,arg=A}, E) -> %% prefix ++x
    {A1,Ref,E1} = lhs_ref(A, E),
    V = value(Ref, E1, A1),
    if is_number(V) ->
	    E2 = add_value(Ref, E1, 1),
	    {V+1,#bic_assign{line=Ln,type=Type,op='=',lhs=A1,rhs=(V+1)}, E2};
       true ->
	    {undefined,X#bic_unary{arg=A1}, E1}
    end;
expr(X=#bic_unary{op='--',line=Ln,type=Type,arg=A}, E) -> %% prefix --x
    {A1,Ref,E1} = lhs_ref(A, E),
    V = value(Ref, E1, A1),
    if is_number(V) ->
	    E2 = add_value(Ref, E1, -1),
	    {V-1,#bic_assign{line=Ln,type=Type,op='=',lhs=A1,rhs=(V-1)}, E2};
       true ->
	    {undefined,X#bic_unary{arg=A1}, E1}
    end;

expr(#bic_unary{op=typeof,arg=A}, E) ->
    %% FIXME!!!
    {undefined,bic:typeof(A), E};

expr(X=#bic_unary{op=sizeof,arg=Type}, E) when is_record(Type, bic_type) ->
    Opts = maps:get(opts, E, #{}),
    Model = maps:get(model, Opts, 0),
    Size = bic_sizeof:sizeof(Type, Model),
    {Size,X,E};
expr(X=#bic_unary{op=sizeof,arg=A}, E) -> %% expression like var
    {_Value,A1,E1} = expr(A,E),
    Opts = maps:get(opts, E, #{}),
    Model = maps:get(model, Opts, 0),
    Size = bic_sizeof:sizeof(bic:typeof(A1),Model),
    {Size,X#bic_unary{arg=A1},E1};

expr(#bic_unary{op='*',arg=#bic_unary{op='&',arg=A}}, E) -> %% short cut *&var
    expr(A, E);

expr(X=#bic_unary{op=Op,arg=A}, E) ->
    {V1,A1,E1} = expr(A,E),
    case is_number(V1) of
	true ->
	    Value = case Op of
			'+' -> +V1;
			'-' -> -V1;
			'~' -> bnot V1;
			'!' -> bool(V1 =/= 0)
		    end,
	    {Value,A1,E1};
	false ->
	    {V1,X#bic_unary{arg=A1}, E1}
    end;
expr(X=#bic_assign{op='=',lhs=Lhs,rhs=Rhs},E) ->
    {V0,Rhs1,E1} = expr(Rhs, E),
    {Lhs1,VRef,E2} = lhs_ref(Lhs, E1),
    if is_number(V0) ->
	    E3 = set_value(VRef, E2, V0),
	    {V0,X#bic_assign{lhs=Lhs1,rhs=V0}, E3};
       true ->
	    E3 = unset_value(VRef, E2),
	    {V0,X#bic_assign{lhs=Lhs1,rhs=Rhs1},E3}
    end;
expr(X=#bic_assign{op=Op,lhs=Lhs,rhs=Rhs},E) -> 
    {V0,Rhs1,E1} = expr(Rhs, E),
    {Lhs1,VRef,E2} = lhs_ref(Lhs, E1),
    V1 = value(VRef, E, undefined),
    case is_number(V0) andalso is_number(V1) of
	true ->
	    case Op of
		'+=' ->
		    V = V0+V1,
		    E3 = set_value(VRef, E2, V),
		    {V,X#bic_assign{op='=',lhs=Lhs1,rhs=V}, E3};
		'-=' ->
		    V = V0-V1,
		    E3 = set_value(VRef, E2, V),
		    {V,X#bic_assign{op='=',lhs=Lhs1,rhs=V}, E3};
		'*=' ->
		    V = V0*V1,
		    E3 = set_value(VRef, E2, V),
		    {V,X#bic_assign{op='=',lhs=Lhs1,rhs=V}, E3};
		'/=' ->
		    V = if is_integer(V0), is_integer(V1) -> V0 div V1;
			   true -> V0 / V1
			end,
		    E3 = set_value(VRef, E2, V),
		    {V,X#bic_assign{op='=',lhs=Lhs1,rhs=V}, E3};
		'%=' ->
		    V = V0 rem V1,
		    E3 = set_value(VRef, E2, V),
		    {V,X#bic_assign{op='=',lhs=Lhs1,rhs=V}, E3};
		'<<=' ->
		    V = V0 bsl V1,
		    E3 = set_value(VRef, E2, V),
		    {V,X#bic_assign{op='=',lhs=Lhs1,rhs=V}, E3};
		'>>=' ->
		    V = V0 bsr V1,
		    E3 = set_value(VRef, E2, V),
		    {V,X#bic_assign{op='=',lhs=Lhs1,rhs=V}, E3};
		'&=' ->
		    V = V0 band V1,
		    E3 = set_value(VRef, E2, V),
		    {V,X#bic_assign{op='=',lhs=Lhs1,rhs=V}, E3};
		'^=' ->
		    V = V0 bxor V1,
		    E3 = set_value(VRef, E2, V),
		    {V,X#bic_assign{op='=',lhs=Lhs1,rhs=V}, E3};
		'|=' ->
		    V = V0 bor V1,
		    E3 = set_value(VRef, E2, V),
		    {V,X#bic_assign{op='=',lhs=Lhs1,rhs=V}, E3}
	    end;
	false ->
	    {undefined,X#bic_assign{lhs=Lhs1,rhs=Rhs1}, E2}
    end;
expr(X=#bic_binary{op='&&',arg1=A,arg2=B}, E) ->
    case expr(A,E) of
	Ar = {0,_,_} ->
	    Ar;
	{1,_,E1} ->
	    expr(B,E1);
	{Av,A1,E1} ->
	    case expr(B,E1) of
		Br = {0,_,_} -> Br;
		{1,_B1,E2} -> {Av,A1,E2};
		{_Bv,B1,E2} ->
		    {undefined,X#bic_binary{arg1=A1,arg2=B1},E2}
	    end
    end;
expr(X=#bic_binary{op='||',arg1=A,arg2=B}, E) ->
    case expr(A,E) of
	Ar = {1,_,_} ->
	    Ar;
	{0,_,E1} ->
	    expr(B,E1);
	{Av,A1,E1} ->
	    case expr(B,E1) of
		Br = {1,_,_} -> Br;
		{0,_B1,E2} -> {Av,A1,E2};
		{_Bv,B1,E2} ->
		    {undefined,X#bic_binary{arg1=A1,arg2=B1},E2}
	    end
    end;
expr(X=#bic_binary{op=cast,arg1=Type,arg2=A}, E) ->
    {V1,A1,E1} = expr(A,E),
    X1 = X#bic_binary{arg2=A1},
    case is_number(V1) of
	true ->
	    case bic:is_float_type(Type) of
		true -> {float(V1),X1,E1};
		false -> {trunc(V1),X1,E1}
	    end;
	false ->
	    {V1,X1,E1}
    end;
expr(X=#bic_binary{op=Op,arg1=A,arg2=B}, E) ->
    {V1,A1,E1} = expr(A,E),
    {V2,B1,E2} = expr(B,E1),
    X1 = X#bic_binary{arg1=A1,arg2=B1},
    case is_number(V1) andalso is_number(V2) of
	true ->
	    Value = case Op of
			'+' -> V1 + V2;
			'-' -> V1 - V2;
			'*' -> V1 * V2;
			'/' when V2 == 0 ->
			    %% fixme: warn about division by zero
			    undefined;
			'/' when is_integer(V1), is_integer(V2), V2 =/= 0 ->
			    V1 div V2;
			'/' -> V1 / V2;
			'%' when V2 =:= 0 ->
			    %% fixme: warn about division by zero
			    undefined;
			'%' when V2 =/= 0 ->
			    V1 rem V2;
			'&' -> V1 band V2;
			'|' -> V1 bor V2;
			'^' -> V1 bxor V2;
			'<<' -> V1 bsl V2;
			'>>' -> V1 bsr V2;
			'<' -> bool(V1 < V2);
			'<=' -> bool(V1 =< V2);
			'>' -> bool(V1 > V2);
			'>=' -> bool(V1 >= V2);
			'==' -> bool(V1 =:= V2);
			'!=' -> bool(V1 =/= V2)
		    end,
	    {Value,X1,E2};
	false -> 
	    {undefined,X1,E2}
    end;
expr(X=#bic_ifexpr{test=C,then=A,'else'=B},E) ->
    case expr(C,E) of
	{1,_,E1} -> expr(A,E1);
	{0,_,E1} -> expr(B,E1);
	{_,C1,E1} ->
	    {_,A1,E3} = expr(A,E1),
	    {_,B1,E3} = expr(B,E1),  %% match E3!!
	    {undefined,X#bic_ifexpr{test=C1,then=A1,'else'=B1},E3}
    end.

expr_list(As, E) ->
    expr_list_(As, E, [], []).
    
expr_list_([A|As], E, Acc, Vs) ->
    {V1,A1,E1} = expr(A, E),
    expr_list_(As, E1, [A1|Acc],[V1|Vs]);
expr_list_([], E, Acc, Vs) ->
    {lists:reverse(Vs),lists:reverse(Acc), E}.

%% currently supported LHS simple var and simple array
lhs_ref(Lhs=#bic_id{name=V}, E) ->
    {Lhs,V,E};
lhs_ref(Lhs=#bic_binary{op='.',arg1=#bic_id{name=V},arg2=Field}, E) ->
    {Lhs,{V,Field},E};
lhs_ref(Lhs=#bic_binary{op='[]',arg1=#bic_id{name=V},arg2=Index}, E) ->
    {Index1,I1,E1} = expr(Index, E),
    if is_integer(Index1) ->
	    {Lhs#bic_binary{arg2=Index1},{V,Index1},E1};
       true ->
	    {Lhs#bic_binary{arg2=I1},{V,Index1},E1}
    end;
lhs_ref(#bic_unary{op='*',arg=#bic_unary{op='&',arg=A}},E) ->
    lhs_ref(A, E);
lhs_ref(Lhs=#bic_unary{op='*',arg=#bic_id{name=V}},E) ->
    {Lhs,{V,'*'},E}.


bool(true) -> 1;
bool(false) -> 0.

-spec new_env() -> env().

new_env() ->
    new_env(#{}).

-spec new_env(Options::map()) -> env().

new_env(Opts) ->
    new_env(#{}, Opts).

-spec new_env(Globals::map(),Options::map()) -> env().

new_env(Globals,Opts) ->
    #{ global => Globals, stack => [], opts => Opts, code => [], unique => 1 }.

push_env(E=#{ stack := Stack }) ->
    E#{ stack => [#{}|Stack] }.

pop_env(E=#{ stack := [_|Stack] }) ->
    E#{ stack => Stack }.

push_code(Code, E = #{ code := Cs }) ->
    %% fixme handle various special cases
    E# { code => [Code|Cs]}.

pop_code(E = #{ code := Cs }) ->
    {lists:reverse(Cs), E#{ code => [] }}.

unique(BaseName, E = #{ unique := U }) ->
    unique(BaseName, U, E).

unique(BaseName, U, E) ->
    Name = BaseName ++ integer_to_list(U),
    case type(Name, E) of
	undefined -> {Name, E#{ unique => U+1 }};
	_Type -> unique(BaseName, U+1, E)
    end.

current_function(#{ current_function := Name }) -> Name.

return_variable(#{ return_variable := Name }) -> Name.

return_label(#{ return_label := Name }) -> Name;
return_label(#{ }) -> undefined.

set_current_function(Name, Env) ->
    Env#{ current_function => Name }.

set_return_variable(Name, Env) ->
    Env#{ return_variable => Name }.

set_return_label(Name, Env) ->
    Env#{ return_label => Name }.

%% 
decl(Var, E = #{ global := Global, stack := Stack }, Type) ->
    case Stack of
	[] ->
	    ?dbg("declare global ~p, type=~p\n", [Var,Type]),
	    E#{ global => decl_(Var, Global, Type) };
	[Local|Stack1] ->
	    ?dbg("declare local ~p, type=~p\n", [Var,Type]),
	    E#{ stack => [decl_(Var, Local, Type)|Stack1]}
    end.

decl_(Var, Scope, Type) ->
    Scope#{ {type,Var}  => Type }.

%% if loop unroll fail then variables in body etc need
%% to be unmarked (set to undefined) since the value are 
%% unknown.
unset_values(Code, E) ->
    {_,E1} = 
	bic_transform:fold(
	  fun(F=#bic_id{name=Var}, Ei) ->
		  try unset_value(Var, Ei) of
		      Eii -> {F, Eii}
		  catch
		      error:{variable_not_found,_} ->
			  {F,Ei}
		  end;
	     (F, Ei) ->
		  {F, Ei}
	  end, E, Code),
    E1.

-spec type(Var::string() | {string(),[integer()]}, env()) ->
	  bic_type().

%% NOTE! variable may be set to undefined! to signal that they are unset
%% so undefined variables are either undefined or set to undefined

type(Var, #{ global:=G, stack:=Stack}) ->
    type_(Var, Stack, G).

type_(Var, [Local|Stack], G) ->
    case maps:get({type,Var},Local,undefined) of
	undefined -> type_(Var,Stack,G);
	Type -> Type
    end;
type_(Var, [], G) ->
    maps:get({type,Var},G,undefined).


-spec get_function(Var::string() | {Var::string(),Index::[integer()]}, env()) ->
	  bic_function() | undefined.

get_function(Name, Env) ->
    value(Name, Env, undefined).

-spec value(Var::string() | {Var::string(),Index::[integer()]},
	    env(), value()) ->
	  value().

%% NOTE! variable may be set to undefined! to signal that they are unset
%% so undefined variables are either undefined or set to undefined

value(Var, #{ global:=G, stack:=Stack}, Default) ->
    value_(Var, Stack, G, Default).

value_(Var, [Local|Stack], G, Default) ->
    case maps:get({value,Var},Local,undefined) of
	undefined -> value_(Var,Stack,G,Default);
	Value -> Value
    end;
value_(Var, [], G, Default) ->
    case maps:get({value,Var},G,undefined) of
	undefined -> Default;
	Value -> Value
    end.    

%% store function
-spec set_function(VRef::vref(), env(), bic_function()) ->
	  env().

set_function(VRef, Env, Function=#bic_function{}) ->
    set_value(VRef, Env, Function).

%% set value
-spec set_value(VRef::vref(), env(), value()) ->
	  env().

set_value(VRef, Env, Value) ->
    ?dbg("set_value: ~p = ~p\n", [VRef,Value]),
    {_,_,Env1} = update_value(VRef, Env,
			    fun(_Value,_Type) -> Value end),
    Env1.

-spec unset_value(VRef::string() | {string(),[integer()]}, env()) ->
	  env().

unset_value(VRef, Env) ->
    ?dbg("unset_value: ~p\n", [VRef]),
    {_,_,Env1} = update_value(VRef, Env, 
			    fun(_Value,_Type) -> undefined end),
    Env1.

-spec add_value(VRef::string() | {string(),[integer()]}, env(), value()) ->
	  env().

add_value(VRef, Env, Value) ->
    ?dbg("add_value: ~w to ~p\n", [Value, VRef]),
    Update = fun(PrevValue,_Type) -> PrevValue + Value end,
    {_,_,Env1} = update_value(VRef, Env, Update),
    Env1.

%% update value references in local and global 
%% value environments.

-spec update_value(VRef::vref(), env(), 
		   Update::fun((Old::value(),bic_type()) -> New::value())) ->
	  {New::value(), Old::value(), env()}.

update_value(VRef, E=#{ global := G, stack := Stack}, Update) ->
    ?dbg("update_value: ~p\n", [VRef]),
    update_value_(VRef, Stack, [], G, E, Update).


update_value_(VRefI={VRef,_Index}, [Local|Stack], Stack1, G, E, Update) ->
    case maps:get({type,VRef},Local,undefined) of
	undefined ->
	    update_value_(VRef,Stack,[Local|Stack1],G,E,Update);
	Type -> %% check array size?
	    Old = maps:get({value,VRefI},Local,undefined),
	    New = Update(Old, Type),
	    ?dbg("updated local ~p~w from ~w to ~w\n", [VRef,_Index,Old,New]),
	    Local1 = Local#{{value,VRefI} => New },
	    Stack2 = lists:reverse(Stack1,[Local1|Stack]),
	    {New, Old, E#{ stack => Stack2 }}
    end;
update_value_(VRef, [Local|Stack], Stack1, G, E, Update) ->
    case maps:get({type,VRef},Local,undefined) of
	undefined ->
	    update_value_(VRef,Stack,[Local|Stack1],G,E,Update);
	Type ->
	    Old = maps:get({value,VRef},Local,undefined),
	    New = Update(Old, Type),
	    ?dbg("updated local ~p from ~w to ~w\n", [VRef,Old,New]),
	    Local1 = Local#{{value,VRef} => New },
	    Stack2 = lists:reverse(Stack1,[Local1|Stack]),
	    {New, Old, E#{ stack => Stack2 }}
    end;
update_value_(VRefI={VRef,_Index}, [], _RStack, Global, E, Update) ->
    case maps:get({type,VRef},Global,undefined) of
	undefined -> 
	    error({varieble_not_found, VRef}),
	    {undefined, undefined, E};
	Type ->
	    Old = maps:get({value,VRefI},Global,undefined),
	    New = Update(Old, Type),
	    ?dbg("updated global ~p~w from ~w to ~w\n", [VRef,_Index,Old,New]),
	    Global1 = Global#{{value,VRefI} => New },
	    {New, Old, E#{ global => Global1 }}
    end;
update_value_(VRef, [], _RStack, Global, E, Update) ->
    case maps:get({type,VRef},Global,undefined) of
	undefined -> 
	    error({variable_not_found, VRef}),
	    {undefined, undefined, E};
	Type ->
	    Old = maps:get({value,VRef},Global,undefined),
	    New = Update(Old, Type),
	    ?dbg("updated global ~p from ~w to ~w\n", [VRef,Old,New]),
	    Global1 = Global#{{value,VRef} => New },
	    {New, Old, E#{ global => Global1 }}
    end.
