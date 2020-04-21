%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    Some transforms over C code
%%% @end
%%% Created : 21 Apr 2020 by Tony Rogvall <tony@rogvall.se>

-module(bic_transform).

-compile(export_all).

-include_lib("../include/bic.hrl").

%% make (local) variables uniq

variables([D|Ds]) ->
    case D of 
	#bic_function{} ->
	    D1 = function(D),
	    [D1 | variables(Ds)];
	_ ->
	    [D | variables(Ds)]
    end;
variables([]) ->
    [].

function(F=#bic_function{params=Params,body=Body}) ->
    {Params1,Scope} = decl_list(Params, #{}),
    Body1 = statement(Body, Scope),
    F#bic_function{params=Params1,body=Body1}.

statement(Body, Scope) when is_list(Body) ->
    {Body1, _Scope} = statement(Body, [], Scope),
    Body1;
statement(Stmt, Scope) ->
    {[Stmt1],_Scope} = statement([Stmt], [], Scope),
    Stmt1.

statement([Stmt|Stmts], Acc, Scope) ->
    case Stmt of
	#bic_decl{type=Type,value=undefined} ->
	    Stmt1 = Stmt#bic_decl{type=type(Type,Scope)},
	    {Stmt2,Scope1} = decl(Stmt1, Scope),
	    statement(Stmts, [Stmt2|Acc], Scope1);
	#bic_decl{type=Type,value=Value} ->
	    Stmt1 = Stmt#bic_decl{type=type(Type,Scope),
				  value=expr(Value,Scope)},
	    {Stmt2,Scope1} = decl(Stmt1, Scope),
	    statement(Stmts, [Stmt2|Acc], Scope1);
	#bic_for{init=Init,test=Test,update=Update,body=Body} ->
	    Body1 = statement(Body, Scope),
	    Stmt1 = Stmt#bic_for{init=expr(Init,Scope),
				 test=expr(Test,Scope),
				 update=expr(Update,Scope),
				 body=Body1},
	    statement(Stmts, [Stmt1|Acc], Scope);
	#bic_while{test=Test,body=Body} ->
	    Body1 = statement(Body, Scope),
	    Stmt1 = Stmt#bic_while{test=expr(Test,Scope),
				   body=Body1},
	    statement(Stmts, [Stmt1|Acc], Scope);
	#bic_do{test=Test,body=Body} ->
	    Body1 = statement(Body, Scope),
	    Stmt1 = Stmt#bic_do{test=expr(Test,Scope),
				body=Body1},
	    statement(Stmts, [Stmt1|Acc], Scope);
	#bic_if{test=Test, then=Then, else=undefined } ->
	    Then1 = statement(Then, Scope),
	    Stmt1 = Stmt#bic_if{test=expr(Test,Scope),
				then=Then1},
	    statement(Stmts, [Stmt1|Acc], Scope);
	#bic_if{test=Test, then=Then, else=Else } ->
	    Then1 = statement(Then, Scope),
	    Else1 = statement(Else, Scope),
	    Stmt1 = Stmt#bic_if{test=expr(Test,Scope),
				then=Then1, else=Else1},
	    statement(Stmts, [Stmt1|Acc], Scope);
	#bic_switch{expr=Expr,body=Body} ->
	    Body1 = statement(Body, Scope),
	    Stmt1 = Stmt#bic_switch{expr=expr(Expr,Scope),
				    body=Body1},
	    statement(Stmts, [Stmt1|Acc], Scope);
	#bic_case{expr=Expr,code=Code} ->
	    Code1 = statement(Code, Scope),
	    Stmt1 = Stmt#bic_case{expr=expr(Expr,Scope),
				  code=Code1},
	    statement(Stmts, [Stmt1|Acc], Scope);
	#bic_default{code=Code} ->
	    Code1 = statement(Code, Scope),
	    Stmt1 = Stmt#bic_default{code=Code1},
	    statement(Stmts, [Stmt1|Acc], Scope);
	#bic_label{code=Code} ->
	    Code1 = statement(Code, Scope),
	    Stmt1 = Stmt#bic_label{code=Code1},
	    statement(Stmts, [Stmt1|Acc], Scope);
	#bic_goto{} ->
	    statement(Stmts, [Stmt|Acc], Scope);
	#bic_continue{} ->
	    statement(Stmts, [Stmt|Acc], Scope);
	#bic_break{} ->
	    statement(Stmts, [Stmt|Acc], Scope);
	#bic_return{expr=undefined} ->
	    statement(Stmts, [Stmt|Acc], Scope);
	#bic_return{expr=Expr} ->
	    Stmt1 = Stmt#bic_return{expr=expr(Expr,Scope)},
	    statement(Stmts, [Stmt1|Acc], Scope);
	#bic_empty{} ->
	    statement(Stmts, [Stmt|Acc], Scope);
	Expr ->
	    Stmt1 = expr(Expr, Scope),
	    statement(Stmts, [Stmt1|Acc], Scope)
    end;
statement([], Acc, Scope) ->
    {lists:reverse(Acc), Scope}.

type(Type, Scope) ->
    case Type of
	#bic_typeid{} -> Type;
	#bic_type{type=Type1} ->
	    if is_atom(Type1) -> Type;
	       true -> Type#bic_type{type=type(Type1,Scope)}
	    end;
	#bic_enum{} -> Type;
	#bic_pointer{type=Type1} ->
	    Type#bic_pointer{type=type(Type1,Scope)};
	#bic_array{type=Type1,dim=[]} ->
	    Type#bic_pointer{type=type(Type1,Scope)};
	#bic_array{type=Type1,dim=Dim} ->
	    Type#bic_array{type=type(Type1,Scope),dim=expr(Dim,Scope)};
	#bic_struct{} -> Type;  %% Check!
	#bic_union{} -> Type;  %% Check!
	#bic_fn{} -> Type  %% Check!
    end.

expr(Expr, Scope) ->
    case Expr of
	#bic_id{name=Name} ->
	    case maps:find(Name, Scope) of
		error -> Expr;  %% global
		{ok,New} -> Expr#bic_id{name=New}
	    end;
	#bic_constant{} -> Expr;
	#bic_unary{arg=Arg} -> 
	    Expr#bic_unary{arg=expr(Arg,Scope)};
	#bic_binary{op=cast,arg1=Type,arg2=Arg} ->
	    Expr#bic_binary{arg1=type(Type,Scope),
			    arg2=expr(Arg,Scope)};
	#bic_binary{arg1=Arg1,arg2=Arg2} ->
	    Expr#bic_binary{arg1=expr(Arg1,Scope),
			    arg2=expr(Arg2,Scope)};
	#bic_call{func=Func,args=Args} ->
	    Expr#bic_call{func=expr(Func,Scope),
			  args=expr_list(Args,Scope)};
	#bic_ifexpr{test=Test,then=Then,else=Else} ->
	    Expr#bic_ifexpr{test=expr(Test,Scope),
			    then=expr(Then,Scope),
			    else=expr(Else,Scope)};
	#bic_assign{lhs=Lhs, rhs=Rhs} ->
	    Expr#bic_assign{lhs=expr(Lhs,Scope),
			    rhs=expr(Rhs,Scope)}
    end.

expr_list(ExprList, Scope) ->
    [expr(Expr,Scope) || Expr <- ExprList].

decl_list(Decls, Scope) ->
    decl_list_(Decls, [], Scope).

decl_list_([D|Ds], Acc, Scope) ->
    {D1,Scope1} = decl(D, Scope),
    decl_list_(Ds, [D1|Acc], Scope1);
decl_list_([], Acc, Scope) ->
    {lists:reverse(Acc), Scope}.

decl(D,  Scope) ->
    Old = D#bic_decl.name,
    New = create_variable(),
    {D#bic_decl{name=New}, Scope#{ Old => New }}.

create_variable() ->
    U = case get(unique) of
	    undefined -> 0;
	    U0 -> U0
	end,
    put(unique, U+1),
    [$v|integer_to_list(U+1)].

    
	     
