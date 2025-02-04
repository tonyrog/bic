%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    Some transforms over C code
%%% @end
%%% Created : 21 Apr 2020 by Tony Rogvall <tony@rogvall.se>

-module(bic_transform).

-compile(export_all).

-export([fold/3, fold_list/3]).
-export([used/1, calls/1]).

-include_lib("../include/bic.hrl").

-define(dbg(F,A), ok).

-type form() :: bic_expr() | bic_statement() |
		bic_declaration() | bic_forms().

-spec used(form()) -> [ Var::string() ].
	  
used(Form) ->
    {_,Set} = fold(fun(F=#bic_id{name=X}, Set) ->
			   {F, Set#{ X => true }};
		      (F, Set) ->
			   {F, Set}
		   end, #{}, Form),
    maps:fold(fun(Var,true,A) -> [Var|A] end, [], Set).

-spec calls(form()) -> [Call::string()].

calls(Form) ->
    {_,Set} = fold(fun
		       (F=#bic_call{func=#bic_id{name=X}}, Set) ->
			   {F, Set#{ X => true }};
		       (F, Set) ->
			   {F, Set}
		   end, #{}, Form),
    maps:fold(fun(Call,true,A) -> [Call|A] end, [], Set).


%% dive into expression and replace / remove statemets/expressions...

%%fold(Fun, Acc0, Form) when is_list(Form) ->
%%    fold_compound(Fun,Acc0,Line,Form);
fold(Fun, Acc0, Form) ->
    case Form of
	undefined ->
	    {undefined,Acc0};
	%% Expression
	Const when is_number(Const) ->
	    Fun(Form, Acc0);
	#bic_id{} ->
	    Fun(Form, Acc0);
	#bic_constant{} ->
	    Fun(Form,Acc0);
	#bic_unary{arg=A} ->
	    {A1,Acc1} = fold(Fun, Acc0, A),
	    Form1 = Form#bic_unary{arg=A1},
	    Fun(Form1,Acc1);
	#bic_binary{arg1=A,arg2=B} ->
	    {A1,Acc1} = fold(Fun, Acc0, A),
	    {A2,Acc2} = fold(Fun, Acc1, B),
	    Form1 = Form#bic_binary{arg1=A1,arg2=A2},
	    Fun(Form1,Acc2);
	#bic_call{args=As} ->
	    {As1,Acc1} = fold_list(Fun,Acc0,As),
	    Form1 = Form#bic_call{args=As1},
	    Fun(Form1,Acc1);
	#bic_ifexpr{test=C,then=T,'else'=E} ->
	    {C1,Acc1} = fold(Fun, Acc0, C),
	    {T1,Acc2} = fold(Fun, Acc1, T),
	    {E1,Acc3} = fold(Fun, Acc2, E),
	    Form1 = Form#bic_ifexpr{test=C1,then=T1,'else'=E1},
	    Fun(Form1,Acc3);
	#bic_assign{lhs=Lhs, rhs=Rhs} ->
	    {Rhs1,Acc1} = fold(Fun, Acc0, Rhs),
	    {Lhs1,Acc2} = fold(Fun, Acc1, Lhs),
	    Form1 = Form#bic_assign{lhs=Lhs1,rhs=Rhs1},
	    Fun(Form1,Acc2);
	%% Types
	#bic_typeid{} -> 
	    Fun(Form,Acc0);
	#bic_type{type=T} ->
	    if is_atom(T) ->
		    Fun(Form,Acc0);
	       true ->
		    {T1,Acc1} = Fun(T,Acc0),
		    Form1 = Form#bic_type{type=T1},
		    Fun(Form1,Acc1)
	    end;
	#bic_enum{} -> 
	    Fun(Form,Acc0);
	#bic_pointer{type=T} ->
	    {T1,Acc1} = Fun(T,Acc0),
	    Form1 = Form#bic_pointer{type=T1},
	    Fun(Form1,Acc1);
	#bic_array{type=T,dim=[]} ->
	    {T1,Acc1} = Fun(T,Acc0),
	    Form1 = Form#bic_array{type=T1},
	    Fun(Form1,Acc1);
	#bic_array{type=T,dim=D} ->
	    {T1,Acc1} = Fun(T,Acc0),
	    {D1,Acc2} = Fun(D,Acc1),
	    Form1 = Form#bic_array{type=T1,dim=D1},
	    Fun(Form1,Acc2);
	#bic_struct{} -> %% FIXME: traverse
	    Fun(Form,Acc0);
	#bic_union{} -> %% FIXME: traverse
	    Fun(Form,Acc0);
	#bic_fn{} ->
	    Fun(Form,Acc0);
	%% DECLS
	#bic_function{line=Line,type=T,params=Ps,body=B} ->
	    {T1,Acc1} = fold(Fun, Acc0, T),
	    {Ps1,Acc2} = fold_list(Fun, Acc1, Ps),
	    {B1,Acc3} = fold_compound(Fun, Acc2, Line, B),
	    Form1 = Form#bic_function{type=T1,params=Ps1,body=B1},
	    Fun(Form1,Acc3);
	#bic_decl{type=T,value=V} ->
	    {T1,Acc1} = fold(Fun, Acc0, T),
	    {V1,Acc2} = fold(Fun, Acc1, V),
	    Form1 = Form#bic_decl{type=T1,value=V1},
	    Fun(Form1,Acc2);
	%% STATEMENTS
	#bic_expr_stmt{expr=Expr} ->
	    {Expr1,Acc1} = fold(Fun, Acc0, Expr),
	    Form1 = Form#bic_expr_stmt{expr=Expr1},
	    Fun(Form1,Acc1);
	#bic_for{init=I,test=T,update=U,body=B} ->
	    {I1,Acc1} = fold(Fun, Acc0, I),
	    {T1,Acc2} = fold(Fun, Acc1, T),
	    {U1,Acc3} = fold(Fun, Acc2, U),
	    {B1,Acc4} = fold(Fun, Acc3, B),
	    Form1 = Form#bic_for{init=I1,test=T1,update=U1,body=B1},
	    Fun(Form1,Acc4);
	#bic_while{test=C,body=B} ->
	    {C1,Acc1} = fold(Fun, Acc0, C),
	    {B1,Acc2} = fold(Fun, Acc1, B),
	    Form1 = Form#bic_while{test=C1,body=B1},
	    Fun(Form1,Acc2);
	#bic_do{test=C,body=B} ->
	    {B1,Acc1} = fold(Fun, Acc0, B),
	    {C1,Acc2} = fold(Fun, Acc1, C),
	    Form1 = Form#bic_do{test=C1,body=B1},
	    Fun(Form1,Acc2);
	#bic_if{test=C, then=T, 'else'=E } ->
	    {C1,Acc1} = fold(Fun, Acc0, C),
	    {T1,Acc2} = fold(Fun, Acc1, T),
	    {E1,Acc3} = fold(Fun, Acc2, E),
	    Form1 = Form#bic_if{test=C1,then=T1,'else'=E1},
	    Fun(Form1,Acc3);
	#bic_switch{expr=E,body=B} ->
	    {E1,Acc1} = fold(Fun, Acc0, E),
	    {B1,Acc2} = fold(Fun, Acc1, B),
	    Form1 = Form#bic_switch{expr=E1,body=B1},
	    Fun(Form1,Acc2);
	#bic_case{expr=E,code=B} ->
	    {E1,Acc1} = fold(Fun, Acc0, E),
	    {B1,Acc2} = fold(Fun, Acc1, B),
	    Form1 = Form#bic_case{expr=E1,code=B1},
	    Fun(Form1,Acc2);
	#bic_default{code=B} ->
	    {B1,Acc1} = fold(Fun, Acc0, B),
	    Form1 = Form#bic_default{code=B1},
	    Fun(Form1,Acc1);
	#bic_label{code=B} ->
	    {B1,Acc1} = fold(Fun, Acc0, B),
	    Form1 = Form#bic_default{code=B1},
	    Fun(Form1,Acc1);	    
	#bic_goto{} ->
	    Fun(Form,Acc0);
	#bic_continue{} ->
	    Fun(Form,Acc0);
	#bic_break{} ->
	    Fun(Form,Acc0);
	#bic_return{expr=E} ->
	    {E1,Acc1} = fold(Fun, Acc0, E),
	    Form1 = Form#bic_return{expr=E1},
	    Fun(Form1,Acc1);
	#bic_empty{} ->
	    Fun(Form,Acc0);
	#bic_compound{line=Line,code=Stmts} when is_list(Stmts) ->
	    {Stmts1,Acc1} = fold_compound(Fun,Acc0,Line,Stmts),
	    Form1 = Form#bic_compound{code=Stmts1},
	    Fun(Form1,Acc1)
    end.

fold_compound(_Fun, Acc, _Line, undefined) -> %% undefine body, declaration
    {undefined, Acc};
fold_compound(Fun, Acc, Line, Stmts) when is_list(Stmts) ->
    {_,Acc1} = Fun(#bic_begin{line=Line}, Acc),
    {Stmts1, Acc2} = fold_list(Fun, Acc1, Stmts),
    {_,Acc3} = Fun(#bic_end{line=Line}, Acc2),
    {Stmts1, Acc3}.

fold_list(Fun, Acc, As) ->
    fold_list_(Fun, Acc, As, []).

fold_list_(Fun, Acc0, [A|As], As1) ->
    case fold(Fun,Acc0,A) of
	{false,Acc1} ->
	    fold_list_(Fun,Acc1,As,As1);
	{#bic_empty{},Acc1} ->
	    fold_list_(Fun,Acc1,As,As1);
	{A1,Acc1} when is_list(A1) ->
	    fold_list_(Fun,Acc1,As,lists:reverse(A1,As1));
	{A1,Acc1} ->
	    fold_list_(Fun,Acc1,As,[A1|As1])
    end;
fold_list_(_Fun, Acc0, [], As1) ->
    {lists:reverse(As1), Acc0}.
