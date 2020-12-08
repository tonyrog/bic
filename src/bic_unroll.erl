%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    Code expansion/unrolling
%%% @end
%%% Created : 25 Nov 2020 by Tony Rogvall <tony@rogvall.se>

-module(bic_unroll).

-export([definitions/1, definitions/2]).
-export([new_env/0, push_env/1, pop_env/1]).

-compile(export_all).

-include_lib("bic/include/bic.hrl").

-define(dbg(F,A), ok).

-type value() :: undefined | integer() | float().
-type scope() :: #{ {value,VarName::string()} => value(),
		    {type,VarName::string()} => bic_type() }.
-type env() :: #{
		 global => scope(),
		 stack => [scope()]
		}.

-spec definitions(Ds::bic_definitions()) ->
	  bic_definitions().

definitions(Ds) ->
    definitions_(Ds, [], [], new_env()).

-spec definitions(Ds::bic_definitions(), FunctionNames::[string()]) ->
	  bic_definitions().

definitions(Ds, Fs) ->
    definitions_(Ds, Fs, [], new_env()).
    
definitions_([D|Ds],Fs,Acc,E0) ->
    case D of
	#bic_function{name=Name} ->
	    case (Fs =:= []) orelse lists:member(Name,Fs) of
		true ->
		    {D1,E1} = definition(D, E0),
		    definitions_(Ds,Fs,[D1|Acc],E1);
		false ->
		    E1 = set_value(Name, E0, D),
		    definitions_(Ds,Fs,Acc,E1)
	    end;
	_ ->
	    {D1,E1} = definition(D, E0),
	    definitions_(Ds,Fs,[D1|Acc],E1)
    end;
definitions_([], _Fs, Acc, E) ->
    {lists:reverse(Acc), E}.

definition(D=#bic_function{name=Fun, params=Params, body=Code}, E) ->
    E0 = push_env(E),
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
    D1 = D#bic_function{params=Params,body=Code1},
    {D1, set_value(Fun, E4, D1)};
definition(D=#bic_decl{name=Var,type=Type,value=Expr}, E) ->
    E1 = decl(Var,E,Type),
    {Expr1, E2} = expr(Expr, E1),
    {D#bic_decl{value=Expr1}, set_value(Var, E2, Expr1)}.

%% for (init, cond, update)
statement(S=#bic_for{},E) ->
    for(S,E);
statement(S=#bic_while{},E) ->
    while(S,E);
statement(S=#bic_do{},E) ->
    do(S,E);
statement(S=#bic_if {test=Cond,then=Then,else=undefined}, E) ->
    {Value1,E1} = expr(Cond, E),
    if is_integer(Value1), Value1 =/= 0; Value1 =:= true ->
	    statement(Then,E1);
       is_integer(Value1), Value1 =:= 0; Value1 =:= false ->
	    {#bic_empty{},E1};
       true ->
	    {Then1,E2} = statement(Then,E1),
	    {S#bic_if{test=Value1,then=Then1},E2}
    end;
statement(S=#bic_if {test=Cond,then=Then,else=Else}, E) ->
    {Value1,E1} = expr(Cond, E),
    if is_integer(Value1), Value1 =/= 0; Value1 =:= true ->
	    statement(Then,E1);
       is_integer(Value1), Value1 =:= 0; Value1 =:= false ->
	    statement(Else,E1);
       true ->
	    {Then1,E2} = statement(Then,E1),
	    {Else1,E2} = statement(Else,E1), %% match E2!
	    {S#bic_if{test=Value1,then=Then1,else=Else1},E2}
    end;
statement(S=#bic_return{expr=Expr}, E) ->
    {Expr1, E1} = expr(Expr, E),
    {S#bic_return{expr=Expr1}, E1};

statement(S=#bic_typedef{}, E) ->
    %% FIXME: introduce type ! in env.
    {S, E};
statement(S=#bic_decl{name=Var,type=Type,value=Expr}, E) ->
    E1 = decl(Var,E,Type),
    {Expr1, E2} = expr(Expr, E1),
    {S#bic_decl{value=Expr1}, set_value(Var, E2, Expr1)};

statement(Const=#bic_constant{}, E) ->
    {Const,E};
statement(A=#bic_assign{}, E) ->
    expr(A,E);
statement(A=#bic_call{}, E) ->
    expr(A,E);
statement(A=#bic_binary{}, E) ->
    expr(A,E);
statement(A=#bic_unary{}, E) ->
    expr(A,E);
statement(A=#bic_compound{code=Stmts},E) ->
    case compound(Stmts,E) of
	{[],E1} ->
	    {#bic_empty{}, E1};
	{Stmts1,E1} ->
	    {A#bic_compound{code=Stmts1},E1}
    end.

compound(Code, E) when is_list(Code) ->
    E0 = push_env(E),
    {Code1, E1} = statement_list(Code, E0),
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
    {Init1,E1} = statement(Init, E),
    for_(For,Cond,Update,Code,E1,[Init1]).

for_(For,Cond,Update,Code,E,Acc) ->
    {Cond1,E1} = expr(Cond, E),
    if is_integer(Cond1), Cond1 =/= 0 ->
	    {Code1,E2} = statement(Code,E1),
	    {Update1,E3} = statement(Update,E2),
	    for_(For,Cond,Update,Code,E3,[Update1,Code1|Acc]);
       is_integer(Cond1), Cond1 =:= 0 ->
	    {lists:reverse(Acc), E1};
       length(Acc) =:= 1 -> %% only init ignore unroll
	    {_,E2} = unset_values(Code,E1),
	    {For, E2};
       true -> %% loop a bit
	    {_, E2} = unset_values(Code,E),
	    {rcat(#bic_while{test=Cond,body=cat(Code,Update)},Acc), E2}
    end.

while(While=#bic_while{test=Cond, body=Code}, E) ->
    while_(While, Cond, Code, E, []).

while_(While, Cond, Code, E, Acc) ->
    {Cond1,E1} = expr(Cond, E),
    if is_integer(Cond1), Cond1 =/= 0 ->
	    {Code1,E2} = statement(Code,E1),
	    while_(While, Cond, Code, E2, [Code1|Acc]);
       is_integer(Cond1), Cond1 =:= 0 ->
	    {lists:reverse(Acc), E1};
       Acc =:= [] ->
	    {_,E2} = unset_values(Code,E1),
	    {While, E2};
       true ->
	    {_, E2} = unset_values(Code,E),
	    {rcat(While#bic_while{test=Cond,body=Code},Acc), E2}
    end.

do(Do=#bic_do{body=Code, test=Cond}, E) ->
    do_(Do,Code, Cond, E, []).

do_(Do, Code, Cond, E, Acc) ->
    {Code1,E1} = statement(Code,E),
    {Cond1,E2} = expr(Cond, E1),
    if is_integer(Cond1), Cond1 =/= 0 ->
	    do_(Do,Code, Cond, E2, [Code1|Acc]);
       is_integer(Cond1), Cond1 =:= 0 ->
	    {lists:reverse(Acc), E1};
       Acc =:= [] ->
	    {_,E3} = unset_values(Code,E),
	    {Do, E3};
       true ->
	    {_, E3} = unset_values(Code,E),
	    {rcat(Do#bic_do{body=Code,test=Cond},Acc), E3}
    end.

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


-spec expr(bic_expr(), env()) ->
	  {bic_expr()|value(), env()}.

%% meta expressions
expr(undefined, E) -> {undefined,E};
expr(Const,E) when is_number(Const) -> {Const,E};
expr(#bic_constant{base=B,value=V},E) when is_integer(B) -> {V,E};
expr(#bic_constant{base=float,value=V},E) -> {V,E};
expr(X=#bic_id{name=Vx}, E) ->
    Value = value(Vx, E, X),
    {Value, E};
expr(X=#bic_call{func=Func=#bic_id{name=Name},args=Args}, E0) ->
    {Actuals, E1} = expr_list(Args, E0),
    Function = value(Name,E1,undefined),
    case Function of
	#bic_function{params=Formals,body=Code} ->
	    %% assign only constants righ now
	    F0 = new_env(maps:get(global,E1)),
	    F1 = lists:foldl(fun({#bic_decl{name=Var,type=Type},Value}, EB0) ->
				     EB1 = decl(Var, EB0, Type),
				     set_value(Var, EB1, Value)
			     end, F0,
			     lists:zip(Formals,Actuals)),
	    {Code1,F2} = compound(Code, F1),
	    io:format("Code1 = ~p\n", [Code1]),
	    G2 = maps:get(global,F2),
	    {#bic_compound{code=Code1},E0#{ global => G2}};
	_ ->
	    {X#bic_call{func=Func, args=Actuals}, E1}
    end;
expr(X=#bic_binary{op=',',arg1=A,arg2=B},E) ->
    {A1,E1} = expr(A,E),
    {B1,E2} = expr(B,E1),
    if is_number(A1),is_number(B1) ->
	    {[A1,B1], E2};
       is_number(A1),is_list(B1) ->
	    {[A1|B1], E2};
       true ->
	    {X#bic_binary{arg1=A1,arg2=B1}, E2}
    end;
expr(X=#bic_binary{op='[]',arg1=Var,arg2=Index},E) ->
    {Index1,E1} = expr(Index,E),
    case Var of
	#bic_id{name=Vx} ->
	    X1 = X#bic_binary{arg2=Index1},
	    Value = value({Vx,Index1}, E, X1),
	    {Value,E1};
	Map when is_map(Map) -> %% FIXME: eval Var to map..
	    {maps:get(Index1, Map),E1};
	_V1 ->
	    {X#bic_binary{arg2=Index1},E1}
    end;
expr(X=#bic_unary{op='&',arg=A}, E) ->
    case A of
	#bic_binary{op='[]',arg2=Index} ->
	    {Index1,E1} = expr(Index,E),
	    {X#bic_unary{arg=A#bic_binary{arg2=Index1}}, E1};
	_ -> %% fix more lhs expressions
	    {X, E}
    end;

%% fixme: must be transformed
expr(X=#bic_unary{op='+++',arg=A}, E) ->
    {A1,Ref,E1} = lhs_ref(A, E),
    {X#bic_unary{arg=A1}, add_value(Ref, E1, 1)};
expr(X=#bic_unary{op='---',arg=A}, E) ->
    {A1,Ref,E1} = lhs_ref(A, E),
    {X#bic_unary{arg=A1}, add_value(Ref, E1, -1)};
%% fixme: must be transformed
expr(X=#bic_unary{op='++',arg=A}, E) ->
    {A1,Ref,E1} = lhs_ref(A, E),
    {X#bic_unary{arg=A1}, add_value(Ref, E1, 1)};
expr(X=#bic_unary{op='--',arg=A}, E) ->
    {A1,Ref,E1} = lhs_ref(A, E),
    {X#bic_unary{arg=A1}, add_value(Ref, E1, -1)};
%% fixme add sizeof/typeof
expr(X=#bic_unary{op=Op,arg=A}, E) ->
    {A1,E1} = expr(A,E),
    case is_number(A1) of
	true ->
	    Value = case Op of
			'+' -> +A1;
			'-' -> -A1;
			'~' -> bnot A1;
			'!' -> bool(A1 =/= 0)
		    end,
	    {Value, E1};
	false ->
	    {X#bic_unary{arg=A1}, E1}
    end;

expr(X=#bic_assign{op='=',lhs=Lhs,rhs=V},E) -> 
    {V1,E1} = expr(V, E),
    {Lhs1,Ref,E2} = lhs_ref(Lhs, E1),
    case is_number(V1) of
	true ->
	    {X#bic_assign{lhs=Lhs1,rhs=V1}, set_value(Ref, E2, V1)};
	false ->
	    {X#bic_assign{lhs=Lhs1,rhs=V1}, E2}
    end;
expr(X=#bic_assign{op='+=',lhs=Lhs,rhs=V},E) -> 
    {V1,E1} = expr(V, E),
    {Lhs1,Ref,E2} = lhs_ref(Lhs, E1),
    case is_number(V1) of
	true ->
	    {X#bic_assign{lhs=Lhs1,rhs=V1}, add_value(Ref, E2, V1)};
	false ->
	    {X#bic_assign{lhs=Lhs1,rhs=V1}, E2}
    end;

expr(X=#bic_binary{op=Op,arg1=A,arg2=B}, E) ->
    {A1,E1} = expr(A,E),
    {B1,E2} = expr(B,E1),
    case is_number(A1) andalso is_number(B1) of
	true ->
	    Value = case Op of
			'+' -> A1 + B1;
			'-' -> A1 - B1;
			'*' -> A1 * B1;
			'/' -> A1 div B1;
			'%' -> A1 rem B1;
			'&' -> A1 band B1;
			'|' -> A1 bor B1;
			'^' -> A1 bxor B1;
			'<<' -> A1 bsl B1;
			'>>' -> A1 bsr B1;
			'<' -> bool(A1 < B1);
			'<=' -> bool(A1 =< B1);
			'>' -> bool(A1 > B1);
			'>=' -> bool(A1 >= B1);
			'==' -> bool(A1 =:= B1);
			'!=' -> bool(A1 =/= B1);
			'&&' -> bool(A1=/=0 andalso B1=/=0);
			'||' -> bool(A1=/=0 orelse B1=/=0)
		    end,
	    {Value, E2};
	false -> 
	    {X#bic_binary{arg1=A1,arg2=B1}, E2}
    end;
expr(X=#bic_ifexpr{test=C,then=A,else=B},E) ->
    case expr(C,E) of
	{1,E1} -> expr(A,E1);
	{0,E1} -> expr(B,E1);
	{C1,E1} ->
	    {A1,E3} = expr(A,E1),
	    {B1,E3} = expr(B,E1),  %% match E3!!
	    {X#bic_ifexpr{test=C1,then=A1,else=B1},E3}
    end.

expr_list(As, E) ->
    expr_list_(As, E, []).
    
expr_list_([A|As], E, Acc) ->
    {A1,E1} = expr(A, E),
    expr_list_(As, E1, [A1|Acc]);
expr_list_([], E, Acc) ->
    {lists:reverse(Acc), E}.

%% currently supported LHS simple var and simple array
lhs_ref(Lhs=#bic_id{name=V}, E) ->
    {Lhs,V,E};
lhs_ref(Lhs=#bic_binary{op='[]',arg1=#bic_id{name=V},arg2=Index}, E) ->
    {Index1,E1} = expr(Index, E),
    {Lhs#bic_binary{arg2=Index},{V,Index1},E1}.

bool(true) -> 1;
bool(false) -> 0.

%% scope keeps current declarations
%% store keeps previous values
%% stack keeps store stack for reference (not used right now)

-spec new_env() -> env().

new_env() ->
    new_env(#{}).

new_env(Globals) ->
    #{ global => Globals, stack => [] }.

push_env(E=#{ stack := Stack }) ->
    E#{ stack => [#{}|Stack] }.

pop_env(E=#{ stack := [_|Stack] }) ->
    E#{ stack => Stack }.

%% 
decl(Var, E = #{ global := Global, stack := Stack }, Type) ->
    case Stack of
	[] ->
	    E#{ global => decl_(Var, Global, Type) };
	[Local|Stack1] ->
	    E#{ stack => [decl_(Var, Local, Type)|Stack1]}
    end.

decl_(Var, Scope, Type) ->
    Scope#{ {type,Var}  => Type }.

%% if loop unroll fail then variables in body etc need
%% to be unmarked (set to undefined) since the value are 
%% unknown.
unset_values(Code, E) ->
    bic_transform:fold(
      fun(F=#bic_id{name=Var}, Ei) ->
	      {F, unset_value(Var, Ei)};
	 (F, Ei) ->
	      {F, Ei}
      end, E, Code).

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


-spec value(Var::string() | {string(),[integer()]}, env(), value()) ->
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

-spec set_value(Var::string() | {string(),[integer()]}, env(), value()) ->
	  env().

set_value(Var, E=#{ global := G, stack := Stack}, Value) ->
    set_value_(Var, Stack, [], G, E, Value).

set_value_(Var, [Local|Stack], Stack1, G, E, Value) ->
    case maps:get({type,Var},Local,undefined) of
	undefined -> set_value_(Var,Stack,[Local|Stack1],G,E,Value);
	_Type -> 
	    Local1 = Local#{{value,Var}=>Value },
	    Stack2 = lists:reverse(Stack1,[Local1|Stack]),
	    E#{ stack => Stack2 }
    end;
set_value_(Var, [], _RStack, G, E, Value) ->
    case maps:get({type,Var},G,undefined) of    
	undefined -> error({undefined, Var});
	_Type ->
	    G1 = G#{{value,Var}=>Value },
	    E#{ global => G1 }
    end.

-spec unset_value(Var::string() | {string(),[integer()]}, env()) ->
	  env().

unset_value(Var, E=#{ global := G, stack := Stack}) ->
    unset_value_(Var, Stack, [], G, E).

unset_value_(Var, [Local|Stack], Stack1, G, E) ->
    case maps:find({type,Var},Local) of
	{ok,undefined} -> E;
	{ok,_Type} ->
	    Local1 = Local#{{value,Var}=>undefined },
	    Stack2 = lists:reverse(Stack1,[Local1|Stack]),
	    E#{ stack => Stack2 };
	error ->
	    unset_value_(Var,Stack,[Local|Stack1],G,E)
    end;
unset_value_(Var, [], _RStack, G, E) ->
    case maps:find({type,Var},G) of    
	{ok,undefined} -> E;
	{ok,_Type} ->
	    G1 = G#{{value,Var}=>undefined },
	    E#{ global => G1 };
	error -> E
    end.

-spec add_value(Var::string() | {string(),[integer()]}, env(), value()) ->
	  value().

add_value(Var, E=#{ global := G, stack := Stack}, Value) ->
    ?dbg("set_value: ~p = ~p\n", [Var, Value]),
    add_value_(Var, Stack, [], G, E, Value).

add_value_(Var, [Local|Stack], Stack1, G, E, Value) ->
    case maps:get({value,Var},Local,undefined) of
	undefined -> add_value_(Var,Stack,[Local|Stack1],G,E,Value);
	PrevValue -> 
	    Local1 = Local#{{value,Var} => PrevValue + Value },
	    Stack2 = lists:reverse(Stack1,[Local1|Stack]),
	    E#{ stack => Stack2 }
    end;
add_value_(Var, [], _RStack, G, E, Value) ->
    case maps:get({value,Var},G,undefined) of    
	undefined -> 
	    E;
	PrevValue ->
	    G1 = G#{{value,Var} => PrevValue+Value },
	    E#{ global => G1 }
    end.
