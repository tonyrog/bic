%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    Test
%%% @end
%%% Created : 14 Apr 2020 by Tony Rogvall <tony@rogvall.se>

-module(bic_test).
-compile(export_all).

-include("../include/bic.hrl").
-define(L, line=0).

type() ->
    Cases = 
	[
	 %% basic types
	 {"int x", int()},
	 {"char x", type(char)},
	 {"float x", type(float)},
	 {"double x", type(double)},
	 {"short x", #bic_type{?L,size=short}},
	 {"long x", #bic_type{?L,size=long}},
	 {"long long x", #bic_type{?L,size=long_long}},
	 {"long double x", #bic_type{?L,size=long,type=double}},
	 {"unsigned int x", #bic_type{?L,sign=unsigned,type=int}},
	 {"signed int x", #bic_type{?L,sign=signed,type=int}},

	 %% storage
	 {"extern int x", extern, int()},
	 {"static int x", static, int()},
	 {"auto int x", auto, int()},
	 {"register int x", register, int()},
	 
	 %% more basic types
	 {"const int x = 12", #bic_type{?L,const=true, type=int}},
	 {"const float x = 3.14", #bic_type{?L,const=true, type=float}},
	 {"volatile int x", #bic_type{?L,volatile=true, type=int}},
	 {"unsigned int x", #bic_type{?L,sign=unsigned,type=int}},
	 {"signed int x", #bic_type{?L,sign=signed,type=int}},
	 
	 %% pointer
	 {"void* x", pointer(type(void))},
	 {"int* x",  pointer(int())},
	 {"int** x", pointer(pointer(int()))},

	 %% array
	 {"int x[]", array(int(),[])},
	 {"int x[5]", array(int(),dim(5))},
	 {"int x[3][4]",array(array(int(),dim(4)),dim(3))},
	 {"int* x[3]", array(pointer(int()),dim(3))},
	 
	 %% function declarations
	 {"int f()",  fn(int(),[])},
	 {"int f(int)", fn(int(),[decl(int())])},
	 {"int f(int a1)", fn(int(),[decl("a1",int())])},
	 {"int f(int a1,int a2)", fn(int(),[decl("a1",int()),
					    decl("a2",int())])},
	       
	 {"int f(int* a1)", fn(int(),[decl("a1",pointer(int()))])},
    
	 {"static int f(const int a1)", static, 
	  fn(int(),[decl("a1",#bic_type{?L,const=true,type=int})])},

	 {"int f(int[2])", fn(int(),[decl(array(int(),dim(2)))])},
	 {"int f(int[2][3])", 
	  fn(int(),[decl(array(array(int(),dim(3)),dim(2)))])},
	 {"int* f(int)", fn(pointer(int()),[decl(int())])},
	 
	 %% function pointers
	 {"int (*f)(int)", pointer(fn(int(),[decl(int())]))},

	 %% const pointers
	 {"const int* ptr", pointer(const(int))},

	 {"int * const ptr", const(pointer(int()))}

	],
    NFail = 
	lists:sum(lists:map(
		    fun({String,Type}) -> 
			    match_type(String, Type);
		       ({String,Storage,Type}) ->
			    match_type(String, Storage, Type)
		    end, Cases)),
    if NFail =:= 0 ->
	    io:format("ALL CASES OK\n", []);
       true ->
	    io:format("FAILED ~w cases\n", [NFail])
    end.


int() -> type(int).
type(T) -> #bic_type{?L,type=T}.
const(T) -> #bic_type{?L,const=true,type=T}.
pointer(T) -> #bic_pointer{?L,type=T}.
array(T,D) -> #bic_array{?L,type=T,dim=D}.
fn(T,Ps) -> #bic_fn{?L,type=T,params=Ps}.
    

dim(N) ->
    #bic_constant{line=0,base=10, value=integer_to_list(N)}.

decl(Name,Type) ->
    #bic_decl{line=0,name=Name,type=Type}.

decl(Type) -> %% paramter declaration without name has no linenumber yet!
    #bic_decl{type=Type}.

match_type(String, Type) ->    
    match_type(String, undefined, Type).
    
match_type(String, Storage, Type) ->
    io:format("test: ~s  -- ", [String]),
    case bic:string(String++";") of
	{ok,[#bic_decl{ storage=Storage, type=Type}]} ->
	    io:format("OK\n"),
	    0;
	{ok,[#bic_decl{ type=BadMatch}]} ->
	    io:format("ERROR bad match ~s\n", [bic:format_type(BadMatch)]),
	    1;
	{error,Reason} ->
	    io:format("ERROR ~p\n", [Reason]),
	    1
    end.
