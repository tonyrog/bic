%%% File    : bic.erl
%%% Author  : Tony Rogvall <tony@iMac.local>
%%% Description : BEAM interpreted C (ode)
%%% Created : 27 Dec 2005 by Tony Rogvall <tony@iMac.local>

-module(bic).

-export([file/1, file/2]).
-export([string/1, string/2]).
-export([parse/1, parse/2]).

-export([format_type/1]).
-compile(export_all).

-import(lists, [map/2]).

-include("../include/bic.hrl").

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

format_type(undefined) -> "_";
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
	 undefined -> "(int)";
	 void -> "void ";
	 char -> "char";
	 int -> "int";
	 float -> "float";
	 double -> "double";
	 #bic_typeid{name=Name} -> [Name]
     end];
format_type(#bic_enum{name=Name,elems=Es}) ->
    ["enum ",Name," {", format_elems(Es,","), "}"];
%% format_type(#bic_pointer{type=Type=#bic_fn{}}) ->
%%    ["(*)",format_type(Type)];
format_type(#bic_pointer{type=Type}) ->
    [format_type(Type),"*"];
format_type(#bic_array{type=Type,dim=D}) ->
    if D =:= [] ->
	    [format_type(Type),"[]"];
       true ->
	    [format_type(Type),"[",format_expr(D),"]"]
    end;
format_type(#bic_fn{type=Type,params=Ps}) ->
    [format_type(Type),"(",format_decls(Ps,","),")"];
format_type(#bic_struct{name=Name,elems=Es}) ->
    ["struct ",Name,"{", format_decls(Es,";"),"}"];
format_type(#bic_union{name=Name,elems=Es}) ->
    ["union ",Name,"{", format_decls(Es,";"),"}"].


format_decl(#bic_decl{name=undefined, type=Type}) ->
    format_type(Type);
format_decl(#bic_decl{name=Name, type=Type}) ->
    [format_type(Type)," ",Name].

format_decls([],_Sep) -> [];
format_decls([X],_Sep) -> [format_decl(X)];
format_decls([X|Xs],Sep) -> [format_decl(X),Sep,format_decls(Xs,Sep)].

format_elem({Name,_Ln,Value}) ->
    [Name,"=",format_expr(Value)].

format_elems([],_Sep) -> [];
format_elems([X],_Sep) -> [format_elem(X)];
format_elems([X|Xs],Sep) -> [format_elem(X),Sep,format_elems(Xs,Sep)].

format_expr(#bic_id{name=Name}) -> Name;
format_expr(#bic_constant{value=Value}) -> Value;
format_expr(#bic_unary{op=Op,arg=Arg}) -> 
    [atom_to_list(Op),format_expr(Arg)];
format_expr(#bic_binary{op=Op,arg1=Arg1,arg2=Arg2}) -> 
    [format_expr(Arg1),atom_to_list(Op),format_expr(Arg2)];
format_expr(#bic_call{func=F, args=Exprs}) ->
    [format_expr(F),"(",format_exprs(Exprs),")"];
format_expr(#bic_ifexpr{test=C,then=T,else=E}) ->
    [format_expr(C),"?",format_expr(T),":",format_expr(E)];
format_expr(#bic_assign{op='=',lhs=L,rhs=R}) ->
    [format_expr(L),"=",format_expr(R)].

format_exprs([]) -> [];
format_exprs([X]) -> [format_expr(X)];
format_exprs([X|Xs]) -> [format_expr(X),",",format_exprs(Xs)].
