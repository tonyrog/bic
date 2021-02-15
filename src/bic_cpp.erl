%%%-------------------------------------------------------------------
%%% File    : bic_cpp.erl
%%% Author  : Tony Rogvall <tony@rogvall.se>
%%% Description : BIC C preprocessor
%%%
%%% Created : 25 Aug 2009 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(bic_cpp).

-behaviour(gen_server).

-include("../include/bic.hrl").

%% -define(debug, true).

-define(Q, $\").  %% "stupid emacs mode!
-define(LP,$\().
-define(RP,$\)).
-define(DOT,$.).
-define(COMMA, $,).

-define(is_alpha(C),
	(C >= $a andalso C =< $z) orelse 
	(C >= $A andalso C =< $Z) orelse
	(C =:= $_)).

-define(is_identifier(C),
	(C >= $0 andalso C =< $9) orelse
	?is_alpha(C)).

-define(is_hex_digit(C),
	(C >= $a andalso C =< $f) orelse 
	(C >= $A andalso C =< $F) orelse 
        (C >= $0 andalso C =< $9)).

-define(is_bin_digit(C),
        (C >= $0 andalso C =< $1)).

-define(is_oct_digit(C), 
	((C >= $0) andalso (C =< $7))).

-define(is_dec_digit(C), 
	((C >= $0) andalso (C =< $9))).

-define(is_blank(C), ((C =:= $\s) orelse (C =:= $\t))).

-define(FALSE, 0).
-define(TRUE,  1).
-define(SKIP,  2).
-define(UNDEF, undef).

%% API
-export([open/1,open/2,close/1,read/1, read_line/1]).
-export([string/1, string/2]).
-export([line/1, file/1, value/2, values/1]).
-export([command/0, command/1]).

%% DEBUG
-export([dump_file/1]).
-export([dump_defs/1]).
-export([dump_lines/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(lists, [foreach/2, reverse/1, map/2, foldl/3]).

-export([token/1, tokens/1]).
-export([to_integer/1, to_integer/2]).

-ifdef(debug).
-define(dbg(F,A), io:format(standard_error,(F),(A))).
-compile(export_all).
-else.
-define(dbg(F,A), ok).
-endif.

-record(save,
	{
	  fd,        %% character input 
	  readf,     %% read functions
	  file,      %% file name
	  line,      %% line number
	  line1,     %% next line number
	  inc_level, %% include level
	  if_stack,  %% current if stack
	  cwd,       %% current working directory
	  lbuf,
	  cbuf       %% stored data from file
	 }).

-record(s,
	{
	  fd,              %% current fd
	  file,            %% current file name
	  readf,           %% Read function
	  line,            %% current line number
	  line1,           %% next line number
	  inc_level,       %% include level
	  if_stack=[],     %% {Tag,0|1|2,Ln}
	  inc_stack=[],    %% #save {}
	  expand=[],       %% current macro expansion
	  cwd,             %% current working directory
	  include=[],      %% search path for #include <foo.h> 
	  qinclude=[],     %% search path for #include "foo.h"
	  defs,            %% dict of defines {Name,Value}
	  lbuf=[],         %% buffer of line ready for delivery
	  cbuf=[]          %% character buffer
	 }).

-ifdef(debug).
-define(CHUNK_SIZE, 1). 
-else.
-define(CHUNK_SIZE, 1024).
-endif.


%%====================================================================
%% API
%%====================================================================

string(String) ->
    string(String,[]).

string(String, Env) ->
    gen_server:start_link(?MODULE, [{string,String},Env], []).    
				    
open(File) ->
    open(File, []).

open(File, Env) ->
    gen_server:start_link(?MODULE, [{file,File},Env], []).

close(PFd) when is_pid(PFd) ->
    gen_server:call(PFd, close).

read(PFd) when is_pid(PFd) ->
    gen_server:call(PFd, read).

read_line(PFd) when is_pid(PFd) ->
    gen_server:call(PFd, read_line).

line(PFd) when is_pid(PFd) ->
    value(PFd, "__LINE__").

file(PFd) when is_pid(PFd) ->
    value(PFd,"__FILE__").

value(PFd, Var) ->
    gen_server:call(PFd, {value, Var}).

values(PFd) ->
    gen_server:call(PFd, values).


command() ->
    io:format(standard_error, "bicpp: no input files\n", []),
    halt(1).

command([]) ->
    io:format(standard_error, "bicpp: no input files\n", []),
    halt(1);
command([File|_]) ->
    case dump_file(atom_to_list(File)) of
	ok ->
	    halt(0);
	{error,Reason} ->
	    io:format(standard_error, "bicpp: error: ~p\n", [Reason]),
	    halt(1)
    end.

dump_lines(File) ->
    case open(File) of
	{ok,PFd} ->
	    dump_fd_lines(PFd),
	    close(PFd);
	Error ->
	    Error
    end.

dump_fd_lines(PFd) ->
    case read_line(PFd) of
	eof -> ok;
	Err = {error,_} -> Err;
	Cs ->
	    io:format("[~s]\n", [Cs]),
	    dump_fd_lines(PFd)
    end.


dump_file(File) ->
    dump_file(File,[]).

dump_file(File,Env) ->
    case open(File,Env) of
	{ok,PFd} ->
	    read_loop(PFd),
	    %% dump_defs(PFd),
	    close(PFd),
	    ok;
	{error,Err} ->
	    {error,Err}
    end.

read_loop(PFd) ->
    case read(PFd) of
	eof ->
	    ok;
	Line ->
	    io:format("~s", [Line]),
	    read_loop(PFd)
    end.

dump_defs(PFd) ->
    List = values(PFd),
    lists:foreach(
      fun({Name, {Params,Def}}) ->
	      io:format("~s(~s)=~s\n", 
			[Name,
			 format_params(Params),
			 format_tokens(Def)]);
	 ({Name, Def}) ->
	      io:format("~s=~s\n", 
			[Name, format_tokens(Def)])
      end, List).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([{string,String},UserOpts]) ->
    File = "*string*",
    Defs = init_defs(default_opts(File)++UserOpts),
    Include = init_include(UserOpts,["/usr/local/include", 
				     "/usr/include"]),
    QInclude = init_qinclude(UserOpts,[]),
    {ok,Cwd} = file:get_cwd(),
    S = #s { file = File,
	     readf = fun() -> eof end,
	     line = 0,
	     line1 = 0,
	     inc_level = 0,
	     cwd = Cwd,
	     include = Include,
	     qinclude = QInclude,
	     defs = Defs,
	     cbuf = String,
	     lbuf = [auto_line(1,File),
		     auto_line(1, "<built-in>"),
		     auto_line(1, "<command-line>"),
		     auto_line(1,File)]
	   },
    {ok, S};
init([{file,File},UserOpts]) ->
    case file:open(File,[read]) of
	{ok,Fd} ->
	    Defs = init_defs(default_opts(File)++UserOpts),
	    Include = init_include(UserOpts,["/usr/local/include", 
					     "/usr/include"]),
	    QInclude = init_qinclude(UserOpts,[]),
	    S = #s { file = File,
		     readf = fun() -> file:read(Fd, ?CHUNK_SIZE) end,
		     fd = Fd,
		     line = 0,
		     line1 = 0,
		     inc_level = 0,
		     cwd = filename:dirname(File),
		     include = Include,
		     qinclude = QInclude,
		     defs = Defs,
		     lbuf = [auto_line(1,File),
			     auto_line(1, "<built-in>"),
			     auto_line(1, "<command-line>"),
			     auto_line(1,File)]
		   },
	    {ok, S};
	Error ->
	    {stop, Error}
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call(read, _From, S) ->
    {Reply, S1} = read_0(S),
    {reply,Reply,S1};
handle_call(read_line, _From, S) ->
    %% read non-expanded line
    case S#s.lbuf of
	[Line|LBuf] ->
	    {reply, Line, S#s{lbuf=LBuf}};
	[] ->
	    {Reply,S1} = read_cbuf_line(S),
	    {reply, Reply, S1}
    end;
handle_call(close, _From, S) ->
    {stop, normal, ok, S};
handle_call({value,Var}, _From, S) ->
    {reply, value_(S,Var), S};
handle_call(values, _From, S) ->
    {reply, maps:to_list(S#s.defs), S};
handle_call(_Call, _From, S) ->
    {reply, {error,bad_call}, S}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, S) ->
    {noreply, S}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, S) ->
    {noreply, S}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, S) ->
    foreach(fun(Save) ->
		    ?dbg("TERMINATE-CLOSE-SAVED: ~s\n", [Save#save.file]),
		    file:close(Save#save.fd)
	    end, S#s.inc_stack),
    if S#s.fd =:= undefined ->
	    ok;
       true ->
	    ?dbg("TERMINATE-CLOSE: ~s\n", [S#s.file]),
	    file:close(S#s.fd)
    end.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
init_include([{include,Path} | Opts], Paths) when is_list(Path) ->
    init_include(Opts, Paths ++ [Path]);
init_include([{include,Path} | Opts], Paths) when is_atom(Path) ->
    init_include(Opts, Paths ++ [atom_to_list(Path)]);
init_include([_|Opts], Paths) ->
    init_include(Opts, Paths);
init_include([], Paths) ->
    Paths.

init_qinclude([{qinclude,Path} | Opts], Paths) when is_list(Path) ->
    init_qinclude(Opts, Paths ++ [Path]);
init_qinclude([{qinclude,Path} | Opts], Paths) when is_atom(Path) ->
    init_qinclude(Opts, Paths ++ [atom_to_list(Path)]);
init_qinclude([_|Opts], Paths) ->
    init_qinclude(Opts, Paths);
init_qinclude([], Paths) ->
    Paths.

init_defs(Opts) ->
    foldl(fun({define,N,V},D) when is_list(V) ->
		  maps:put(N, [{string,0,V}], D);
	     ({define,N,V},D) when is_integer(V) ->
		  maps:put(N, [{decnum,0,integer_to_list(V)}], D);
	     (_, D) ->
		  D
	  end, maps:new(), Opts).

%% Return a list of default options
default_opts(_File) ->
    {Dy,Dm,Dd} = date(),
    {Th,Tm,Ts} = time(),
    Mon = case Dm of
	      1->"Jan"; 2->"Feb"; 3->"Mar"; 4->"Apr"; 5->"May"; 6->"Jun";
	      7->"Jul"; 8->"Aug"; 9->"Sep"; 10->"Oct"; 11->"Nov"; 12->"Dec"
	  end,
    Time=lists:flatten(io_lib:format("~2..0w:~2..0w:~2..0w",[Th,Tm,Ts])),
    Date=lists:flatten(io_lib:format("~s ~2w ~4w",[Mon,Dd,Dy])),
    [{define,"__DATE__", Date},
     {define,"__TIME__", Time}
    ].

%% Read and expand a line
read_0(S) ->
    case S#s.lbuf of
	[Line|LBuf] ->
	    {Line, S#s{lbuf=LBuf}};
	[] ->
	    read_1(S)
    end.

read_1(S) ->
    {Ts,S1} = read_token_line(S),
    case expand(S1, Ts) of
	{empty, S2} ->
	    read_1(S2);
	{Read, S2} ->
	    read_2(S2,Read)
    end.

read_2(S, Data) when is_list(Data) ->
    case stat_if(S) of
	?FALSE ->
	    read_1(S);
	?SKIP ->
	    read_1(S);
	?TRUE ->
	    emit_line(S, Data)
    end;
read_2(S, {D,Bool}) when D=:='#if'; D=:='#ifdef'; D=:='#ifndef'-> 
    case peek_if(S) of
	{_Tag,V,_Ln} ->
	    if V =:= ?FALSE ->
		    S1 = push_if(S,D,?SKIP),
		    read_1(S1);
	       V =:= ?TRUE ->
		    S1 = push_if(S,D,Bool),
		    read_1(S1);
	       V =:= ?SKIP ->
		    S1 = push_if(S,D,?SKIP),
		    read_1(S1)
	    end;
	empty ->
	    S1 = push_if(S,D,Bool),
	    read_1(S1)
    end;
read_2(S, {'#elif',?TRUE}) ->
    case peek_if(S) of
	{Tag,V,Ln} ->
	    if Tag =/= '#if'; Tag =/= '#ifdef';
	       Tag =/= '#ifndef'; Tag=/='#else' ->
		    error(S, "#elif missing #if", []),
		    read_1(S);
	       V =:= ?SKIP ->
		    read_1(S);
	       V =:= ?TRUE -> %% skip,alread processed
		    S1 = mod_if(S,Tag,?SKIP,Ln),
		    read_1(S1);
	       V =:= ?FALSE -> %% take this clause
		    S1 = mod_if(S,'#elif',?TRUE,S#s.line),
		    read_1(S1)
	    end;
	empty ->
	    error(S, "#elif missing #if", []),
	    read_1(S)
    end;
read_2(S, {'#elif',?FALSE}) ->
    case peek_if(S) of
	{Tag,V,Ln} ->
	    if Tag =/= '#if'; Tag =/= '#ifdef';
	       Tag =/= '#ifndef'; Tag =/='#else' ->
		    error(S, "#elif missing #if", []),
		    read_1(S);
	       V =:= ?SKIP ->
		    read_1(S);
	       V =:= ?TRUE -> %% skip,alread processed
		    S1 = mod_if(S,Tag,?SKIP,Ln),
		    read_1(S1);
	       V =:= ?FALSE -> %% ignore this clause
		    S1 = mod_if(S,'#elif',?FALSE,S#s.line),
		    read_1(S1)
	    end;
	empty ->
	    error(S, "#elif missing #if", []),
	    read_1(S)
    end;
read_2(S, '#else') ->
    case peek_if(S) of
	{'#else',_V,_Ln} ->
	    error(S, "#else missing #if/#ifdef", []),
	    read_1(S);
		    
	{Tag,V,Ln} ->
	    if V =:= ?SKIP ->
		    read_1(S);
	       V =:= ?TRUE ->
		    S1 = mod_if(S,Tag,?SKIP,Ln),
		    read_1(S1);
	       V =:= ?FALSE ->
		    S1 = mod_if(S,'#else',?TRUE,S#s.line),
		    read_1(S1)
	    end;
	[] ->
	    error(S, "#else missing #if", []),
	    read_1(S)
    end;
read_2(S, '#endif') -> 
    case pop_if(S) of
	{S1,true} ->
	    read_1(S1);
	{S1,false} ->
	    error(S, "#endif missing #if/#ifdef/#ifndef", []), 
	    read_1(S1)
    end;
read_2(S, eof) ->
    file:close(S#s.fd),
    ?dbg("CLOSE: ~s\n", [S#s.file]),
    %% LBuf = S#s.lbuf ++ lists:duplicate(NLines, "\n"),
    restore(S#s {fd=undefined}, eof).

read_token_line(S) ->
    {CLine,S1} = read_cbuf_line(S),
    Ln = S1#s.line1,
    case tokens(CLine, Ln) of
	{[],[$\n]} ->
	    {[], S1#s { line1=Ln+1 }};
	{Ts,[]} ->
	    {Ts, S1};
	{Ts,[$\n|Ns]} ->
	    {Ts, S1#s { line1=Ln+1, cbuf=Ns++S1#s.cbuf}}
    end.

%%
%% Emit a line
%%
emit_line(S, Data) ->
    N = S#s.line1 - S#s.line,
    %% io:format("EMIT: L1=~w, L=~w [~p]\n", [S#s.line1, S#s.line, Data]),
    emit_line(S, Data, N).

-define(AUTO_LINE_LIMIT, 9).

emit_line(S, Data, N) when N >= ?AUTO_LINE_LIMIT ->
    {auto_line(S#s.line+N-1,S#s.file), S#s { lbuf=[Data], line=S#s.line1 }};
emit_line(S, Data, 0) -> %% ???
    {Data, S};
emit_line(S, Data, N) when N =:= 1 ->
    {Data, S#s { line=S#s.line1 }};
emit_line(S, Data, N) when N >= 2 ->
    {"\n", S#s { lbuf=lists:duplicate(N-2, "\n")++[Data],
		 line=S#s.line1 }}.


%% Return:
%%   {eof, NLines, S}
%%   {Error, NLines, S}
%%   {Tokens, NLines, S}
%%
expand(S,eof) ->
    {eof,S};
expand(S,Err={error,_}) ->
    {Err,S};
expand(S,[]) ->
    {empty, S};
expand(S,Ts) ->
    expand_line(S, Ts, []).

expand_line(S, Ts, Acc) ->
    case Ts of
	[{blank,_Ln,Bs}|Ts1] ->
	    expand_line(S,Ts1,Bs++Acc);
	[{char,_Ln,$#},{blank,_Ln,_Bs}|Ts1] ->
	    directive(S, Ts1, stat_if(S));
	[{char,_Ln,$#}|Ts1] ->
	    directive(S, Ts1, stat_if(S));
	[] ->
	    Cs = lists:flatten(lists:reverse(Acc)),
	    {Cs++[$\n], S};
	_ ->
	    {Ts1,S1} = expand_tokens(S, Ts),
	    {ts_to_cs(reverse(Ts1),Acc)++[$\n], S1}
    end.

%%
%% Expand tokens, return reversed list of expanded tokens
%%
expand_tokens(S, Ts) ->
    expand_tokens(S#s { expand=[]}, Ts, []).
    
expand_tokens(S, [T|Ts], Acc) ->
    case T of
	{identifier,Ln,"__LINE__"} ->
	    %% FIXME: check that __LINE__ is defined 
	    expand_tokens(S, Ts, [{decnum,Ln,integer_to_list(Ln)}|Acc]);

	{identifier,Ln,"__FILE__"} ->
	    expand_tokens(S, Ts, [{string,Ln,S#s.file}|Acc]);
	
	{identifier,_Ln,X} ->
	    case lists:member(X, S#s.expand) of
		true ->
		    ?dbg("Recursive definition parameter ~s\n", [X]),
		    expand_tokens(S,Ts,[T|Acc]);
		false ->
		    case maps:find(X,S#s.defs) of
			error ->
			    expand_tokens(S,Ts,[T|Acc]);
			{ok,{Params,Body}} ->
			    expand_macro_tokens(S,X,Params,Body,T,Ts,Acc);
			{ok,Body} ->
			    expand_macro_tokens(S,X,Body,T,Ts,Acc)
		    end
	    end;
	_ ->
 	    expand_tokens(S,Ts,[T|Acc])
    end;
expand_tokens(S, [], Acc) ->
    {Acc, S}.

%%
%% Expand Macro defintion Name(A1,A2....)
%% If collect_args fail then reinsert the start tokens and
%% continue
%%
expand_macro_tokens(S,Name,Params,Body,T,Ts,Acc) ->
    ?dbg("expand_macro ~s\n", [Name]),
    {Args0,Ts1,S1} = collect_args(S,Ts),
    ?dbg("  params=~p\n", [Params]),
    ?dbg("  body=~p\n", [Body]),
    ?dbg("  args=~p\n", [Args0]),
    %%Args = map(fun(A) -> {As,_,_}=expand_ts(S1, A, 0),reverse(As) end, Args0),
    %% ?dbg("  args1=~p\n", [Args]),
    %% expand Body with parameters set
    try bind_args(Params, Args0, S1#s.defs) of
	Defs1 ->
	    %% expand parameters
	    ?dbg("expand_def: ~s\n", [ts_to_cs(Body)]),
	    TsBody = expand_def(S1#s { defs=Defs1 }, Body, []),
	    ?dbg("expanded: ~s\n", [ts_to_cs(TsBody)]),
	    Expand = [Name|S1#s.expand],
	    {TsBody1,S2} = expand_tokens(S1#s { expand=Expand }, TsBody, []),
	    ?dbg("expanded2: ~s\n", [ts_to_cs(reverse(TsBody1))]),
	    {Acc2,S3}=expand_tokens(S2,reverse(TsBody1), Acc),
	    expand_tokens(S3#s { expand=S1#s.expand }, Ts1, Acc2)
    catch
	error:_ -> 
	    expand_tokens(S, Ts, [T|Acc])
    end.

expand_macro_tokens(S,Name,Body,_T,Ts,Acc) ->
    ?dbg("expand_macro ~s\n", [Name]),
    ?dbg("  body: ~s\n", [ts_to_cs(Body)]),
    TsBody = expand_def(S, Body, []),
    ?dbg("expanded: ~s\n", [ts_to_cs(TsBody)]),
    Expand = [Name|S#s.expand],
    {Acc2,S2} = expand_tokens(S#s { expand=Expand }, TsBody, Acc),
    expand_tokens(S2#s { expand=S#s.expand },Ts,Acc2).



%% top level replace identifiers with value
expand_def(S, [{char,_,$#},{identifier,Ln,X}|Ts], Acc) ->
    expand_def_stringify(S, Ln, X, Ts, Acc);
expand_def(S, [{char,_,$#},{blank,_,_},{identifier,Ln,X}|Ts], Acc) ->
    expand_def_stringify(S, Ln, X, Ts, Acc);
%% some flavors of paste
expand_def(S,[A,{blank,_,_},{char,Ln,$#},{char,_,$#},{blank,_,_},B|Ts],Acc) ->
    expand_def_paste(S, Ln, A, B, Ts, Acc);
expand_def(S,[A,{char,Ln,$#},{char,_,$#},{blank,_,_},B|Ts],Acc) ->
    expand_def_paste(S, Ln, A, B, Ts, Acc);
expand_def(S,[A,{blank,_,_},{char,Ln,$#},{char,_,$#},B|Ts],Acc) ->
    expand_def_paste(S, Ln, A, B, Ts, Acc);
expand_def(S,[A,{char,Ln,$#},{char,_,$#},B|Ts],Acc) ->
    expand_def_paste(S, Ln, A, B, Ts, Acc);
expand_def(S, [T={identifier,_,Var}|Ts], Acc) ->
    case dict_param(S, Var) of
	[] -> expand_def(S, Ts, [T|Acc]);
	Ts1 when is_list(Ts1) -> expand_def(S, Ts1++Ts, Acc);
	_ -> expand_def(S, Ts, [T|Acc])
    end;
expand_def(S, [T|Ts], Acc) ->
    expand_def(S, Ts, [T|Acc]);
expand_def(_S, [], Acc) ->
    reverse(Acc).

%% Paste tokens  <token> ## <token>
expand_def_paste(S, Ln, {identifier,_,Va}, {identifier,_,Vb}, Ts, Acc) ->
    case dict_param(S, Va) of
	[] ->
	    expand_def(S, Ts, [{identifier,Ln,Va++Vb}|Acc]);
	Ta  ->
	    case dict_value(S, Vb) of
		[] ->
		    expand_def(S, Ts, [{identifier,Ln,Va++Vb}|Acc]);
		Tb ->
		    expand_def_paste_ts(S, Ln, Ta, Tb, Ts, Acc)
	    end
    end;
expand_def_paste(S, Ln, A, B, Ts, Acc) ->
    expand_def_paste_ts(S, Ln, [A], [B], Ts, Acc).


expand_def_paste_ts(S, _Ln, [{char,La,$/}], [{char,Lb,$*}|Tb], Ts, Acc) ->
    error(S, "pasting \"~s\" and \"~s\" does not give a valid preprocessing token", 
	  ["/", "*"]),
    expand_def(S, Ts, reverse(Tb)++[{char,Lb,$*},{blank,La," "},{char,La,$/}|Acc]);
expand_def_paste_ts(S, _Ln, [{char,La,$/}], [{char,Lb,$/}|Tb], Ts, Acc) ->
    error(S, "pasting \"~s\" and \"~s\" does not give a valid preprocessing token", 
	  ["/", "/"]),
    expand_def(S, Ts, reverse(Tb)++[{char,Lb,$/},{blank,La," "},{char,La,$/}|Acc]);
expand_def_paste_ts(S, _Ln, Ta, Tb, Ts, Acc) ->
    {Tc, _Ln1} = tokens(ts_to_cs(Ta) ++ ts_to_cs(Tb)),
    expand_def(S, Ts, reverse(Tc)++Acc).
    
    
%% Stringinfy  #<var>
expand_def_stringify(S, Ln, X, Ts, Acc) ->
    case dict_param(S, X) of 
	[] ->
	    error(S, "'#' is not followed by a macro parameter", []),
	    expand_def(S, Ts, Acc);
	Ts1 ->
	    String = ts_to_cs(Ts1),
	    expand_def(S, Ts, [{string,Ln,String}|Acc])
    end.

%%
%% Bind macro arguments
%%
bind_args([{arg,A}|As], [V|Vs], Dict) ->
    ?dbg("bind: ~s  = ~s\n", [A, ts_to_cs(V)]),
    Dict1 = maps:put(A, {param,V}, Dict),
    bind_args(As, Vs, Dict1);
bind_args([{varg,A}], Vs, Dict) ->
    %% insert , between param values
    Vs1 = ts_arg_list(Vs),
    ?dbg("bind: ~s  = ~s\n", [A, ts_to_cs(Vs1)]),
    maps:put(A, {param,Vs1}, Dict);
bind_args([], [], Dict) ->
    Dict.

%% merge list of token lists and insert ',' beteen the elements
ts_arg_list([]) ->
    [];
ts_arg_list([P]) ->
    P;
ts_arg_list([P=[{_,Ln,_}|_] | Ps]) ->
    P++[{char,Ln,?COMMA}] ++ ts_arg_list(Ps).

%%
%%  collect and bind macro parameters
%%
%%   '(' A1 ',' A2 ',' ... ',' An ')'
%% return {[A1,...An], NLines, S'}
%%
collect_args(S, [{blank,_Ln,_} | Ts]) ->
    collect_args(S, Ts);
collect_args(S, [{char,_Ln,?LP} | Ts]) ->
    ?dbg("COLLECT: ~p\n", [Ts]),
    collect_args_(S, Ts, 0, [], []);
collect_args(S, Ts) ->
    if S#s.expand =:= [] ->
	    error(S,"missing macro arguments",[]);
       true -> 
	    ok
    end,
    {[], Ts, S}.

collect_args_(S, [T|Ts], D, Acc, As) ->
    case T of
	{char,_Ln,?LP} -> %% push '('
	    collect_args_(S, Ts, D+1, [T|Acc], As);

	{char,_Ln,?RP} when D =:= 0 -> %% pop last ')'
	    if As =:= [], Acc =:= [] -> %% no arguments
		    {[], Ts, S};
	       Acc =:= [] ->
		    error(S,"missing macro arguments",[]),
		    {reverse([[]|As]), Ts, S};
	       true ->
		    {reverse([make_arg(Acc)|As]), Ts, S}
	    end;

	{char,_Ln,?RP} ->
	    collect_args_(S, Ts, D-1, [T|Acc], As);
	    
	{char,_Ln,?COMMA} when D =:= 0 -> %% handle argument
	    if Acc =:= [] ->
		    error(S,"missing macro arguments",[]),
		    collect_args_(S, Ts, 0, [], [[]|As]);
	       true ->
		    collect_args_(S, Ts, 0, [], [make_arg(Acc)|As])
	    end;
	_ ->
	    collect_args_(S, Ts, D, [T|Acc], As)
    end;
collect_args_(S, [], D, Acc, As) ->
    if S#s.expand =:= [] ->
	    {Ts, S1} = read_token_line(S),
	    collect_args_(S1, Ts, D, Acc, As);
       true ->
	    {reverse([make_arg(Acc)|As]), [], S}
    end.

make_arg(Ts) ->
    trim_(reverse(Ts)).



%% skip: recognise #if/#else/#elif/#endif
directive(S, Ts, ?SKIP) ->
    case Ts of
	[{identifier,_Ln,"endif"}|_] ->  %% ignore trailing data?
	    {'#endif',S};
	[{identifier,_Ln,"else"}|_] ->  %% ignore trailing data?
	    {'#else', S};
	[{identifier,_Ln,"if"}|_] ->
	    {{'#if',?FALSE}, S};
	[{identifier,_Ln,"elif"}|_] ->
	    {{'#elif',?FALSE}, S};
	_ ->
	    {empty, S}
    end;
%% false: recognise #if/#else/#elif/#endif
directive(S, Ts, ?FALSE) ->
    case Ts of
	[{identifier,_Ln,"endif"}|_] ->  %% ignore trailing data?
	    {'#endif', S};
	[{identifier,_Ln,"else"}|_] ->  %% ignore trailing data?
	    {'#else', S};
	[{identifier,_Ln,"if"}|_] ->
	    {{'#if',?FALSE}, S};
	[{identifier,_Ln,"elif"}|Ts1] ->
	    cpp_if(S,Ts1,'#elif');
	_ ->
	    {empty, S}
    end;
directive(S, Ts, ?TRUE) ->
    ?dbg("directive:Tokens=~p\n", [Ts]),
    case Ts of
	[{identifier,_Ln,"if"},{blank,_,_}|Ts1] ->  %% #if <bool-expr>
	    cpp_if(S,Ts1,'#if');

	[{identifier,_Ln,"elif"}|Ts1] -> %% #elif <bool-expr>
	    cpp_if(S,Ts1,'#elif');

	%% #ifdef <id>
	[{identifier,_Ln,"ifdef"},{blank,_,_},{identifier,_,Var}|_Ts1] ->
	    %% io:format("#ifdef ~s is ~w\n", [Var, defined(S,Var)]),
	    {{'#ifdef',defined(S,Var)},S};
	[{identifier,_Ln,"ifdef"}|_Ts1] ->
	    error(S,"no macro name given in #ifdef directive",[]),
	    {{'#ifdef', ?FALSE},S};

	%% #ifndef <id>
	[{identifier,_Ln,"ifndef"},{blank,_,_},{identifier,_,Var}|_] ->
	    case defined(S,Var) of
		?TRUE  -> {{'#ifndef', ?FALSE},S};
		?FALSE -> {{'#ifndef', ?TRUE},S}
	    end;
	[{identifier,_Ln,"ifndef"}|_] ->
	    error(S, "no macro name given in #ifdef directive",[]),
	    {{'#ifndef', ?FALSE},S};
	
	%% #else 
	[{identifier,_Ln,"else"}|_] ->  %% ignore trailing data?
	    {'#else', S};

	%% #endif
	[{identifier,_Ln,"endif"}|_] ->  %% ignore trailing data?
	    {'#endif', S};

	%% #define <id>['(' <args> ')'] <value>
%%	[{identifier,_,"define"},{blank,_,_},
%%	 {identifier,_,Var},{blank,_,_},{char,Ln,?LP}|Body] ->
%%	    S1 = define_macro(S,Var,[{char,Ln,?LP}|Body]),
%%	    {empty,S1};
	[{identifier,_,"define"},{blank,_,_},
	 {identifier,_,Var},{char,Ln,?LP}|Body] ->
	    S1 = define_macro(S,Var,[{char,Ln,?LP}|Body]),
	    {empty,S1};
	[{identifier,_,"define"},{blank,_,_},{identifier,_,Var}|Body] ->
	    S1 = define(S,Var,trim_(Body)),
	    {empty,S1};
	[{identifier,"define"}|_Body] ->
	    error(S, "no macro name given in #define directive", []),
	    {empty,S};

	%% #undef <id>
	[{identifier,_Ln,"undef"},{blank,_,_},{identifier,_,Var}|_] ->   
	    S1 = undef(S,Var),
	    {empty,S1};

	[{identifier,_Ln,"undef"}|_] ->
	    error(S, "no macro name given in #undef directive", []),
	    {empty,S};

	[{identifier,_Ln,"include"} | Ts1 ] ->
	    {Ts2,S1} = expand_tokens(S,Ts1,[]),
	    case reverse(Ts2) of
		[{blank,_,_},{string,_,File}|_] ->
		    %% #include "string"
		    cpp_include(S1,File,quoted);
		[{blank,_,_},{char,_,$<}|Ts3] ->
		    %% #include <string>
		    case search_path(Ts3) of
			{ok,File} ->
			    cpp_include(S1,File,search);
			{error,Fmt,A} ->
			    error(S1, Fmt, A),
			    {empty,S1}
		    end;
		_ -> 
		    error(S1, "bad arguments given in #include directive",[]),
		    {empty, S1}
	    end;

	%% #include_next  ???
	[{identifier,_Ln,"include_next"}|_] ->
	    error(S, "include_next NOT implemented", []),
	    {empty, S};
	
	%% #line <num>
	%% #line <num> <file>
	[{identifier,_Ln,"line"},{blank,_,_},{decnum,_,Ln},{blank,_,_},
	 {string,_,File}|_] ->
	    case catch list_to_integer(Ln) of
		{'EXIT',_} ->
		    error(S, "bad linenumber ~p in #line directive", 
			  [Ln]),
		    {empty, S};
		N ->
		    S1 = S#s { line=N, line1=N, file=File },
		    {"# "++Ln++" "++File++"\n", S1}
	    end;

	[{identifier,_Ln1,"line"},{blank,_Ln2,_},{decnum,_Ln3,Ln}|_] ->
	    case catch list_to_integer(Ln) of
		{'EXIT',_} ->
		    error(S, "bad linenumber ~p in #line directive",
			  [Ln]),
		    {empty, S};
		N ->
		    S1 = S#s { line=N, line1=N },
		    {"# "++Ln++"\n", S1}
	    end;
	[{identifier,_Ln,"line"}|_]->
	    error(S, "bad #line directive", []),
	    {empty,  S};
	%%
	%% #import
	%% 
	[{identifier,_Ln,"import"}|_] -> %% ignored
	    {empty, S};
	%%
	%% #pragma ...
	%%    once is nice to have
	[{identifier,_Ln,"pragma"}|_] -> %% ignored for now
	    {empty, S};

	[{identifier,_Ln,"error"}|Ts1] ->
	    error_ts(S, Ts1),
	    {empty,  S};

	[{identifier,_Ln,"warning"}|Ts1] ->
	    warning_ts(S, Ts1),
	    {empty, S};

	[{identifier,_Ln,Name}|_] ->
	    error(S, "invalid preprocessing directive #~s",[Name]),
	    {empty, S};

	[] -> %% just an empty directive, let it through
	    {empty, S};
	
	_ ->
	    error(S, "preprocessor directive not understood",[]),
	    {empty, S}
    end.
    
%%
%% #if <expr>
%%   [text]
%% [#elif <expr>
%%    text]
%% [#elif <expr>
%%    text]
%% [#else
%%   text]
%% #endif
%%

cpp_if(S0,Ts0,Tag) ->
    {Ts1,S1} = expand_tokens(S0,Ts0,[]),
    Ts2 = reverse(Ts1),
    Cs = ts_to_cs(Ts2),
    ?dbg("TEST: ~s  [~s] Ts0=~p, Ts2=~p\n", [Tag, Cs, Ts0, Ts2]),
    case bic_scan:string(Cs) of
	{ok,CTs,_} ->
	    %% Put scanned tokens in a parsable for:
	    %% int _ = Ts;
	    Ln = S1#s.line,
	    CTs1 = [{int,Ln},{identifier,Ln,"_"},{'=',Ln}|CTs]++[{';',Ln}],
	    ?dbg("if: CTs1=~p\n", [CTs1]),
	    case bic_parse:parse(CTs1) of
		{ok,[#bic_decl { value=Expr }]} ->
		    Value = eval(S1,Expr),
		    ?dbg("if: Expr: ~p = ~w\n", [Expr,Value]),
		    {{Tag,Value},S1};
		{error,{_Ln,Mod,Message}} ->
		    Err = Mod:format_error(Message),
		    error(S1, "~s", [Err]),
		    {{Tag, ?FALSE},S1}
	    end;
	{error,{_Ln,Mod,Message}} ->
	    Err = Mod:format_error(Message),
	    error(S1, "~s", [Err]),
	    {{Tag, ?FALSE},S1}
    end.

%%   #include "file"
%% | #include <file>
%%
cpp_include(S,File,quoted) ->
    Path = [S#s.cwd]++S#s.qinclude ++ S#s.include,
    cpp_include_path(S, File, Path);
cpp_include(S,File,search) ->
    Path = S#s.include,
    cpp_include_path(S, File, Path).

cpp_include_path(S, Name, [Path | Ps]) ->
    File = filename:join(Path, Name),
    ?dbg("OPEN: ~s\n", [File]),
    case file:open(File,[read]) of
	{ok, Fd} ->
	    ?dbg("Open file: ~s\n", [File]),
	    State = save(S),
	    Level = S#s.inc_level + 1,
	    S1 = S#s { fd = Fd,
		       readf = fun() -> file:read(Fd, ?CHUNK_SIZE) end,
		       file = File,
		       line = 1,
		       line1 = 1,
		       inc_level = Level,
		       inc_stack = [State | S#s.inc_stack],
		       if_stack = [],
		       cbuf = [],
		       lbuf = [],
		       cwd = filename:dirname(File)
		     },
	    {auto_line(1,File,1),S1};
	{error, enoent} ->
	    cpp_include_path(S,Name,Ps);
	{error,Error} ->
	    error(S, "~s: error ~s", [File, Error]),
	    {empty, S}
    end;
cpp_include_path(S,File,[]) ->
    error(S, "~s: No such file or directory", [File]),
    {empty,S}.
    
%%
%% FIXME: store buffers positions and reopen,
%%        to save number of open file descriptiors 
%%        (when regular files!)
%%
save(S) ->
    ?dbg("Save file ~s:~w\n", [S#s.file,S#s.line]),
    #save { fd       = S#s.fd,
	    readf    = S#s.readf,
	    file     = S#s.file,
	    line     = S#s.line,
	    line1    = S#s.line1,
	    if_stack = S#s.if_stack,
	    cwd      = S#s.cwd,
	    cbuf     = S#s.cbuf,
	    lbuf     = S#s.lbuf,
	    inc_level = S#s.inc_level
	  }.

restore(S,Tok) ->
    case S#s.inc_stack of
	[] ->
	    {Tok,S};
	[R | Is] ->
	    ?dbg("Restore file ~s:~w\n", [File,Line]),
	    LBuf1 = S#s.lbuf ++ [auto_line(R#save.line,R#save.file,2)] ++ 
		R#save.lbuf,
	    S1 = S#s { file= R#save.file, 
		       readf = R#save.readf,
		       fd = R#save.fd, 
		       line= R#save.line,
		       line1= R#save.line1,
		       inc_level = R#save.inc_level,
		       if_stack = R#save.if_stack, 
		       inc_stack = Is,
		       cwd = R#save.cwd, 
		       cbuf = R#save.cbuf, 
		       lbuf = LBuf1
		     },
	    read_0(S1)
    end.
%%
%% Line marker generation
%%  # linenum filename flags
%%
%%  flags:  1  Start of new file
%%          2  Return to a file
%%          3  System header file
%%          4  extern "C" block
%%
auto_line(N, File) ->
    lists:flatten(io_lib:format("# ~w \"~s\"\n",[N, File])).
auto_line(N, File, Flags) ->
    lists:flatten(io_lib:format("# ~w \"~s\" ~w\n",[N, File, Flags])).

%% push directive status line on IF-STACK
push_if(S,D,V) ->
    Stack = S#s.if_stack,
    S#s { if_stack = [{D,V,S#s.line} | Stack]}.

%% pop IF-STACK
pop_if(S) ->
    case S#s.if_stack of
	[] -> 
	    {S,false};
	[_|Stack] ->
	    {S#s { if_stack = Stack }, true}
    end.

%% peek top element on IF-STACK
peek_if(S) ->
    case S#s.if_stack of
	[] -> 
	    empty;
	[E|_] -> 
	    E
    end.
%%
%% stat_if: return 
%%   ?TRUE  : branch is taken
%%   ?FALSE : branch was not taken
%%   ?SKIP  : skiping elif/else until matching endif
%%
stat_if(S) ->
    case S#s.if_stack of
	[{_,V,_}|_] -> V;
	[] -> ?TRUE
    end.
	    

%% modify top element 
mod_if(S,D,V,Ln) ->
    [_ | Stack] = S#s.if_stack,
    S#s { if_stack = [{D,V,Ln} | Stack]}.


parse_macro_args([{char,_,?LP} | Ts]) ->
    parse_macro_args(trim_(Ts),[]);
parse_macro_args(_) ->
    {error, "missing ("}.

parse_macro_args([{char,_,?RP}|Ts], Ps) ->
    check_macro_args(reverse(Ps), Ts);
parse_macro_args(Ts, Ps) ->
    case parse_macro_arg(Ts) of
	Error={error,_} -> Error;
	{P, [{char,_,?RP}|Ts1]} ->
	    check_macro_args(reverse([P|Ps]), Ts1);
	{P, [{char,_,?COMMA}|Ts1]} ->
	    parse_macro_args(trim_(Ts1), [P|Ps]);
	{_P, _} ->
	    {error, "syntax error"}
    end.

parse_macro_arg([{identifier,_,Name},
		 {char,_,?DOT},{char,_,?DOT},{char,_,?DOT}|Ts]) ->
    {{varg,Name}, trim_(Ts)};
parse_macro_arg([{identifier,_,Name}|Ts]) ->
    {{arg,Name}, trim_(Ts)};
parse_macro_arg([{char,_,?DOT},{char,_,?DOT},{char,_,?DOT}|Ts]) ->
    {{varg,"__VA_ARGS__"}, trim_(Ts)};
parse_macro_arg(_) ->
    {error, "argument error"}.


    
check_macro_args(Ns,Ts) ->
    N1 = length(Ns),
    N2 = length(lists:usort(Ns)),
    if N1 =:= N2 ->
	    {ok,Ns,Ts};
       true ->
	    {error, "duplicate macro parameter \"FIXME\""}
    end.

%% Evaluate a #if expression 
%% return 0 or 1
eval(S,#bic_binary { op=Op, arg1=Arg1, arg2=Arg2}) ->
    case Op of
	'+' -> eval(S,Arg1)+eval(S,Arg2);
	'-' -> eval(S,Arg1)-eval(S,Arg2);
	'*' -> eval(S,Arg1)*eval(S,Arg2);
	'/' -> 
	    A1=eval(S,Arg1),A2=eval(S,Arg2),
	    if is_integer(A1), is_integer(A2) ->
		    A1 div A2;
	       true ->
		    A1 / A2
	    end;
	'%' -> eval(S,Arg1) rem eval(S,Arg2);
	'<<' -> eval(S,Arg1) bsl eval(S,Arg2);
	'>>' -> eval(S,Arg1) bsr eval(S,Arg2);
	'&' -> eval(S,Arg1) band eval(S,Arg2);
	'|' -> eval(S,Arg1) bor eval(S,Arg2);
	'^' -> eval(S,Arg1) bxor eval(S,Arg2);
	'&&' -> case eval(S,Arg1) of
		    ?FALSE -> ?FALSE;
		    _ -> eval(S,Arg2)
		end;
	'||' -> case eval(S,Arg1) of
		    ?FALSE -> eval(S,Arg2);
		    V -> V
		end;
	'>' -> case eval(S,Arg1) > eval(S,Arg2) of
		   true -> ?TRUE;
		   false -> ?FALSE
	       end;
	'>=' -> case eval(S,Arg1) >= eval(S,Arg2) of
		    true -> ?TRUE;
		    false -> ?FALSE
		end;
	'<' -> case eval(S,Arg1) < eval(S,Arg2) of
		   true -> ?TRUE;
		   false -> ?FALSE
	       end;
	'<=' -> case eval(S,Arg1) =< eval(S,Arg2) of
		    true -> ?TRUE;
		    false -> ?FALSE
		end;
	'==' -> case eval(S,Arg1) == eval(S,Arg2) of
		    true -> ?TRUE;
		    false -> ?FALSE
		end;
	'!=' -> case eval(S,Arg1) =/= eval(S,Arg2) of
		    true -> ?TRUE;
		    false -> ?FALSE
		end;
	_ ->
	    ?FALSE
    end;
eval(S,#bic_unary { op=Op, arg=Arg }) ->
    case Op of
	'~' -> bnot eval(S,Arg);
	'+' -> eval(S,Arg);
	'-' -> - eval(S,Arg);
	'!' -> case eval(S,Arg) of
		   ?FALSE -> ?TRUE;
		   _ -> ?FALSE
	       end;
	_ -> ?FALSE
    end;
eval(S,#bic_call { func=Func, args=Args }) ->
    case Func of
	{identifier,_,"defined"} ->
	    case Args of
		[{identifier,_,ID}] ->
		    defined(S,ID);
		_ ->
		    ?FALSE
	    end;
	_ -> ?FALSE
    end;
eval(_S,#bic_constant { base=Base, value=Value }) ->
    if is_integer(Base) -> to_integer(Value,Base);
       Base=:=char -> hd(Value);
       Base=:=float -> list_to_float(Value)
    end;
eval(S,#bic_id { name=ID }) -> value_(S,ID).
	    

value_(S,Var) ->
    case dict_value(S, Var) of
	[] -> 0;
	[{decnum,_,Val}] -> to_integer(Val, 10);
	[{hexnum,_,Val}] -> to_integer(Val, 16);
	[{octnum,_,Val}] -> to_integer(Val, 8);
	[{binnum,_,[$0,$b|Val]}] -> to_integer(Val, 2);
	[{flonum,_,Val}] -> to_float(Val);
	[{string,_,Val}] -> Val;
	_ -> 0
    end.

to_integer(String) ->
    to_integer(String, 0).

to_integer("-"++String, Base) ->
    -to_integer(String, Base);
to_integer("0x"++String, Base) when Base =:= 16; Base =:= 0 ->
    to_integer(String,16);
to_integer("0X"++String, Base)  when Base =:= 16; Base =:= 0 ->
    to_integer(String,16);
to_integer("0b"++String, Base)  when Base =:= 2; Base =:= 0 ->
    to_integer(String,2);
to_integer("0", _Base) -> 0;
to_integer("0"++String, Base) when Base =:= 8; Base =:= 0 ->
    to_integer(String,8);
to_integer(String, 0) ->
    try erlang:list_to_integer(String, 10) of
	Value -> Value
    catch
	_:_ -> 0
    end;	
to_integer(String, Base) ->
    try erlang:list_to_integer(String, Base) of
	Value -> Value
    catch
	_:_ -> 0
    end.

to_float(String) ->
    try list_to_float(String) of
	Value -> Value
    catch
	_:_ -> 0
    end.

defined(S,Var) ->
    case maps:find(Var,S#s.defs) of
	error ->
	    if Var =:= "__LINE__";
	       Var =:= "__FILE__" ->
		    ?TRUE;
	       true ->
		    ?FALSE
	    end;
	{ok,?UNDEF} -> ?FALSE;
	_ -> ?TRUE
    end.

define_macro(S, Name, Ts) ->
    case parse_macro_args(Ts) of
	{ok,Ps,Ts1} ->
	    Ts2 = trim_(Ts1),
	    define(S,Name,{Ps,Ts2});
%%	{error, "argument error"} -> %% kludge?
%%	    define(S,Name,trim_(Ts));
	{error,Msg} ->
	    error(S, "~s in macro argument list", [Msg]),
	    S
    end.

define(S,Var,Value) ->
    ?dbg("Define: ~s = ~p\n", [Var, Value]),
    if  Var =:= "__INCLUDE_LEVEL__" ->
	    warning(S, "can not redefine ~s", [Var]),
	    S;
	true ->
	    case defined(S,Var) of
		1 -> warning(S, "\"~s\" redefined\n", [Var]);
		0 -> ok
	    end,
	    D = maps:put(Var, Value, S#s.defs),
	    S#s { defs = D }
    end.

%% macro operations
undef(S, Var) ->
    if  Var =:= "__INCLUDE_LEVEL__" ->
	    warning(S, "can not undefine ~s", [Var]),
	    S;
	Var =:= "__LINE__";
	Var =:= "__FILE__" ->
	    warning(S, "undefining ~s", [Var]),
	    D = maps:put(Var, ?UNDEF, S#s.defs),
	    S#s { defs = D };
	true ->
	    D = maps:remove(Var, S#s.defs),
	    S#s { defs = D }
    end.


%% Get variable token value 
dict_value(S,"__LINE__") ->
    case maps:find("__LINE__",S#s.defs) of
	error -> [{decnum,S#s.line,integer_to_list(S#s.line)}];
	{ok,?UNDEF} -> [];
	{ok,Val} -> Val
    end;
dict_value(S,"__FILE__") ->
    case maps:find("__FILE__",S#s.defs) of
	error -> [{string,S#s.line,S#s.file}];
	{ok,?UNDEF} -> [];
	{ok,Val} -> Val
    end;
dict_value(S,ID) ->
    case maps:find(ID,S#s.defs) of
	error -> [];
	{ok,{param,Val}} -> Val;
	{ok,Val} -> Val
    end.

dict_param(S, ID) ->
    case maps:find(ID,S#s.defs) of
	{ok,{param,Val}} -> Val;
	_ -> []
    end.

%% 
%% Fixme: mark state error 
%%
warning(S, Fmt, As) ->
    io:format(standard_error, 
	      "~s:~w: warning: "++Fmt++"\n", 
	      [S#s.file, S#s.line | As]).    

error(S, Fmt, As) ->
    io:format(standard_error, 
	      "~s:~w: error: "++Fmt++"\n", 
	      [S#s.file, S#s.line | As]).

warning_ts(S, Ts) ->
    Chars = ts_to_cs(trim(Ts)),
    io:format("~s:~w: warning: #warning ~s\n", 
	      [S#s.file, S#s.line,Chars]).

%% fixme: signal error 
error_ts(S, Ts) ->
    Chars = ts_to_cs(trim(Ts)),
    io:format(standard_error, 
	      "~s:~w: error: #error ~s\n", 
	      [S#s.file, S#s.line,Chars]).


read_cbuf_line(S) ->
    case S#s.cbuf of
	eof ->
	    {eof, S};
	Err={error,_} -> 
	    {Err, S};
	CBuf ->
	    case read_raw_line(CBuf, S#s.readf) of
		{"", eof} ->
		    {eof, S#s {cbuf=eof}};
		{"", Err={error,_Reason}} ->
		    {Err, S#s {cbuf=Err}};
		{CLine,CBuf1} ->
		    ?dbg("cbuf_line: [~p]\n", [CLine]),
		    {CLine, S#s {cbuf=CBuf1}}
	    end
    end.

%%
%% Read one line of characters, 
%% special treat of \ for line continuation
%% then store a \n with-in the line
%% return {Line, Continue}
%%
read_raw_line(Cs,More) ->
    read_raw_line(Cs,[],More).

read_raw_line(Cs, Acc, More) ->
    case read_next_char(Cs, Acc, More) of
	{$/, Cs1, Acc1} ->
	    case read_next_char(Cs1, Acc1, More) of
		{$*, Cs2, Acc2} ->
		    read_raw_comment(Cs2, Acc2, More);
		{$/, Cs2, Acc2} ->
		    read_raw_line_comment(Cs2, Acc2, More);
		{eof, _Cs2, Acc2} ->
		    {reverse(Acc2), eof};
		{{error,_}=Error,_Cs2,Acc2} ->
		    {reverse(Acc2), Error};
		{C, Cs2, Acc2} ->
		    read_raw_line([C|Cs2], [$/|Acc2], More)
	    end;
	{$\n, Cs1, Acc1} ->
	    {reverse([$\n|Acc1]), Cs1};
	{eof, _Cs1, Acc1} ->
	    {reverse(Acc1), eof};
	{{error,_}=Error,_Cs1,Acc1} ->
	    {reverse(Acc1), Error};
	{C, Cs1, Acc1} ->
	    read_raw_line(Cs1, [C|Acc1], More)
    end.

read_raw_comment(Cs, Acc, More) ->
    case read_next_char(Cs, Acc, More) of
	{$*, Cs1, Acc1} ->
	    case read_next_char(Cs1, Acc1, More) of
		{$/, Cs2, Acc2} ->
		    read_raw_line(Cs2, [$\s|Acc2], More);
		{eof, _Cs2, Acc2} ->
		    {reverse(Acc2), eof};
		{{error,_}=Error,_Cs2,Acc2} ->
		    {reverse(Acc2), Error};
		{C, Cs2, Acc2} ->
		    read_raw_comment([C|Cs2], Acc2, More)
	    end;
	{eof, _Cs1, Acc1} ->
	    {reverse(Acc1), eof};
	{{error,_}=Error,_Cs1,Acc1} ->
	    {reverse(Acc1), Error};
	{$\n, Cs1, Acc1} ->
	    read_raw_comment(Cs1, [$\n|Acc1], More);
	{_C, Cs1, Acc1} ->
	    read_raw_comment(Cs1, Acc1, More)
    end.
	
		 
read_raw_line_comment(Cs, Acc, More) ->
    case read_next_char(Cs, Acc, More) of
	{$\n, Cs1, Acc1} ->
	    {reverse([$\n|Acc1]), Cs1};
	{eof, _Cs1, Acc1} ->
	    {reverse(Acc1), eof};
	{{error,_}=Error,_Cs1,Acc1} ->
	    {reverse(Acc1), Error};
	{_, Cs1, Acc1} ->
	    read_raw_line_comment(Cs1, Acc1, More)
    end.

%% Read next character
read_next_char([$\\|Cs], Acc, More) ->
    read_next_char_bs(Cs, Acc, More);
read_next_char([C|Cs], Acc, _More) ->
    {C, Cs, Acc};
read_next_char([], Acc, More) ->
    read_more(Acc, More, fun read_next_char/3).

%% check chars after backslash
read_next_char_bs([$\r,$\n|Cs],Acc,More) ->
    read_next_char(Cs,[$\n|Acc],More);
read_next_char_bs([$\n|Cs], Acc, More) ->
    read_next_char(Cs,[$\n|Acc],More);
read_next_char_bs([$\r],Acc,More) ->
    read_more(Acc, More, fun read_next_char_bsr/3);
read_next_char_bs([$\s|Cs], Acc, More) ->
    read_next_char_bs(Cs, [$\s|Acc], More);
read_next_char_bs([$\t|Cs], Acc, More) ->
    read_next_char_bs(Cs, [$\t|Acc], More);
read_next_char_bs([], Acc, More) ->
    read_more(Acc, More, fun read_next_char_bs/3);
read_next_char_bs(Cs, Acc, _More) ->
    {$\\, Cs, Acc}.

read_next_char_bsr([$\n|Cs],Acc,More) ->
    read_next_char(Cs, [$\n|Acc], More);
read_next_char_bsr(Cs,Acc,More) ->
    read_next_char(Cs, [$\n|Acc], More).

read_more(Acc, More, Cont) ->
    case More() of
	eof -> {eof, [], Acc};
	Error={error,_} -> {Error,[],Acc};
	{ok,Cs} -> Cont(Cs, Acc, More)
    end.

%%
%% Format tokens as a string
%%
format_tokens(Ts) ->
    ts_to_cs(Ts).

%%
%% Format macro arguments
%%
format_params([A]) ->
    case A of
	{arg,Name} -> Name;
	{varg,"__VA_ARGS__"} -> "...";
	{varg,Name} -> Name++"..."
    end;
format_params([{arg,Name}|As]) ->
    Name ++ "," ++ format_params(As);
format_params([]) ->
    "".

%%
%% Convert token list to character list
%%
ts_to_cs(Ts) ->
    ts_to_cs(Ts,[]).

ts_to_cs([T|Ts],Acc) ->
    case T of
	{char,_Ln,X}   -> ts_to_cs(Ts,[X|Acc]);
	{blank,_Ln,_X}  -> ts_to_cs(Ts,[$\s|Acc]);
	{binnum,_Ln,X} -> ts_to_cs(Ts,[X|Acc]);
	{octnum,_Ln,X} -> ts_to_cs(Ts,[X|Acc]);
	{decnum,_Ln,X} -> ts_to_cs(Ts,[X|Acc]);
	{hexnum,_Ln,X} -> ts_to_cs(Ts,[X|Acc]);
	{string,_Ln,X} -> ts_to_cs(Ts,[?Q,X,?Q|Acc]);
	{identifier,_Ln,X} -> ts_to_cs(Ts,[X|Acc])
    end;
ts_to_cs([],Acc) ->
    lists:flatten(reverse(Acc)).

%%
%% Tokenize one line
%%
%% return:
%%  {ListOfTokens,Line'}
%%
tokens(Cs) ->
    tokens(Cs, 1, [], []).

tokens(eof,_Ln) ->
    {eof,[]};
tokens(Err={error,_},_Ln) ->
    {Err,[]};
tokens(Cs, Ln) ->
    tokens(Cs, Ln, [], []).

tokens(Cs,Ln,Ns,Ts) ->
    case token(Cs,Ln,[]) of
	{{eol,_Ln},Ns1} ->
	    {reverse(Ts),Ns++Ns1};
	{{error,Ln},Ns1,_Cs1} ->
	    {[{error,Ln}],Ns++Ns1};
	{T,Ns1,Cs1} ->
	    tokens(Cs1,Ln+length(Ns1),Ns++Ns1,[T|Ts])
    end.

%%
%% Scan for a token
%% return:
%%   {Token, Cs', NewLines}
%%

token(Chars) ->
    token(Chars, 1, []).

token(Cs,Ln,Ns) ->
    case Cs of
	[$\s|Cs1] -> blank(Cs1,Ln,Ns,[$\s]);
        [$\t|Cs1] -> blank(Cs1,Ln,Ns,[$\t]);
        [$\n|Cs1] -> token(Cs1,Ln,[$\n|Ns]);
        [?Q|Cs1]  -> quoted(Cs1,Ln,Ns,?Q,string,[]);
	[$0|Cs1]  -> zero(Cs1,Ln,Ns,[$0]);
	[$-|Cs1]  -> minus(Cs1,Ln,Ns,[$-]);
        [C|Cs1] when ?is_dec_digit(C) -> decnum(Cs1,Ln,Ns,[C]);
        [C|Cs1] when ?is_alpha(C) -> identifier(Cs1,Ln,Ns,[C]);
        [C|Cs1] -> {{char,Ln,C},Ns,Cs1};
	[] -> {{eol,Ln},Ns}
    end.

minus(Cs,Ln,Ns,Acc) ->
    case Cs of
	[$\n|Cs1] -> minus(Cs1,Ln,[$\n|Ns],Acc);
	[$0|Cs1]  -> zero(Cs1,Ln,Ns,[$0|Acc]);
	[C|Cs1] when ?is_dec_digit(C) -> decnum(Cs1,Ln,Ns,[C|Acc]);
	_ -> {{char,Ln,Acc},Ns,Cs}
    end.
	    
zero(Cs,Ln,Ns,Acc) ->
    case Cs of
	[$\s|_] -> {{decnum,Ln,Acc},Ns,Cs};
	[$\t|_] -> {{decnum,Ln,Acc},Ns,Cs};
	[$\n|Cs1] -> zero(Cs1,Ln,[$\n|Ns],Acc);
	[$x|Cs1] -> hexnum(Cs1,Ln,Ns,[$x|Acc]);
	[$b|Cs1] -> binnum(Cs1,Ln,Ns,[$b|Acc]);
	[C|Cs1] when ?is_oct_digit(C) -> octnum(Cs1,Ln,Ns,[C|Acc]);
        [C|Cs1] when ?is_alpha(C) -> identifier(Cs1,Ln,Ns,[C|Acc]);
	_ -> {{decnum,Ln,Acc},Ns,Cs}
    end.

quoted(Cs,Ln,Ns,Q,Tag,Acc) ->
    case Cs of
        [Q|Cs1] ->
            {{Tag,Ln,reverse(Acc)},Ns,Cs1};
	[$\n|Cs1] ->
	    quoted(Cs1,Ln,[$\n|Ns],Q,Tag,Acc);
        [C|Cs1] ->
            quoted(Cs1,Ln,Ns,Q,Tag,[C|Acc]);
	[] ->
	    {{error,Ln},Ns,reverse(Acc)}
    end.

blank(Cs,Ln,Ns,Acc) ->
    scan_while(Cs,Ln,Ns,fun(C) -> ?is_blank(C) end, blank, Acc).

binnum(Cs,Ln,Ns,Acc) ->
    scan_while(Cs,Ln,Ns,fun(C) -> ?is_bin_digit(C) end, binnum, Acc).

octnum(Cs,Ln,Ns,Acc) ->
    scan_while(Cs,Ln,Ns,fun(C) -> ?is_oct_digit(C) end, octnum, Acc).

decnum(Cs,Ln,Ns,Acc) ->
    scan_while(Cs,Ln,Ns,fun(C) -> ?is_dec_digit(C) end, decnum, Acc).

hexnum(Cs,Ln,Ns,Acc) ->
    scan_while(Cs,Ln,Ns,fun(C) -> ?is_hex_digit(C) end, hexnum, Acc).

identifier(Cs,Ln,Ns,Acc) ->
    scan_while(Cs,Ln,Ns,fun(C) -> ?is_identifier(C) end, identifier, Acc).

scan_while(Cs,Ln,Ns,While,Tag,Acc) ->
    case Cs of
	[$\n] ->
	    {{Tag,Ln,reverse(Acc)},[$\n|Ns],[]};
	[$\n|Cs1] ->
	    scan_while(Cs1,Ln,[$\n|Ns],While,Tag,Acc);
	[] ->
	    {{Tag,Ln,reverse(Acc)},Ns,[]};	    
	[C|Cs1] ->
            case While(C) of
                true -> scan_while(Cs1,Ln,Ns,While,Tag,[C|Acc]);
                false -> {{Tag,Ln,reverse(Acc)},Ns,[C|Cs1]}
            end
    end.

%% collect tokens for search path  ... >
search_path([{char,_,$<}|Ts]) -> search_path(Ts, []);
search_path(Ts) -> search_path(Ts,[]).
    
search_path([{char,_,$>}|_Ts], Acc) ->
    {ok,ts_to_cs(trim_(reverse(trim_(Acc))))};
search_path([T|Ts], Acc) -> search_path(Ts, [T|Acc]);
search_path([],_Acc) ->
    {error, "missing character >", []}.

trim(Ts) ->
    reverse(trim_(reverse(trim_(Ts)))).

trim_([{blank,_,_}|Ts]) -> trim_(Ts);
trim_(Ts) -> Ts.
