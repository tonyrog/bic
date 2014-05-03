%%% File    : bic.erl
%%% Author  : Tony Rogvall <tony@iMac.local>
%%% Description : BEAM interpreted C (ode)
%%% Created : 27 Dec 2005 by Tony Rogvall <tony@iMac.local>

-module(bic).

-compile(export_all).

-import(lists, [map/2]).

-include("../include/bic.hrl").

command() ->
    io:format("bic: no input files\n"),
    halt(1).

command([]) ->
    io:format("bic: no input files\n"),
    halt(1);
command([File]) when is_atom(File) ->
    case file(atom_to_list(File)) of
	{ok,List} ->
	    io:format("~p\n", [List]),
	    halt(0);
	_Err ->
	    halt(1)
    end.
%%
%% Env constist of options passed to bic_cpp
%% {define, Name, Value}
%% {include, Path}
%% {qinclude, Path}
%%

string(String) ->
    string(String, []).

string(String, Env) ->
    Env1 = [{define,"__bic__", 1},
	    {define,"__bic_extension_bitfield__", 1} | Env],
    case bic_cpp:string(String, Env1) of
	{ok,Fd} ->
	    bic_scan:init(),  %% setup some dictionay stuff
	    bic_parse:init(), %% setup some dictionay stuff
	    Res = (catch bic_parse:parse_and_scan({bic, scan, [Fd]})),
	    io:format("Variable dump:\n"),
	    lists:foreach(
	      fun({Name,[{string,_,Value}]}) ->
		      io:format("~s: \"~s\"\n", [Name,Value]);
		 ({Name, Value}) ->
		      io:format("~s: ~p\n", [Name,Value])
	      end, bic_cpp:values(Fd)),
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
		    case bic_lint:forms("*string*", Forms) of
			{ok,_LintForms} ->
			    {ok,Forms};
			Err={error,_} ->
			    Err
		    end
	    end;
	Error ->
	    Error
    end.


file(File) ->
    file(File,[]).

file(File,Env) ->
    Env1 = [{define,"__bic__", 1},
	    {define,"__bic_extension_bitfield__", 1} | Env],
    case bic_cpp:open(File, Env1) of
	{ok,Fd} ->
	    bic_scan:init(),  %% setup some dictionay stuff
	    bic_parse:init(), %% setup some dictionay stuff
	    Res = (catch bic_parse:parse_and_scan({bic, scan, [Fd]})),
	    io:format("Variable dump:\n"),
	    lists:foreach(
	      fun({Name,[{string,_,Value}]}) ->
		      io:format("~s: \"~s\"\n", [Name,Value]);
		 ({Name, Value}) ->
		      io:format("~s: ~p\n", [Name,Value])
	      end, bic_cpp:values(Fd)),
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
			      [File,Ln,Mod:format_error(Message)]),
		    {error,parse_error};
		{ok,Forms} ->
		    case bic_lint:forms(File,Forms) of
			{ok,_LintForms} ->
			    {ok,Forms};
			Err={error,_} ->
			    Err
		    end
	    end;
	Error ->
	    Error
    end.

%% Scanner wrapper (simplify debugging)
scan(Fd) ->
    Res = bic_scan:scan(Fd),
    %% io:format("Scan: ~p\n", [Res]),
    Res.
