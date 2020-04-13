%%% File    : bic.erl
%%% Author  : Tony Rogvall <tony@iMac.local>
%%% Description : BEAM interpreted C (ode)
%%% Created : 27 Dec 2005 by Tony Rogvall <tony@iMac.local>

-module(bic).

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

-spec file(Filename::string(), Env::cpp_options()) ->
		  {ok, bic()} | {error, Reason::term()}.

file(Filename,Env) ->
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
		    case bic_lint:forms(Filename,Forms) of
			{ok,_LintForms} ->
			    {ok,Forms};
			Err={error,_} ->
			    Err
		    end
	    end;
	Error ->
	    Error
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
