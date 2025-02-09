%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    Transform: Make local variables unique 
%%% @end
%%% Created :  8 Dec 2020 by Tony Rogvall <tony@rogvall.se>

-module(bic_unique).

-export([definitions/1, definitions/3]).
-export([definition/1, definition/3]).

-include("../include/bic.hrl").

%%-define(dbg(F,A), io:format((F),(A))).
%%-define(dbg(F), io:format((F))).
-define(dbg(F,A), ok).
-define(dbg(F), ok).

-spec definitions(Ds::bic_definitions()) ->
	  bic_definitions().

definitions(Ds) ->
    definitions(Ds,#{},[]).

-spec definitions(Ds::bic_definitions(), Options::map(), 
		  FunctionNames::[string()]) ->
	  bic_definitions().

definitions([D|Ds],Opts,Fs) ->
    D1 = definition(D, Opts, Fs),
    [D1 | definitions(Ds,Opts,Fs)];
definitions([],_Opts,_Fs) ->
    [].

definition(D) ->
    definition(D,#{},[]).

definition(D=#bic_function{name=Name,params=Params,body=Body},
	   _Opts,Fs) ->
    ?dbg("unique function ~s \n", [Name]),
    case (Fs =:= []) orelse lists:member(Name, Fs) of
	true ->
	    S0 = bic_scope:new(),
	    S1 = bic_scope:set(next,1,S0),
	    %% keep parameter names, they must be unique
	    S2 = lists:foldl(fun(#bic_decl{name=Para},Si) ->
				     bic_scope:put(Para,Para,Si)
			     end, S1, Params),
	    {Body1,_} = statements(Body, S2),
	    D#bic_function{body=Body1};
	false ->
	    D
    end;
definition(D, _Opts, _Fs) ->
    D.


statements(undefined, Scope) ->
    {undefined,Scope};
statements(Body, Scope) when is_list(Body) ->
    bic_transform:fold_list(
      fun(F=#bic_begin{}, S0) ->
	      ?dbg("push\n"),
	      S1 = bic_scope:push(S0),
	      {F, S1};
	 (F=#bic_end{}, S0) ->
	      ?dbg("pop\n"),
	      S1 = bic_scope:pop(S0),
	      {F, S1};
	 (F=#bic_id{name=V}, S0) ->
	      V1 = bic_scope:get(V, S0),
	      ?dbg("lookup ~s = ~s\n", [V, V1]),
	      {F#bic_id{name=V1}, S0};
	 (F=#bic_decl{name=V}, S0) ->
	      case bic_scope:get(V, S0, false) of
		  false ->
		      ?dbg("decl var ~s = ~s\n", [V,V]),
		      {F, bic_scope:put(V,V,S0)};
		  _W ->
		      Next = bic_scope:get(next, S0),
		      VV = V ++ "__" ++ integer_to_list(Next),
		      S1 = bic_scope:set(next, Next+1, S0),
		      S2 = bic_scope:put(V, VV, S1),
		      ?dbg("decl var ~s = ~s\n", [V,VV]),
		      {F#bic_decl{name=VV}, S2}
	      end;
	 (F, Mi) ->
	      {F,Mi}
      end, Scope, Body).

