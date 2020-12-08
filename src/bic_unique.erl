%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    Transform: Make local variables unique 
%%% @end
%%% Created :  8 Dec 2020 by Tony Rogvall <tony@rogvall.se>

-module(bic_unique).

-export([definitions/1, definitions/2]).

-include("../include/bic.hrl").

-spec definitions(Ds::bic_definitions()) ->
	  bic_definitions().

definitions(Ds) ->
    definitions(Ds,[]).

-spec definitions(Ds::bic_definitions(), FunctionNames::[string()]) ->
	  bic_definitions().

definitions([D|Ds], Fs) ->
    case D of
	#bic_function{name=Name,params=Params,body=Body} ->
	    case (Fs =:= []) orelse lists:member(Name, Fs) of
		true ->
		    S0 = bic_scope:new(),
		    S1 = bic_scope:set(next,1,S0),
		    %% keep parameter names, they must be unique
		    S2 = lists:foldl(fun(#bic_decl{name=Name},Si) ->
					     bic_scope:put(Name,Name,Si)
				     end, S1, Params),
		    {Body1,_} = statements(Body, S2),
		    [D#bic_function{body=Body1} | definitions(Ds,Fs)];
		false ->
		    [D | definitions(Ds,Fs)]
	    end;
	_ ->
	    [D | definitions(Ds,Fs)]
    end;
definitions([],_Fs) ->
    [].

statements(undefined, Scope) ->
    {undefined,Scope};
statements(Body, Scope) when is_list(Body) ->
    bic_transform:fold_list(
      fun(F=#bic_begin{}, S0) ->
	      S1 = bic_scope:push(S0),
	      {F, S1};
	 (F=#bic_end{}, S0) ->
	      S1 = bic_scope:pop(S0),
	      {F, S1};
	 (F=#bic_id{name=V}, S0) ->
	      V1 = bic_scope:get(V, S0),
	      {F#bic_id{name=V1}, S0};
	 (F=#bic_decl{name=V}, S0) ->
	      case bic_scope:get(V, S0, false) of
		  false ->
		      {F, bic_scope:put(V,V,S0)};
		  _W ->
		      Next = bic_scope:get(next, S0),
		      VV = V ++ "__" ++ integer_to_list(Next),
		      S1 = bic_scope:set(next, Next+1, S0),
		      S2 = bic_scope:put(V, VV, S1),
		      {F#bic_decl{name=VV}, S2}
	      end;
	 (F, Mi) ->
	      {F,Mi}
      end, Scope, Body).

