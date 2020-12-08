%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    Transformation to extract function and closure
%%% @end
%%% Created :  8 Dec 2020 by Tony Rogvall <tony@rogvall.se>

-module(bic_ref).

-export([definitions/1, definitions/2]).

-include_lib("bic/include/bic.hrl").
-define(dbg(F,A), ok).

-spec definitions(Ds::bic_definitions()) ->
	  bic_definitions().

definitions(Ds) ->
    Ds.

-spec definitions(Ds::bic_definitions(), FunctionNames::[string()]) ->
	  bic_definitions().

definitions(Ds, []) ->
    Ds;
definitions(Ds, Fs) ->
    %% partition Stms in functions=S0,from non-functions S1
    {S0,S1} = lists:partition(fun(S) -> is_record(S, bic_function) end, Ds),
    %% partition functions in Referenced S2 and unreferenced S3
    {S2,S3} = 
	lists:partition(
	  fun(#bic_function{name=Name}) -> lists:member(Name, Fs) end, S0),
    %% extract all calls from S2
    {_,Calls} = 
	bic_transform:fold_list(
	  fun(F=#bic_call{func=#bic_id{name=Name}}, Set) ->
		  {F,Set#{ Name => true }};
	     (F,Set) -> 
		  {F,Set}
	  end, #{}, S2),
    %% Add all functions in S3 that are in Calls
    S4 = 
	lists:filter(
	  fun(#bic_function{name=Name}) ->
		  case maps:find(Name, Calls) of
		      {ok,_} -> true;
		      _ -> false
		  end;
	     (_) -> false
	  end, S3),
    S1 ++ S4 ++ S2.
