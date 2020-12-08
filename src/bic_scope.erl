%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    Kind of general scope handling
%%% @end
%%% Created :  8 Dec 2020 by Tony Rogvall <tony@rogvall.se>

-module(bic_scope).

-export([new/0, set/3, put/3, get/2, get/3, find/2]).
-export([search/2]).
-export([push/1, pop/1]).

%% -define(dbg(F,A), io:format((F),(A))).
-define(dbg(F,A), ok).


-type scope() :: #{ scope => map(), stack => [map()] }.
%%
%%  Scope is a map that contains the all current key/value bindings
%%  scope are the bindings on the current level
%%  stack contains a scope stack that can restore old value on pop
%%

-spec new() -> scope().
new() ->
    #{ scope=>#{}, stack=>[] }.

%% Push a new empty scope to scope stack
-spec push(Scope::scope()) -> scope().
push(S=#{ scope := Scope, stack := Stack }) ->
    S#{ scope => #{}, stack=> [Scope|Stack] }.

%% Pop current scope, remove values not used and restore old values
-spec pop(Scope::scope()) -> scope().
pop(S=#{ scope := Scope, stack := [Scope1|Stack1] }) ->
    S1 = maps:fold(
	   fun(Var,_,Si) ->
		   case search(Var,S) of
		       error ->
			   ?dbg("remove: ~s\n", [Var]),
			   maps:remove(Var,Si);  %% not used any more
		       {ok,W} ->
			   ?dbg("pop: ~s => ~s\n", [Var, W]),
			   Si#{ Var => W }       %% restore (previous value)
		   end
	   end, S, Scope),
    S1#{ scope=>Scope1, stack=>Stack1 }.

%% use set for current value only that are not tracked (not restored)
-spec set(Key::term(), Value::term(), Scope::scope()) -> scope().
set(Key,Value,S) ->
    S#{ Key => Value }.

%% set both as current value and in current scope
-spec put(Key::term(), Value::term(), Scope::scope()) -> scope().
put(Key,Value,S=#{ scope := Scope} ) ->
    ?dbg("~s => ~s\n", [Name, NewName]),
    S#{ Key => Value, scope => Scope#{ Key => Value } }.

-spec get(Key::term(), Scope::scope()) -> term().
get(Key, S) ->
    maps:get(Key, S).

-spec get(Key::term(), Scope::scope(), Default::term()) -> term().
get(Key, S, Default) ->
    maps:get(Key, S, Default).

-spec find(Key::term(), Scope::scope()) ->
	  error | {ok,term()}.

find(Key, S) ->
    maps:find(Key, S).


-spec search(Key::term(), Scope::scope()) ->
	  error | {ok,term()}.

%% search for old value in scope stack
search(Var, #{ stack := Stack}) ->
    search_(Var, Stack).
    
search_(Var, [Scope|Stack]) ->
    case maps:find(Var, Scope) of
	error -> search_(Var,Stack);
	Found -> Found
    end;
search_(_Var, []) ->
    error.
