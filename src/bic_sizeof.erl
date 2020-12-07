%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    sizeof types
%%% @end
%%% Created :  7 Dec 2020 by Tony Rogvall <tony@rogvall.se>

-module(bic_sizeof).

-export([sizeof/1, sizeof/2]).

-include("../include/bic.hrl").

%% sizeof in bytes
sizeof(Type) ->
    sizeof(Type, erlang:system_info(wordsize)*8).
sizeof(#bic_type{type=undefined,size=S},Model) ->
    sizeof_type(int, S, Model);
sizeof(#bic_type{type=T,size=S},Model) ->
    sizeof_type(T, S, Model).

sizeof_type(char, _, _) -> 1;
sizeof_type(int, short, _) -> 2;
sizeof_type(int, undefined, 32) -> 4;
sizeof_type(int, undefined, 64) -> 4;
sizeof_type(int, long, 32) -> 4;
sizeof_type(int, long, 64) -> 8;
sizeof_type(int, long_long, 32) -> 8;
sizeof_type(int, long_long, 64) -> 8;
sizeof_type(float, undefined, _) -> 4;
sizeof_type(double, long, _) -> 10;
sizeof_type(double, undefined, _) -> 8;
sizeof_type(#bic_typeid{name="uint8_t"},_,_) -> 1;
sizeof_type(#bic_typeid{name="uint16_t"},_,_) -> 2;
sizeof_type(#bic_typeid{name="uint32_t"},_,_) -> 4;
sizeof_type(#bic_typeid{name="uint64_t"},_,_) -> 8;
sizeof_type(#bic_typeid{name="int8_t"},_,_) -> 1;
sizeof_type(#bic_typeid{name="int16_t"},_,_) -> 2;
sizeof_type(#bic_typeid{name="int32_t"},_,_) -> 4;
sizeof_type(#bic_typeid{name="int64_t"},_,_) -> 8;
sizeof_type(#bic_typeid{name="wchar_t"},_,_) -> 4;  %% 16-bit? when?
sizeof_type(#bic_typeid{name="size_t"},_,32) -> 4;
sizeof_type(#bic_typeid{name="size_t"},_,64) -> 8;
sizeof_type(#bic_pointer{},_,32) -> 4;
sizeof_type(#bic_pointer{},_,64) -> 8.
%% FIXME: array/struct/union
