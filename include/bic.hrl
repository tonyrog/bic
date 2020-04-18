%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2011, Tony Rogvall
%%% @doc
%%%    Record definition of parse elements
%%% @end
%%% Created : 10 Jan 2011 by Tony Rogvall <tony@rogvall.se>

-ifndef(__BIC_HRL__).
-define(__BIC_HRL__, true).

-type bic_type() ::
	bic_typeid() |
	bic_typespec() |
	bic_enum() |
	bic_pointer() |
	bic_array() |
	bic_fn() |
	bic_struct() |
	bic_union().

-type bic_expr() ::
	bic_id() |
	bic_constant() |
	bic_unary() |
	bic_binary() |  %% include ',' expression!
	bic_call() |
	bic_ifexpr() |
	bic_assign().

-type bic_statement() ::
	bic_expr() |  %% expression statement
	bic_decl() |
	bic_for() |
	bic_while() |
	bic_do() |
	bic_if() |
	bic_switch() |
	bic_case() |
	bic_default() |
	bic_label() |
	bic_goto() |
	bic_continue() |
	bic_break() |
	bic_return() |
	bic_empty() |
	[ bic_statement() ]. %% compound statement

-type bic_declaration() ::
	bic_decl() |
	bic_function().

-type bic_forms() ::
	[bic_declaration()].


-type bic_storage() :: undefined | auto | static | register | extern.
-type bic_sign()    :: undefined | signed | unsigned.
-type bic_size()    :: undefined | short | long | long_long.

-record(bic_typeid,
	{
	 line :: integer(),
	 const :: boolean(),   %% data is constant
	 name :: string()
	}).
-type bic_typeid() :: #bic_typeid{}.

%% Basic type specifier
-record(bic_type,
	{
	 line      :: integer(),
	 sign      :: bic_sign(),
	 const     :: undefined | boolean(),
	 volatile  :: undefined | boolean(),
	 size      :: bic_size(),
	 type      :: undefined | '_' | void | char | int | float | double |
		      bic_struct() | bic_union() | bic_typeid() | bic_enum() |
		      bic_pointer()
	}).
-type bic_typespec() :: #bic_type{} | bic_struct() | bic_union() |
			bic_typeid() | bic_enum().

-record(bic_enum,
	{
	 line :: integer(),
	 name :: string(),
	 elems :: [{string(),term()}]
	}).
-type bic_enum() :: #bic_enum{}.

%% Pointer type
-record(bic_pointer,
	{
	 line      :: integer(),
	 type      :: bic_type()
	}).
-type bic_pointer() :: #bic_pointer{}.

-record(bic_array,
	{
	 line      :: integer(),
	 type      :: bic_type(),
	 dim       :: [] | bic_expr()
	}).
-type bic_array() :: #bic_array{}.

-record(bic_struct,
	{
	 line :: integer(),
	 name :: string(),
	 elems :: [bic_decl()]
	}).
-type bic_struct() :: #bic_struct{}.

-record(bic_union,
	{
	 line :: integer(),
	 name :: string(),
	 elems :: [bic_decl()]
	}).
-type bic_union() :: #bic_union{}.

-record(bic_fn,
	{
	 line   :: integer(),
	 type   :: bic_type(),   %% return type
	 params :: [bic_decl()]
	}).
-type bic_fn() :: #bic_fn{}.


-record(bic_id,
	{ 
	  line :: integer(),
	  name :: string()
	}).
-type bic_id() :: #bic_id{}.
		    
-record(bic_constant,
	{
	  line :: integer(),
	  base :: 8 | 10 | 16 | char | float | string,
	  value :: string()
	}).
-type bic_constant() :: #bic_constant{}.

-type unary_op() :: '&' | '*' | '+' | '-' | '~' | '!'.
	  
-record(bic_unary,
	{
	 line :: integer(),
	 op   :: unary_op(),
	 arg  :: bic_expr()
	}).
-type bic_unary() :: #bic_unary{}.

-type binary_op() :: 
	'->' | '.' |
	'++' | '--' | '=' |
	'<<' | '>>' | '<' | '>' | '>=' | '<=' | '==' | '!=' |
	'&&' | '||' |
	'+' | '-' | '*' | '/' | '%' |
	'&' | '|' | '^'.

-record(bic_binary,
	{
	 line :: integer(),
	 op   :: binary_op(),
	 arg1 :: bic_expr(),
	 arg2 :: bic_expr()
	}).
-type bic_binary() :: #bic_binary{}.

-record(bic_call,
	{
	 line :: integer(),
	 func :: bic_expr(),  %% typically #bic_constant{base=string}
	 args :: [bic_expr()]
	}).
-type bic_call() :: #bic_call{}.
	
%%  cond ? then : else FIXME? GNU:  cond ? then
-record(bic_ifexpr,
	{
	 line :: integer(),
	 test :: bic_expr(),
	 then :: bic_expr(),
	 else :: bic_expr()
	}).
-type bic_ifexpr() :: #bic_ifexpr{}.

-type assign_op() :: '=' | '*=' | '/=' | '%=' | '+=' | '-=' |
		   '<<=' | '>>=' | '&=' | '^=' | '|='.

-record(bic_assign,
	{
	 line :: integer(),
	 op   :: assign_op(),
	 lhs  :: bic_expr(),
	 rhs  :: bic_expr()
	}).
-type bic_assign() :: #bic_assign{}.


%% Specialize declaration - simplify processing a bit
-record(bic_typedef,
	{
	 line :: integer(),
	 name :: string(),   %% name of type defined
	 storage :: bic_storage(),
	 type :: bic_type(),    %% type spec
	 size :: bic_expr(),    %% optional constant bit field size
	 value :: bic_expr()    %% optional init value?
	}).
-type bic_typedef() :: #bic_typedef{}.

-record(bic_decl,
	{
	 line :: integer(),
	 name :: string(),   %% name of type defined
	 storage :: bic_storage(),
	 type :: bic_type(),  %% type spec
	 size :: bic_expr(),  %% optional constant bit field size
	 value :: bic_expr()  %% optional init value
	}).
-type bic_decl() :: #bic_decl{}.

%% Function declaration
-record(bic_function,
	{
	 line :: integer(),        %% line number
	 name :: string(),
	 storage :: bic_storage(), %% storage class specifier
	 type :: bic_type(),       %% return type
	 params :: [bic_decl()],   %% list of parameters
	 body                      %% function body
	}).
-type bic_function() :: #bic_function{}.

-record(bic_for,
	{
	 line :: integer(),    %% line number
	 init :: bic_expr(),
	 test :: bic_expr(),
	 update :: bic_expr(),
	 body :: bic_statement()
	 }).
-type bic_for() :: #bic_for{}.

-record(bic_while,
	{
	 line :: integer(),    %% line number
	 test :: bic_expr(),
	 body :: bic_statement()
	}).
-type bic_while() :: #bic_while{}.

-record(bic_do,
	{
	 line :: integer(),    %% line number
	 body :: bic_statement(),
	 test :: bic_expr()
	}).
-type bic_do() :: #bic_do{}.

-record(bic_if,
	  {
	  line :: integer(),
	  test :: bic_expr(),
	  then :: bic_statement(),
	  else :: bic_statement()
	  }).
-type bic_if() :: #bic_if{}.

-record(bic_switch,
	{
	  line :: integer(),
	  expr :: bic_expr(),
	  body :: bic_statement() 
	}).
-type bic_switch() :: #bic_switch{}.

-record(bic_case,
	{
	 line :: integer(),
	 expr :: bic_expr(),
	 code :: bic_statement()
	 }).
-type bic_case() :: #bic_case{}.

-record(bic_default,
	{
	 line :: integer(),
	 code :: bic_statement()
	}).
-type bic_default() :: #bic_default{}.

-record(bic_label,
	{
	 line :: integer(),
	 name :: string(),
	 code :: bic_statement()
	 }).
-type bic_label() :: #bic_label{}.
	
-record(bic_goto,
	{
	 line :: integer(),
	 label :: bic_expr()  %% allow for variable goto!
	 }).
-type bic_goto() :: #bic_goto{}.

-record(bic_continue,
	{
	 line :: integer()
	}).
-type bic_continue() :: #bic_continue{}.

-record(bic_break,
	{
	 line :: integer()
	}).
-type bic_break() :: #bic_break{}.
	
-record(bic_return,
	{
	 line :: integer(),
	 expr :: bic_expr()
	}).
-type bic_return() :: #bic_return{}.

-record(bic_empty,
	{
	 line :: integer()
	}).
-type bic_empty() :: #bic_empty{}.
	  
-endif.
