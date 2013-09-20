%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2011, Tony Rogvall
%%% @doc
%%%    Record definition of parse elements
%%% @end
%%% Created : 10 Jan 2011 by Tony Rogvall <tony@rogvall.se>

-record(bic_id,
	{ 
	  line,
	  name
	 }).

-record(bic_typeid,
	{
	  line,
	  name
	 }).

-record(bic_constant,
	{
	  line :: integer(),
	  base :: 8 | 10 | 16 | char | float | string,
	  value :: string()
	}).
	  
-record(bic_unary,
	{
	  line :: integer(),
	  op   :: atom(),
	  arg
	 }).

-record(bic_binary,
	{
	  line :: integer(),
	  op   :: atom(),
	  arg1,
	  arg2
	 }).

-record(bic_call,
	{
	  line :: integer(),
	  func,
	  args
	 }).
	
%%  cond ? then : else FIXME? GNU:  cond ? then
-record(bic_ifexpr,
	{
	  line :: integer(),
	  test,
	  then,
	  else
	 }).

-record(bic_assign,
	{
	  line :: integer(),
	  op,
	  lhs,
	  rhs
	 }).

%% Function declaration
-record(bic_function,
	{
	  line :: integer(),       %% line number
	  name,
	  storage,    %% list of specifiers & return type
	  type,       %% {fn,..} function argument type with formal params
	  params,     %% list of (old style) parameters [#DECL]
	  body        %% function body
	 }).

%% variable & element declarations
-record(bic_decl,
	{
	  line :: integer(),    %% line number
	  name,    %% optional identifier
	  type=[], %% type (specifier list)
	  size,    %% optional constant bit field size
	  value    %% init value assignment-expr | [assignment-expr]
	}).

%% Specialize declaration - simplify processing a bit
-record(bic_typedef,
	{
	  line :: integer(),    %% line number
	  name,    %% name of type defined
	  type=[], %% type spec
	  size,    %% optional constant bit field size
	  value    %%  init value (error if present)
	}).

-record(bic_struct,
	{
	  line :: integer(),
	  name,
	  elems
	 }).

-record(bic_union,
	{
	  line :: integer(),
	  name,
	  elems
	 }).

-record(bic_enum,
	{
	  line :: integer(),
	  name,   %% string() | undefined
	  elems   %% [{id,value|undefined}]
	 }).

-record(bic_for,
	{
	  line :: integer(),    %% line number
	  init,
	  test,
	  update,
	  body 
	 }).

-record(bic_while,
	{
	  line :: integer(),    %% line number
	  test,
	  body 
	 }).

-record(bic_do,
	{
	  line :: integer(),    %% line number
	  body,
	  test
	 }).

-record(bic_if,
	  {
	  line :: integer(),
	  test,
	  then,
	  else
	  }).

-record(bic_switch,
	{
	  line :: integer(),
	  expr,
	  body 
	 }).

-record(bic_case,
	{
	  line :: integer(),
	  expr,
	  code
	 }).

-record(bic_default,
	{
	  line :: integer(),
	  code
	 }).

-record(bic_label,
	{
	  line :: integer(),
	  name,
	  code
	 }).

	
-record(bic_goto,
	{
	  line :: integer(),
	  label
	 }).

-record(bic_continue,
	{
	  line :: integer()
	 }).

-record(bic_break,
	{
	  line :: integer()
	 }).
	
-record(bic_return,
	{
	  line :: integer(),
	  expr
	 }).

-record(bic_empty,
	{
	  line :: integer()
	}).

	  
