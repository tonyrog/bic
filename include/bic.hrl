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
	  line,
	  base,  %% 8,10,16 | char | float
	  value  %% string rep of value
	 }).
	  
-record(bic_unary,
	{
	  line,
	  op,
	  arg
	 }).

-record(bic_expr,
	{
	  line,
	  op,
	  arg1,
	  arg2
	 }).

-record(bic_call,
	{
	  line,
	  func,
	  args
	 }).
	
%%  cond ? then : else FIXME? GNU:  cond ? then
-record(bic_ifexpr,
	{
	  line,
	  test,
	  then,
	  else
	 }).

-record(bic_assign,
	{
	  line,
	  op,
	  lhs,
	  rhs
	 }).

%% Function declaration
-record(bic_function,
	{
	  line,       %% line number
	  name,
	  storage,    %% list of specifiers & return type
	  type,       %% {fn,..} function argument type with formal params
	  params,     %% list of (old style) parameters [#DECL]
	  body        %% function body
	 }).

%% variable & element declarations
-record(bic_decl,
	{
	  line,    %% line number
	  name,    %% optional identifier
	  type=[], %% type (specifier list)
	  size,    %% optional constant bit field size
	  value    %%  init value
	}).

%% Specialize declaration - simplify processing a bit
-record(bic_typedef,
	{
	  line,    %% line number
	  name,    %% name of type defined
	  type=[], %% type spec
	  size,    %% optional constant bit field size
	  value    %%  init value (error if present)
	}).

-record(bic_struct,
	{
	  line,
	  name,
	  elems
	 }).

-record(bic_union,
	{
	  line,
	  name,
	  elems
	 }).

-record(bic_enum,
	{
	  line,
	  name,   %% string() | undefined
	  elems   %% [{id,value|undefined}]
	 }).



-record(bic_for,
	{
	  line,    %% line number
	  init,
	  test,
	  update,
	  body 
	 }).

-record(bic_while,
	{
	  line,    %% line number
	  test,
	  body 
	 }).

-record(bic_do,
	{
	  line,    %% line number
	  test,
	  body 
	 }).

-record(bic_if,
	  {
	  line,
	  test,
	  then,
	  else
	  }).

-record(bic_switch,
	{
	  line,
	  expr,
	  body 
	 }).

-record(bic_case,
	{
	  line,
	  expr,
	  code
	 }).

-record(bic_default,
	{
	  line,
	  code
	 }).

-record(bic_label,
	{
	  line,
	  name,
	  code
	 }).

	
-record(bic_goto,
	{
	  line,
	  label
	 }).

-record(bic_continue,
	{
	  line
	 }).

-record(bic_break,
	{
	  line
	 }).
	
-record(bic_return,
	{
	  line,
	  expr
	 }).

-record(bic_empty,
	{
	  line
	}).

	  
