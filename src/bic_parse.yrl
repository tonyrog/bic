%% -*- erlang -*-
%% ANSI C parser 
%%
Nonterminals
	primary_expr postfix_expr argument_expr_list
	unary_expr unary_operator cast_expr
	multiplicative_expr additive_expr shift_expr
	relational_expr equality_expr and_expr exclusive_or_expr
	inclusive_or_expr logical_and_expr logical_or_expr
	conditional_expr assignment_expr assignment_operator
	expr constant constant_expr declaration declaration_specifiers
	init_declarator_list init_declarator
	storage_class_specifier type_specifier type_name
	struct_or_union_specifier struct_or_union
	struct_declaration_list struct_declaration
	struct_declarator_list struct_declarator
	enum_specifier enumerator_list enumerator
	declarator declarator2 pointer type_specifier_list
	parameter_identifier_list identifier_list
	parameter_type_list parameter_list
	parameter_declaration abstract_declarator
	abstract_declarator2 initializer
	initializer_list statement labeled_statement
	compound_statement declaration_list
	statement_list expression_statement 
	selection_statement iteration_statement jump_statement 
	file external_definition function_definition
	function_body 
	.

Terminals 
	  hexnum octnum binnum decnum flonum chrnum
	  identifier string sizeof type
	  '->' '++' '--' '<<' '>>' '<' '>' '>=' '<=' '==' '!='
	  '&&' '||' '*=' '/=' '%=' '+='
	  '-=' '<<=' '>>=' '&=' '^=' '|=' 
	  '(' ')' '[' ']' '{' '}' ',' '.' '&' '*' '+' '-' '~' '!'
	  '/' '%' '^' '|' ':' '?' '=' ';'
	  'typedef' 'extern' 'static' 'auto' 'register'
	  'char' 'short' 'int' 'long' 'signed' 'unsigned' 'float' 'double' 
	  'const' 'volatile' 'void'
	  'struct' 'union' 'enum' '...'
	  'case' 'default' 'if' 'else' 'switch' 'while' 'do' 
	  'for' 'goto' 'continue' 'break' 'return'
	  .

Rootsymbol file.

%%

primary_expr -> identifier : id('$1').
primary_expr -> constant : '$1'.
primary_expr -> string : str('$1').
primary_expr -> '(' expr ')' : '$2'.

postfix_expr -> primary_expr : '$1'.
postfix_expr -> postfix_expr '[' expr ']' : 
		    #bic_binary {line=line('$1'),op='[]',arg1='$1',arg2='$3'}.
postfix_expr -> postfix_expr '(' ')' : 
		    #bic_call {line=line('$1'),func='$1',args=[]}.
postfix_expr -> postfix_expr '(' argument_expr_list ')' :
		    #bic_call {line=line('$1'),func='$1',args='$3'}.
postfix_expr -> postfix_expr '.' identifier : 
		    #bic_binary {line=line('$1'),op='.',arg1='$1',arg2=id('$3') }.
postfix_expr -> postfix_expr '->' identifier : 
		    #bic_binary {line=line('$1'),op='->',arg1='$1',arg2=id('$3')}.
postfix_expr -> postfix_expr '++' : 
		    #bic_unary {line=line('$1'),op='+++',arg='$1'}.
postfix_expr -> postfix_expr '--' : 
		    #bic_unary {line=line('$1'),op='---',arg='$1'}.

argument_expr_list -> assignment_expr : ['$1'].
argument_expr_list -> argument_expr_list ',' assignment_expr : '$1'++['$3'].

unary_expr -> postfix_expr : '$1'.
unary_expr -> '++' unary_expr :
		  #bic_unary {line=line('$1'),op='++',arg='$2'}.
unary_expr -> '--' unary_expr : 
		  #bic_unary {line=line('$1'),op='--',arg='$2'}.
unary_expr -> unary_operator cast_expr : 
		  #bic_unary {line=line('$1'),op=op('$1'),arg='$2'}.
unary_expr -> 'sizeof' unary_expr : 
		  #bic_unary {line=line('$1'),op='sizeof',arg='$2'}.
unary_expr -> 'sizeof' '(' type_name ')' : 
		  #bic_unary {line=line('$1'),op='sizeof',arg='$3'}.

unary_operator -> '&' : '$1'.
unary_operator -> '*' : '$1'.
unary_operator -> '+' : '$1'.
unary_operator -> '-' : '$1'.
unary_operator -> '~' : '$1'.
unary_operator -> '!' : '$1'.

cast_expr -> unary_expr : '$1'.
cast_expr -> '(' type_name ')' cast_expr :
		 #bic_binary {line=line('$1'),op=cast,arg1='$2',arg2='$4'}.

multiplicative_expr -> cast_expr : '$1'.
multiplicative_expr -> multiplicative_expr '*' cast_expr : 
		   #bic_binary { line=line('$2'), op='*',arg1='$1',arg2='$3'}.
multiplicative_expr -> multiplicative_expr '/' cast_expr :
		   #bic_binary { line=line('$2'), op='/',arg1='$1',arg2='$3'}.
multiplicative_expr -> multiplicative_expr '%' cast_expr :
		   #bic_binary { line=line('$2'), op='%',arg1='$1',arg2='$3'}.

additive_expr -> multiplicative_expr : '$1'.
additive_expr -> additive_expr '+' multiplicative_expr :
		   #bic_binary { line=line('$2'), op='+',arg1='$1',arg2='$3'}.
additive_expr -> additive_expr '-' multiplicative_expr :
		   #bic_binary { line=line('$2'), op='-',arg1='$1',arg2='$3'}.

shift_expr -> additive_expr : '$1'.
shift_expr -> shift_expr '<<' additive_expr :
		   #bic_binary { line=line('$2'),op='<<',arg1='$1',arg2='$3'}.
shift_expr -> shift_expr '>>' additive_expr :
		  #bic_binary { line=line('$2'),op='>>',arg1='$1',arg2='$3'}.

relational_expr -> shift_expr : '$1'.
relational_expr -> relational_expr '<' shift_expr :
		  #bic_binary { line=line('$2'),op='<',arg1='$1',arg2='$3'}.
relational_expr -> relational_expr '>' shift_expr :
		  #bic_binary { line=line('$2'),op='>',arg1='$1',arg2='$3'}.
relational_expr -> relational_expr '<=' shift_expr :
		  #bic_binary { line=line('$2'),op='<=',arg1='$1',arg2='$3'}.
relational_expr -> relational_expr '>=' shift_expr :
		  #bic_binary { line=line('$2'),op='>=',arg1='$1',arg2='$3'}.

equality_expr -> relational_expr : '$1'.
equality_expr -> equality_expr '==' relational_expr :
		  #bic_binary { line=line('$2'),op='==',arg1='$1',arg2='$3'}.
equality_expr -> equality_expr '!=' relational_expr :
		  #bic_binary { line=line('$2'),op='!=',arg1='$1',arg2='$3'}.

and_expr -> equality_expr : '$1'.
and_expr -> and_expr '&' equality_expr :
		  #bic_binary { line=line('$2'),op='&',arg1='$1',arg2='$3'}.

exclusive_or_expr -> and_expr : '$1'.
exclusive_or_expr -> exclusive_or_expr '^' and_expr :
		  #bic_binary { line=line('$2'),op='^',arg1='$1',arg2='$3'}.

inclusive_or_expr -> exclusive_or_expr : '$1'.
inclusive_or_expr -> inclusive_or_expr '|' exclusive_or_expr :
		  #bic_binary { line=line('$2'),op='|',arg1='$1',arg2='$3'}.

logical_and_expr -> inclusive_or_expr : '$1'.
logical_and_expr -> logical_and_expr '&&' inclusive_or_expr :
		  #bic_binary { line=line('$2'),op='&&',arg1='$1',arg2='$3'}.

logical_or_expr -> logical_and_expr : '$1'.
logical_or_expr -> logical_or_expr '||' logical_and_expr :
		  #bic_binary { line=line('$2'),op='||',arg1='$1',arg2='$3'}.

conditional_expr -> logical_or_expr : '$1'.
conditional_expr -> logical_or_expr '?' logical_or_expr ':' conditional_expr :
		  #bic_ifexpr { line=line('$2'),test='$1',then='$3',else='$5'}.

assignment_expr -> conditional_expr : '$1'.
assignment_expr -> unary_expr assignment_operator assignment_expr :
	          #bic_assign {line=line('$2'),op=op('$2'),lhs='$1',rhs='$3'}.

assignment_operator -> '=' : '$1'.
assignment_operator -> '*=' : '$1'.
assignment_operator -> '/=' : '$1'.
assignment_operator -> '%=' : '$1'.
assignment_operator -> '+=' : '$1'.
assignment_operator -> '-=' : '$1'.
assignment_operator -> '<<=' : '$1'.
assignment_operator -> '>>=' : '$1'.
assignment_operator -> '&=' : '$1'.
assignment_operator -> '^=' : '$1'.
assignment_operator -> '|=' : '$1'.

expr -> assignment_expr : '$1'.
expr -> expr ',' assignment_expr : 
     	#bic_binary { line=line('$1'), op=',',arg1='$1',arg2='$3'}.

constant_expr -> conditional_expr : '$1'.

constant -> hexnum : hex('$1').
constant -> octnum : oct('$1').
constant -> decnum : dec('$1').
constant -> binnum : bin('$1').
constant -> flonum : flo('$1').
constant -> chrnum : chr('$1').

declaration -> declaration_specifiers ';' : 
		   {Storage,TypeSpec} = '$1',
	       [#bic_decl { line=line('$2'), storage=Storage, type=TypeSpec}].
declaration -> declaration_specifiers init_declarator_list ';' :
		   {Storage,TypeSpec} = '$1',
	       map(fun(D) when is_record(D,bic_decl) ->
			   D#bic_decl { storage=Storage,
					type=bic:combine_types(TypeSpec,D#bic_decl.type)};
		      (T) when is_record(T,bic_typedef) ->
			   Type = bic:combine_types(TypeSpec,T#bic_typedef.type),
			   T#bic_typedef { storage=Storage, type=Type }
		   end, '$2').

declaration_specifiers -> storage_class_specifier : 
			      {'$1', undefined}.
declaration_specifiers -> storage_class_specifier declaration_specifiers : 
			      {_,Type} = '$2', {'$1',Type}.
declaration_specifiers -> type_specifier : 
			      {undefined, '$1'}.
declaration_specifiers -> type_specifier declaration_specifiers : 
			      {Storage,Type} = '$2',
			  {Storage,bic:combine_types('$1',Type)}.

init_declarator_list -> init_declarator : 
		['$1'].
init_declarator_list -> init_declarator_list ',' init_declarator : 
		'$1'++['$3'].

%% Extension - allow bit size construct for normal variables

init_declarator -> declarator : 
		decl('$1').
init_declarator -> declarator ':' constant_expr : 
		decl('$1'#bic_decl { size ='$3'}).
init_declarator -> declarator '=' initializer : 
		decl('$1'#bic_decl { value = '$3'}).
init_declarator -> declarator ':' constant_expr '=' initializer : 
		decl('$1'#bic_decl { size = '$3', value = '$5'}).

storage_class_specifier -> 'extern'   : op('$1').
storage_class_specifier -> 'static'   : op('$1').
storage_class_specifier -> 'auto'     : op('$1').
storage_class_specifier -> 'register' : op('$1'). 
storage_class_specifier -> 'typedef'  : put(bic_is_typedef, true), undefined.

type_specifier -> 'void'     : #bic_type{line=line('$1'),type=void}.
type_specifier -> 'char'     : #bic_type{line=line('$1'),type=char}.
type_specifier -> 'int'      : #bic_type{line=line('$1'),type=int}.
type_specifier -> 'float'    : #bic_type{line=line('$1'),type=float}.
type_specifier -> 'double'   : #bic_type{line=line('$1'),type=double}.
type_specifier -> 'short'    : #bic_type{line=line('$1'),size=short}.
type_specifier -> 'long'     : #bic_type{line=line('$1'),size=long}.
type_specifier -> 'signed'   : #bic_type{line=line('$1'),sign=signed}.
type_specifier -> 'unsigned' : #bic_type{line=line('$1'),sign=unsigned}.
type_specifier -> 'const'    : #bic_type{line=line('$1'),const=true}.
type_specifier -> 'volatile' : #bic_type{line=line('$1'),volatile=true}.
type_specifier -> struct_or_union_specifier : '$1'.
type_specifier -> enum_specifier : '$1'.
type_specifier -> type : typeid('$1').

struct_or_union_specifier -> struct_or_union identifier '{' struct_declaration_list '}' : 
	case op('$1') of
	     struct -> #bic_struct {line=line('$1'),name=arg('$2'),elems='$4'};
	     union ->  #bic_union  {line=line('$1'),name=arg('$2'),elems='$4'}
	end.
		     
struct_or_union_specifier -> struct_or_union '{' struct_declaration_list '}' :
	case op('$1') of
	     struct -> #bic_struct {line=line('$1'),elems='$3'};
	     union ->  #bic_union  {line=line('$1'),elems='$3'}
	end.

struct_or_union_specifier -> struct_or_union identifier :
	case op('$1') of
	     struct -> #bic_struct {line=line('$1'),name=arg('$2')};
	     union ->  #bic_union  {line=line('$1'),name=arg('$2')}
	end.

struct_or_union -> 'struct' : '$1'.
struct_or_union -> 'union' : '$1'.

struct_declaration_list -> struct_declaration : '$1'.
struct_declaration_list -> struct_declaration_list struct_declaration :
			'$1'++'$2'.

struct_declaration -> type_specifier_list struct_declarator_list ';' :
			  map(fun(D) ->
				      T = bic:combine_types('$1',D#bic_decl.type),
				      D#bic_decl { type=T } 
			      end, '$2').

struct_declarator_list -> struct_declarator : ['$1'].
struct_declarator_list -> struct_declarator_list ',' struct_declarator :
		       '$1'++['$3'].

struct_declarator -> declarator : 
		   '$1'.
struct_declarator -> ':' constant_expr : 
		   #bic_decl { line=line('$1'), size ='$2'} .
struct_declarator -> declarator ':' constant_expr : 
		   '$1'#bic_decl { size ='$3'}.

enum_specifier -> 'enum' '{' enumerator_list '}' : 
	       #bic_enum {line=line('$1'),elems='$3'}.
enum_specifier -> 'enum' identifier '{' enumerator_list '}' :
	       #bic_enum {line=line('$1'),name=arg('$2'),elems='$4'}.
enum_specifier -> 'enum' identifier :
	       #bic_enum {line=line('$1'),name=arg('$2')}.

enumerator_list -> enumerator : ['$1'].
enumerator_list -> enumerator_list ',' enumerator : '$1'++['$3'].

enumerator -> identifier : { arg('$1'),undefined}.
enumerator -> identifier '=' constant_expr : {arg('$1'),line('$1'),'$3'}.

declarator -> declarator2 : '$1'.
declarator -> pointer declarator2 : 
		  '$2'#bic_decl { type=bic:combine_types('$1','$2'#bic_decl.type) }.

declarator2 -> identifier : 
		   #bic_decl { line=line('$1'), name=arg('$1') }.
declarator2 -> '(' declarator ')' : 
		   '$2'.
declarator2 -> declarator2 '[' ']' : 
		   '$1'#bic_decl { type=#bic_array{line=line('$2'),type='$1'#bic_decl.type,dim=[]}}.
declarator2 -> declarator2 '[' constant_expr ']' : 
		   '$1'#bic_decl { type=bic:combine_types('$1'#bic_decl.type,
							  #bic_array{line=line('$2'),dim='$3'}) }.
declarator2 -> declarator2 '(' ')' : 
		   '$1'#bic_decl { type=bic:combine_types('$1'#bic_decl.type,
							  #bic_fn{line=line('$2'),params=[]})}.
declarator2 -> declarator2 '(' parameter_type_list ')' :
		   '$1'#bic_decl { type=bic:combine_types('$1'#bic_decl.type,
							  #bic_fn{line=line('$2'),params='$3'}) }.
declarator2 -> declarator2 '(' parameter_identifier_list ')' :
		   '$1'#bic_decl { type=bic:combine_types('$1'#bic_decl.type,
							  #bic_fn{line=line('$2'),params='$3'}) }.
pointer -> '*' :
	       #bic_pointer{line=line('$1')}.
pointer -> '*' type_specifier_list : 
	       #bic_pointer{line=line('$1'),type='$2'}.
pointer -> '*' pointer :
	       #bic_pointer{line=line('$1'),type='$2'}.
pointer -> '*' type_specifier_list pointer :
	       #bic_pointer{line=line('$1'),type=bic:combine_types('$2','$3')}.

type_specifier_list -> type_specifier : '$1'.
type_specifier_list -> type_specifier_list type_specifier : 
			   bic:combine_types('$1','$2').

parameter_identifier_list -> identifier_list : '$1'.
parameter_identifier_list -> identifier_list ',' '...' : '$1'++['$3'].

identifier_list -> identifier : [id('$1')].
identifier_list -> identifier_list ',' identifier : '$1'++[id('$3')].

parameter_type_list -> parameter_list : '$1'.
parameter_type_list -> parameter_list ',' '...' : '$1' ++ ['$3'].

parameter_list -> parameter_declaration : ['$1'].
parameter_list -> parameter_list ',' parameter_declaration : '$1'++['$3'].

parameter_declaration -> type_specifier_list declarator :
		'$2'#bic_decl { type=bic:combine_types('$1','$2'#bic_decl.type)}.
parameter_declaration -> type_name : 
		#bic_decl { type='$1'}. %% FIXME: line number

type_name -> type_specifier_list : '$1'.
type_name -> type_specifier_list abstract_declarator : bic:combine_types('$1','$2').

abstract_declarator -> pointer : '$1'.
abstract_declarator -> abstract_declarator2 : '$1'.
abstract_declarator -> pointer abstract_declarator2 : bic:combine_types('$1','$2').

abstract_declarator2 -> '(' abstract_declarator ')' : 
			    '$2'.
abstract_declarator2 -> '[' ']' :
			    #bic_array{line=line('$1'),dim=[]}.
abstract_declarator2 -> '[' constant_expr ']' : 
			    #bic_array{line=line('$1'),dim='$2'}.
abstract_declarator2 -> abstract_declarator2 '[' ']' : 
			    bic:combine_types('$1',#bic_array{line=line('$2'),dim=[]}).
abstract_declarator2 -> abstract_declarator2 '[' constant_expr ']' : 
			    bic:combine_types('$1',#bic_array{line=line('$2'),dim='$3'}).
abstract_declarator2 -> '(' ')' : 
			    #bic_fn{line=line('$1'),params=[]}.
abstract_declarator2 -> '(' parameter_type_list ')' : 
			    #bic_fn{line=line('$1'),params='$2'}.
abstract_declarator2 -> abstract_declarator2 '(' ')' : 
			    #bic_fn{line=line('$2'),type='$1',params=[]}.
abstract_declarator2 -> abstract_declarator2 '(' parameter_type_list ')' :
			    #bic_fn{line=line('$2'),type='$1',params='$3'}.

initializer -> assignment_expr : '$1'.
initializer -> '{' initializer_list '}' : '$2'.
initializer -> '{' initializer_list ',' '}' : '$2'.

initializer_list -> initializer : ['$1'].
initializer_list -> initializer_list ',' initializer : '$1'++['$3'].

statement -> labeled_statement : '$1'.
statement -> compound_statement : '$1'.
statement -> expression_statement : '$1'.
statement -> selection_statement : '$1'.
statement -> iteration_statement : '$1'.
statement -> jump_statement : '$1'.

labeled_statement -> identifier ':' statement : 
			 #bic_label { line=line('$1'),name=arg('$1'),code='$3'}.
labeled_statement -> 'case' constant_expr ':' statement : 
			 #bic_case { line=line('$1'),expr='$2',code='$4'}.
labeled_statement -> 'default' ':' statement : 
			 #bic_default {line=line('$1'),code='$3'}.

compound_statement -> '{' '}' : [].
compound_statement -> '{' statement_list '}' : '$2'.
compound_statement -> '{' declaration_list '}' : '$2'.
compound_statement -> '{' declaration_list statement_list '}' : '$2'++'$3'.

declaration_list -> declaration : '$1'.
declaration_list -> declaration_list declaration : '$1'++'$2'.

statement_list -> statement : ['$1'].
statement_list -> statement_list statement : '$1' ++ ['$2'].

expression_statement -> ';' : #bic_empty{line=line('$1')}.
expression_statement -> expr ';' : '$1'.

selection_statement -> 'if' '(' expr ')' statement :
	    #bic_if {line=line('$1'),test='$3',then='$5'}.
selection_statement -> 'if' '(' expr ')' statement 'else' statement :
	    #bic_if {line=line('$1'),test='$3',then='$5',else='$7'}.
selection_statement -> 'switch' '(' expr ')' statement :
	    #bic_switch { line=line('$1'),expr='$3',body='$5'}.

iteration_statement -> 'while' '(' expr ')' statement :
	    #bic_while { line=line('$1'),test='$3',body='$5'}.
iteration_statement -> 'do' statement 'while' '(' expr ')' ';' :
	    #bic_do { line=line('$1'),body='$2',test='$5'}.
iteration_statement -> 'for' '(' ';' ';' ')' statement :
	    #bic_for {line=line('$1'),body='$6'}.
iteration_statement -> 'for' '(' ';' ';' expr ')' statement :
	    #bic_for {line=line('$1'),update='$5',body='$7'}.
iteration_statement -> 'for' '(' ';' expr ';' ')' statement :
	    #bic_for {line=line('$1'),test='$4',body='$7'}.
iteration_statement -> 'for' '(' ';' expr ';' expr ')' statement :
	    #bic_for {line=line('$1'),test='$4',update='$6',body='$8'}.
iteration_statement -> 'for' '(' expr ';' ';' ')' statement :
	    #bic_for {line=line('$1'),init='$3',body='$7'}.
iteration_statement -> 'for' '(' expr ';' ';' expr ')' statement :
	    #bic_for {line=line('$1'),init='$3',update='$6',body='$8'}.
iteration_statement -> 'for' '(' expr ';' expr ';' ')' statement :
	    #bic_for {line=line('$1'),init='$3',test='$5',body='$8'}.
iteration_statement -> 'for' '(' expr ';' expr ';' expr ')' statement :
	    #bic_for {line=line('$1'),init='$3',test='$5',update='$7',body='$9'}.

jump_statement -> 'goto' identifier ';' : 
	    #bic_goto {line=line('$1'),label=arg('$2')}.
jump_statement -> 'continue' ';' : 
	    #bic_continue {line=line('$1')}.
jump_statement -> 'break' ';' : 
	    #bic_break {line=line('$1')}.
jump_statement -> 'return' ';' : 
	    #bic_return {line=line('$1')}.
jump_statement -> 'return' expr ';' : 
	   #bic_return {line=line('$1'),expr='$2'}.

file -> external_definition : '$1'.
file -> file external_definition : '$1'++'$2'.

external_definition -> function_definition : ['$1'].
external_definition -> declaration : '$1'.

function_definition -> declarator function_body : 
			   {OldDecl,Body} = '$2',
		       #bic_fn{type=undefined,params=_Params} =
			   '$1'#bic_decl.type,
		       #bic_function { line='$1'#bic_decl.line,
				       name='$1'#bic_decl.name,
				       type=#bic_type{type=int},
				       params=OldDecl,
				       body=Body }.
function_definition -> declaration_specifiers declarator function_body :
			   {OldDecl,Body} = '$3',
		       {Storage,TypeSpec} = '$1',
		       ?dbg("typespec=~s\n", 
			    [bic:format_type(TypeSpec)]),
		       ?dbg("declarator=~s\n", 
			    [bic:format_type('$2'#bic_decl.type)]),
		       #bic_fn{type=Return,params=Params} =
			   bic:combine_types(TypeSpec,'$2'#bic_decl.type),
		       ReturnType = if Return =:= undefined -> [int];
				       true -> Return
				    end,
		       FnParams = select_params(OldDecl, Params),
		       #bic_function { line='$2'#bic_decl.line,
				       name='$2'#bic_decl.name,
				       storage=Storage,
				       type=ReturnType,
				       params=FnParams,
				       body=Body }.

function_body -> compound_statement : {[],'$1'}.
function_body -> declaration_list compound_statement : {'$1','$2'}.

Erlang code.

-include("../include/bic.hrl").
-import(lists, [map/2, member/2]).
-export([init/0]).

%% -define(dbg(F,A), io:format((F),(A))).
-define(dbg(F,A), ok).

init() ->
    %% erase dictionay use
    erase(bic_is_typedef),
    lists:foreach(
      fun
	  ({{bic_type,T},_}) -> erase({bic_type,T});
	  (_) -> ok
      end, get()).



select_params([], NewParams) ->
    NewParams;
select_params(OldDecl, _NewParams) ->
    OldDecl.

%% set non undefined elements from T1 to T2

id({identifier,Line,Name}) ->
    #bic_id { line=Line, name=Name}.

typeid({type,Line,Name}) ->
    #bic_typeid { line=Line, name=Name}.

bin({binnum,Line,Val}) ->
    #bic_constant { line=Line, base=2, value=Val}.
    
oct({octnum,Line,Val}) ->
    #bic_constant { line=Line, base=8, value=Val}.

hex({hexnum,Line,Val}) ->
    #bic_constant { line=Line, base=16, value=Val}.

dec({decnum,Line,Val}) ->
    #bic_constant { line=Line, base=10, value=Val}.

chr({chrnum,Line,Val}) ->
    #bic_constant { line=Line, base=char, value=Val}.

flo({flonum,Line,Val}) ->
    #bic_constant { line=Line, base=float, value=Val}.

str({string,Line,Val}) ->
    #bic_constant { line=Line, base=string, value=Val}.

%% Handle typedef declaration (very) special
decl(D) ->
	case get(bic_is_typedef) of
	    true ->
		put({bic_type,D#bic_decl.name}, true),
		put(bic_is_typedef, false),
		#bic_decl { line=L, name=N, type=T, size=S, value=V } = D,
		#bic_typedef { line=L, name=N, type=T, value=V, size=S };
	    _ ->
		D
	end.

op({Type,_Line})     -> Type.

arg({_Type,_Line,Val}) -> Val.

line([H|_]) -> line(H);
line({_,Line}) -> Line;
line({_,Line,_}) -> Line;
line({_,Line,_,_}) -> Line.
