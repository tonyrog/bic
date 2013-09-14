%% -*- erlang -*-
%% Ansi C scanner
%%

Definitions.

B	= [0-1]
D	= [0-9]
L	= [a-zA-Z_]
H	= [a-fA-F0-9]
E	= [Ee][+-]?{D}+
FS	= (f|F|l|L)
IS	= (u|U|l|L)*
WS      = [\000-\s]

Rules.

auto		: {token,{auto,TokenLine}}.
break		: {token,{break,TokenLine}}.
case		: {token,{'case',TokenLine}}.
char		: {token,{char,TokenLine}}.
const		: {token,{const,TokenLine}}.
continue	: {token,{continue,TokenLine}}.
default	        : {token,{default,TokenLine}}.
do		: {token,{do,TokenLine}}.
double		: {token,{double,TokenLine}}.
else		: {token,{else,TokenLine}}.
enum		: {token,{enum,TokenLine}}.
extern		: {token,{extern,TokenLine}}.
float		: {token,{float,TokenLine}}.
for		: {token,{for,TokenLine}}.
goto		: {token,{goto,TokenLine}}.
if		: {token,{'if',TokenLine}}.
int		: {token,{int,TokenLine}}.
long		: {token,{long,TokenLine}}.
register	: {token,{register,TokenLine}}.
return	        : {token,{return,TokenLine}}.
short		: {token,{short,TokenLine}}.
signed		: {token,{signed,TokenLine}}.
sizeof		: {token,{sizeof,TokenLine}}.
static		: {token,{static,TokenLine}}.
struct		: {token,{struct,TokenLine}}.
switch		: {token,{switch,TokenLine}}.
typedef		: {token,{typedef,TokenLine}}.
union		: {token,{union,TokenLine}}.
unsigned	: {token,{unsigned,TokenLine}}.
void		: {token,{void,TokenLine}}.
volatile	: {token,{volatile,TokenLine}}.
while		: {token,{while,TokenLine}}.

"(\^.|\.|[^\"])*" :	S = lists:sublist(TokenChars,2,TokenLen-2),
		  	{token,{string, TokenLine, S}}.

{L}({L}|{D})*	    :	case get({bic_type,TokenChars}) of
			     undefined -> {token,{identifier,TokenLine,TokenChars}};
			     _Type -> {token,{type,TokenLine,TokenChars}}
			end.

0[xX]{H}+{IS}?      : {token,{hexnum,TokenLine,TokenChars}}.
0[b]{B}+{IS}?       : {token,{binnum,TokenLine,TokenChars}}.
0{D}+{IS}?	    : {token,{octnum,TokenLine,TokenChars}}.
{D}+{IS}?	    : {token,{decnum,TokenLine,TokenChars}}.

'(.|[^\'])+'	    : {token,{chrnum,TokenLine,TokenChars}}.

{D}+{E}{FS}?	      : {token,{flonum,TokenLine,TokenChars}}.
{D}*\.{D}+({E})?{FS}? : {token,{flonum,TokenLine,TokenChars}}.
{D}+\.{D}*({E})?{FS}? : {token,{flonum,TokenLine,TokenChars}}.

>>=		: {token,{'>>=',TokenLine}}.
<<=		: {token,{'<<=',TokenLine}}.
\+=		: {token,{'+=',TokenLine}}.
\-=		: {token,{'-=',TokenLine}}.
\*=		: {token,{'*=',TokenLine}}.
/=		: {token,{'/=',TokenLine}}.
\%=		: {token,{'%=',TokenLine}}.
&=		: {token,{'&=',TokenLine}}.
\^=		: {token,{'^=',TokenLine}}.
\|=		: {token,{'|=',TokenLine}}.
>>		: {token,{'>>',TokenLine}}.
<<		: {token,{'<<',TokenLine}}.
\+\+		: {token,{'++',TokenLine}}.
--		: {token,{'--',TokenLine}}.
->		: {token,{'->',TokenLine}}.
&&		: {token,{'&&',TokenLine}}.
\|\|		: {token,{'||',TokenLine}}.
<=		: {token,{'<=',TokenLine}}.
>=		: {token,{'>=',TokenLine}}.
==		: {token,{'==',TokenLine}}.
!=		: {token,{'!=',TokenLine}}.
;		: {token,{';',TokenLine}}.
{		: {token,{'{',TokenLine}}.
}		: {token,{'}',TokenLine}}.
,		: {token,{',',TokenLine}}.
:		: {token,{':',TokenLine}}.
=		: {token,{'=',TokenLine}}.
\(		: {token,{'(',TokenLine}}.
\)		: {token,{')',TokenLine}}.
\[		: {token,{'[',TokenLine}}.
\]		: {token,{']',TokenLine}}.
\.\.\.		: {token,{'...',TokenLine}}.
\.		: {token,{'.',TokenLine}}.
&		: {token,{'&',TokenLine}}.
!		: {token,{'!',TokenLine}}.
~		: {token,{'~',TokenLine}}.
-		: {token,{'-',TokenLine}}.
\+		: {token,{'+',TokenLine}}.
\*		: {token,{'*',TokenLine}}.
/		: {token,{'/',TokenLine}}.
\%		: {token,{'%',TokenLine}}.
<		: {token,{'<',TokenLine}}.
>		: {token,{'>',TokenLine}}.
\^		: {token,{'^',TokenLine}}.
\|		: {token,{'|',TokenLine}}.
\?		: {token,{'?',TokenLine}}.
#		: {token,{'#',TokenLine}}.
{WS}+		: skip_token .


Erlang code.

-export([init/0, scan/1]).

init() -> 
    erase(bic_scan_chars).

%%
%% The scanner MUST use bic_scan:token to get ONE token at the time
%% otherwise the parser won't work. This is beacuse of the grammer
%% is not capable of handling type names and identifiers in a good way.
%%
scan(Fd) ->
    case get(bic_scan_chars) of
	undefined ->
	    put(bic_scan_chars,[]),
	    scan(Fd, [], []);
	Chars ->
	    scan(Fd,[],Chars)
    end.

scan(Fd, _Cont, eof) ->
    put(bic_scan_chars, eof),
    {eof, bic_cpp:line(Fd)};
scan(Fd, _Cont, {error,Reason}) ->
    put(bic_scan_chars, {error,Reason}),
    {error, Reason, bic_cpp:line(Fd)};
scan(Fd, Cont, Data) ->
    CLn = bic_cpp:line(Fd),
    Tr = token(Cont,Data,CLn),
    %% io:format("BIC_SCAN: ~w: data=~p, R=~p\n", [CLn,Data,Tr]),
    case Tr of
    	{more, Cont1} ->
	    scan(Fd, Cont1, bic_cpp:read(Fd));
	{done,{ok,{'#',_Ln},_Ln1}, _Data1} ->
	    %% skip injected lines
	    scan(Fd, [], bic_cpp:read(Fd));
	{done,{ok,Tok,Line1}, Data1} ->
	    put(bic_scan_chars, Data1),
	    {ok, [Tok], Line1};
	{done, {error,Reason}, _Data1} ->
	    put(bic_scan_chars, {error,Reason}),
	    {error, Reason, bic_cpp:line(Fd)}
    end.
