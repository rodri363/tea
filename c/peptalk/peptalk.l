%{
/* \file 
This file describes the tokenizer---the rules to categorize input
characters as numbers, white space, delimiters, et cetera.

The first part lists a few regular expressions for use in the second part.

The second part lists regular expressions, and what to send back if they match. 

The relevant characters, like ;, :, and -, are sent as-is to the
parser. Numbers and text are copied to the yylval variable, and an
appropriate token sent back.

There's a hack to add an extra \n at the end of the file, which makes parsing easier.

That's all. The real work happens in editlang.y, which will make use of these tokens.
*/
#include <apop.h>
#include <string.h>
#include <stdio.h>
#define YYSTYPE char*
#include "peptalk.tab.h"
extern int lineno;        /* current line number  */
extern char parsed_type;

//<<EOF>> {static int once=0; return once++ ? 0 : EOL;}
%}

digit [0-9]
space [ \t☃]
text [a-zA-Z_/~'][_a-zA-Z0-9./~']*
sqlpunct (==|<=|>=|[=+/(<|>)\'"?!%&]|\[|\])
notnewline \\[ \t\n]+

%option noinput
%option nounput
%option noyywrap

/* the "incl" state is used for picking up the name
 * of an include file
[a-z]+              ECHO;
[^a-z\n]*\n?        ECHO;

 */
%x incl
%s declarations

%{
#define MAX_INCLUDE_DEPTH 100
YY_BUFFER_STATE include_stack[MAX_INCLUDE_DEPTH];
int include_stack_ptr = 0;
%}

%%

^[ \t]*fields[ \t]*\{                    { BEGIN(declarations);}
<declarations>\}                         { BEGIN(INITIAL);}
<declarations>\\\n                       { return SPACE; }
<declarations>[ \t]int[ \t\n] { parsed_type = 'i'; return TYPE; }
<declarations>[ \t]real[ \t\n] { parsed_type = 'r'; return TYPE; }
<declarations>[ \t]cat[ \t\n] { parsed_type = 'c'; return TYPE; }
<declarations>({space}|{notnewline})+    { ; }
<declarations>#.*\n                      { return DECLARATION; }
<declarations>\n                         { return DECLARATION; }
				
include[ \t]*\:      BEGIN(incl);//stuff about including files that I cut/pasted it from the flex documentation.

<incl>[ \t]*      /* eat the whitespace */
<incl>[^ \t\n]+   { /* got the include file name */
				apop_assert_s(include_stack_ptr < MAX_INCLUDE_DEPTH, "Includes nested too deeply");
				include_stack[include_stack_ptr++] = YY_CURRENT_BUFFER;

				yyin = fopen( yytext, "r" );
				apop_assert_s(yyin, "You asked me to include %s, but I couldn't open that file.", yytext);

				yy_switch_to_buffer( yy_create_buffer( yyin, YY_BUF_SIZE ) );

				BEGIN(INITIAL);
				}

<<EOF>> {
        if ( --include_stack_ptr < 0 )
            {
            yyterminate();
            }

        else
            {
            yy_delete_buffer( YY_CURRENT_BUFFER );
            yy_switch_to_buffer(
                 include_stack[include_stack_ptr] );
            }
				return EOL;
        } //end include section.


({space}|{notnewline})+   { return SPACE; }

#.*\n      { lineno++; return EOL; }    // one-line comments 
\*          { return '*';}
,          { return ',';}
:          { return ':';}
-          { return '-'; }
\$          { return '$'; }
=>         { return THEN; } 
\n         { lineno++; return EOL; }
\|\|      { yylval = strdup(yytext); return TEXT;}

{digit}+\.?|{digit}*\.{digit}+ { yylval = strdup(yytext); 
	                                return NUMBER; }

({text}|{sqlpunct})      { yylval = strdup(yytext); 
                    return TEXT;}

\{      { return OPENKEY;}
\}      { return CLOSEKEY;}

%%
