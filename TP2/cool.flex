/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
  if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
    YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */

%}

/*
 * Define names for regular expressions here.
 */




WHITESPACE      [ \f\r\t\v]+


CLASS           [Cc][Ll][Aa][Ss][Ss]
ELSE            [Ee][Ll][Ss][Ee]
FI              [Ff][Ii]
IF              [Ii][Ff]
IN              [Ii][Nn]
INHERITS        [Ii][Nn][Hh][Ee][Rr][Ii][Tt][Ss]
LET             [Ll][Ee][Tt]
LOOP            [Ll][Oo][Oo][Pp]
POOL            [Pp][Oo][Oo][Ll]
THEN            [Tt][Hh][Ee][Nn]
WHILE           [Ww][Hh][Ii][Ll][Ee]
ASSIGN          <-
CASE            [Cc][Aa][Ss][Ee]
ESAC            [Ee][Ss][Aa][Cc]
OF              [Oo][Ff]
DARROW          =>
NEW             [Nn][Ee][Ww]

TRUE            t[Rr][Uu][Ee]
FALSE           f[Aa][Ll][Ss][Ee]
NUMBER          [0-9]+
TYPEID          [A-Z][a-zA-Z0-9_]*
OBJECTID        [a-z][a-zA-Z0-9_]*
LEQ             <=
NOT             [Nn][Oo][Tt]
ISVOID          [Ii][Ss][Vv][Oo][Ii][Dd]





%%
<<EOF>>       { return (0); }
{WHITESPACE}
 
 /*
  *  Nested comments
  */

--[^\n]*
\n    { curr_lineno++; }
"(*"  {
      register int c;
      
      for ( ; ; ){
        while ( (c = yyinput()) != '*' && c != '\n'
                        && c != EOF ); /* eat up text of comment */
        if ( c == '*' ) {
          while ( (c = yyinput()) == '*' );
          if ( c == ')' )  break; /* found the end */
        }
        if (c == '\n') curr_lineno++;
        if ( c == EOF ){
                cool_yylval.error_msg = (char*) "EOF in comment";
                return (ERROR);
          }
      }
    }

"*)"    { cool_yylval.error_msg = (char*) "Unmatched *)"; return(ERROR); }

"+"             { return '+'; }
"/"             { return '/'; }
"-"             { return '-'; }
"*"             { return '*'; }
"="             { return '='; }
"<"             { return '<'; }
"."             { return '.'; }
"~"             { return '~'; }
","             { return ','; }
";"             { return ';'; }
":"             { return ':'; }
"("             { return '('; }
")"             { return ')'; }
"@"             { return '@'; }
"{"             { return '{'; }
"}"             { return '}'; }

 /*
  *  The multiple-character operators.
  */
 
{LEQ}       { return (LE); }
{DARROW}    { return (DARROW); }
{ASSIGN}    { return (ASSIGN); }
 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */

{CLASS}     { return (CLASS); }
{ELSE}      { return (ELSE); }
{FI}        { return (FI); }
{IF}        { return (IF); }
{IN}        { return (IN); }
{INHERITS}  { return (INHERITS); }
{LET}       { return (LET); }
{LOOP}      { return (LOOP); }
{POOL}      { return (POOL); }
{THEN}      { return (THEN); }
{WHILE}     { return (WHILE); }

{CASE}      { return (CASE); }
{ESAC}      { return (ESAC); }
{OF}        { return (OF); }
{NEW}       { return (NEW); }
{NOT}       { return (NOT); }
{ISVOID}    { return (ISVOID); }



{NUMBER}        { yylval.symbol = inttable.add_string(yytext,yyleng);
                        return (INT_CONST); }
{TRUE}          { yylval.boolean = true; return (BOOL_CONST);} 
{FALSE}         { yylval.boolean = false; return (BOOL_CONST); }

{TYPEID}        { yylval.symbol = idtable.add_string(yytext,yyleng);
                        return (TYPEID); }
{OBJECTID}      { yylval.symbol = idtable.add_string(yytext,yyleng);
                        return (OBJECTID); }

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */
 
"\""  {
        register int c;
        register int size = 0;
        bool newline;
        for ( ; ; ){
          newline = false;
          c = yyinput();
          
          if (c == '"') {
            string_buf[size] = '\0';
            cool_yylval.symbol = stringtable.add_string(string_buf,size);
            return (STR_CONST);
          }
          else if (c == '\n'){
            newline = true;
            curr_lineno++;
            cool_yylval.error_msg = (char*) "Unterminated string constant";
          }
          else if (c == '\\'){
            c = yyinput();
            switch (c) {
              case 'b': c = '\b'; break;
              case 't': c = '\t'; break;
              case 'n': c = '\n'; break;
              case 'f': c = '\f'; break;
              case '\n':
                        newline = true;
                        curr_lineno++;
                        break;
              default: break;
            }
          }
          
          if(c == EOF){
            cool_yylval.error_msg = (char*) "EOF in string constant";
            return (ERROR);
          }
          
          if (c == '\0'){
            cool_yylval.error_msg = (char*) "String contains null character";
            while ( (c = yyinput()) != '"' && c != '\n' && c != EOF ){
              if (c == '\\') c = yyinput();
            }
            if (c == '\n') curr_lineno++;
            return (ERROR);
          }
          if (!newline){
            if (size < MAX_STR_CONST){
              string_buf[size++] = c;
            }
            else{
              cool_yylval.error_msg = (char*) "String constant too long";
              while ( (c = yyinput()) != '"' && c != '\n' && c != EOF ){
                if (c == '\\') c = yyinput();
              }
              if (c == '\n') curr_lineno++;
              return (ERROR);
            }
          }
        }
      }
 

.       { cool_yylval.error_msg = yytext; return (ERROR); }

%%
