%{
#include <string>
#include "TopazAST.h"
#include "TopazParser.hpp"

extern int yylineno;
extern void yyerror(const char *s);
int cur_column = 0;
int last_token_len = 0;

std::vector<std::string> imported;
extern std::stack<std::string> cur_file;
std::stack<int> cur_line;

#define SAVE_TOKEN  yylval.string = new std::string(yytext, yyleng); last_token_len = yyleng; cur_column += yyleng
#define GET_COLUMN  cur_column += yyleng; last_token_len = yyleng
#define TOKEN(t)    (yylval.token = t)

#define YY_USER_ACTION yylloc.first_line = yylineno-1; yylloc.last_line = yylineno; yylloc.first_column = cur_column - last_token_len;

%}

%s normal
%x comment
%x import

%option noyywrap
%option yylineno

%%

"import" {BEGIN import;}
<import>[^\ \t\n]+   {
	cur_line.push(yylineno);
	yylineno = 1;
	cur_column = 0;

	// Check if this has already been imported
	if (std::find(imported.begin(), imported.end(), yytext) == imported.end()) {
		imported.push_back(yytext);
		printf("%s... Importing %s%s\n", BOLDYELLOW, yytext, RESET);
		std::string fname = "libs/" + std::string(yytext)+".tdc";
		cur_file.push(fname);
	    yyin = fopen( fname.c_str(), "r" );
	    if ( ! yyin ) yyerror(("Declarations file `" + std::string(yytext) + "` not found!").c_str());
	    yypush_buffer_state(yy_create_buffer( yyin, YY_BUF_SIZE ));
	    BEGIN(normal);
	}
	else {
		// No color here
		printf("... Ignoring %s as already imported\n", yytext);
		BEGIN(normal);
	}
}

<<EOF>> {
    yypop_buffer_state();
    if ( !YY_CURRENT_BUFFER ) yyterminate(); yylineno = cur_line.top(); cur_line.pop(); cur_column = 0; cur_file.pop();}

[ \t\f\v]                       cur_column++  ;
\n								cur_column = 0; // Reset column
"extern"                        GET_COLUMN; return TOKEN(TEXTERN);
"return"                        GET_COLUMN; return TOKEN(TRETURN);
"struct"                        GET_COLUMN; return TOKEN(TSTRUCT);

"is"                            GET_COLUMN; return TOKEN(TCEQ);
"isnt"                          GET_COLUMN; return TOKEN(TCNE);
"and"                           GET_COLUMN; return TOKEN(TAND);
"or"                            GET_COLUMN; return TOKEN(TOR);

"if"                            GET_COLUMN; return TOKEN(TIF);
"then"                          GET_COLUMN; return TOKEN(TTHEN);
"do"                            GET_COLUMN; return TOKEN(TDO);
"else"                          GET_COLUMN; return TOKEN(TELSE);
"ptr"                           GET_COLUMN; return TOKEN(TPTR);
"at"                            GET_COLUMN; return TOKEN(TAT);
"ref"                           GET_COLUMN; return TOKEN(TADDR);
"template"                      GET_COLUMN; return TOKEN(TTEMPLATE);
"sizeof"                        GET_COLUMN; return TOKEN(TSIZEOF);
"typeof"                        GET_COLUMN; return TOKEN(TTYPEOF);
"variant"                     	GET_COLUMN; return TOKEN(TPOLYMORPH);
"stringify"                     GET_COLUMN; return TOKEN(TSTRINGIFY);
"print"                     	GET_COLUMN; return TOKEN(TPRINT);
"istype"						GET_COLUMN; return TOKEN(TISTYPE);
"end"                           GET_COLUMN; return TOKEN(TEND);
"new"                           GET_COLUMN; return TOKEN(TNEW);
"as"                            GET_COLUMN; return TOKEN(TAS);
"line"							GET_COLUMN; return TOKEN(TLINE);
"exit"							GET_COLUMN; return TOKEN(TEXIT);
"for"                           GET_COLUMN; return TOKEN(TFOR);
"while"                         GET_COLUMN; return TOKEN(TWHILE);
"of"                            GET_COLUMN; return TOKEN(TOF);
"func"                          GET_COLUMN; return TOKEN(TFUNC);
"global"                        GET_COLUMN; return TOKEN(TVAR);
"type"                          GET_COLUMN; return TOKEN(TTYPE);
"class"                         GET_COLUMN; return TOKEN(TCLASS);

[a-z_][a-zA-Z0-9_]*             SAVE_TOKEN; return TIDENTIFIER;
[A-Z][a-zA-Z0-9_]*              SAVE_TOKEN; return TCLASSID;
[0-9]+\.[0-9]*                  SAVE_TOKEN; return TDOUBLE;
[0-9]+                          SAVE_TOKEN; return TINTEGER;
L?\"(\\.|[^\\"])*\"             SAVE_TOKEN; return TSTRING;
L?\'(\\.|[^\\'])*\'             SAVE_TOKEN; return TCHAR;

"="                             GET_COLUMN; return TOKEN(TEQUAL);
"<"                             GET_COLUMN; return TOKEN(TCLT);
"<="                            GET_COLUMN; return TOKEN(TCLE);
">"                             GET_COLUMN; return TOKEN(TCGT);
">="                            GET_COLUMN; return TOKEN(TCGE);

"++"                            GET_COLUMN; return TOKEN(TINC);
"--"                            GET_COLUMN; return TOKEN(TDEC);

"("                             GET_COLUMN; return TOKEN(TLPAREN);
")"                             GET_COLUMN; return TOKEN(TRPAREN);
"{"                             GET_COLUMN; return TOKEN(TLBRACE);
"}"                             GET_COLUMN; return TOKEN(TRBRACE);
"["                             GET_COLUMN; return TOKEN(TLBRACKET);
"]"                             GET_COLUMN; return TOKEN(TRBRACKET);
"|"                             GET_COLUMN; return TOKEN(TBAR);

"."                             GET_COLUMN; return TOKEN(TDOT);
","                             GET_COLUMN; return TOKEN(TCOMMA);

"+"                             GET_COLUMN; return TOKEN(TPLUS);
"-"                             GET_COLUMN; return TOKEN(TMINUS);
"*"                             GET_COLUMN; return TOKEN(TMUL);
"%"                             GET_COLUMN; return TOKEN(TMOD);
"&"                             GET_COLUMN; return TOKEN(TDEREF);
"==>"                           GET_COLUMN; return TOKEN(TREFARROW);
";"                             GET_COLUMN; return TOKEN(TSEMICOLON);
":"                             GET_COLUMN; return TOKEN(TCOLON);
"::"                            GET_COLUMN; return TOKEN(TDOUBLECOLON);
"->"                            GET_COLUMN; return TOKEN(TRARROW);
"<=="                           GET_COLUMN; return TOKEN(TLARROW);
"/"                             GET_COLUMN; return TOKEN(TDIV);

\/\/(.*) 						// Single and multiline comments
"/*"([^*]|\*+[^*/])*\*+"/"		
.                               yyerror(("Unknown token '" + std::string(yytext) + "'").c_str()); yyterminate();

%%
