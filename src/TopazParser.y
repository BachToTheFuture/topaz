%{
    #include "TopazAST.h"
    #include <cstdio>
    #include <cstdlib>
    #include <fstream>

    #define YYERROR_VERBOSE 1
    #define GET_DEBUG_INFO(x) x.first_line, x.first_column, x.last_line, x.last_column, cur_file.top()
    #define END_COLUMN yylloc.last_column = cur_column - last_token_len
    
    NBlock *programBlock; /* the top level root node of our final AST */

    extern int cur_column;
    extern int last_token_len;
    extern int yylex();
    extern char* yytext;
    extern int yylineno;
    extern std::stack<std::string> cur_file;
    
    // Keep track of current line and columns
    int curline = 1;
    int start_column = 0;
    int end_column = 0;

    void yyerror(const char *s) {
        std::ifstream f(cur_file.top());
        std::string end_line_str;
        std::string buf;
        f.seekg(std::ios::beg);
        // Get the end line
        for(int i = 0; i < yylineno; ++i) {
            getline(f, end_line_str);
        }
        // Begin the end line with formatting only if end column isnt 0
        if (cur_column+1 > end_line_str.length())
            end_line_str += RESET;
        else if (cur_column > 0)
            end_line_str.insert(cur_column+1, RESET);
        if (cur_column-last_token_len > 1 && cur_column < end_line_str.length())
            end_line_str.insert(cur_column-last_token_len, BOLDBLUE);

        // Print the full formatted error message!!!
        printf("\n\n%sIn file %s, line %d:%s\n", BOLD, cur_file.top().c_str(), yylineno, RESET);
        printf("%d | %s\n", yylineno, end_line_str.c_str());
        printf("%s[SyntaxError] %s%s\n", BOLDRED, s, RESET);
        f.close();
        exit(1);
    }

    std::string unescape(const std::string& s)
    {
    std::string res;
    std::string::const_iterator it = s.begin();
    while (it != s.end())
    {
        char c = *it++;
        if (c == '\\' && it != s.end())
        {
        switch (*it++) {
        case '\\': c = '\\'; break;
        case 'n': c = '\n'; break;
        case 't': c = '\t'; break;
        case '0': c = '\0'; break;
        // all other escapes
        default: 
            // invalid escape sequence - skip it. alternatively you can copy it as is, throw an exception...
            continue;
        }
        }
        res += c;
    }

    return res;
    }

    std::stack<std::string> current_class;
    StatementList *prototypes;
    StatementList *class_stmts;
%}

/* Represents the many different ways we can access our data */
%union {
    Node *node;
    NBlock *block;
    NExpression *expr;
    NStatement *stmt;
    NIdentifier *ident;
    NVariableDeclaration *var_decl;
    std::vector<int> *indices;
    NType *signature;
    std::vector<NType*> *type_collection;
    std::vector<NVariableDeclaration*> *varvec;
    std::vector<NIdentifier*> *idvec;
    std::vector<NExpression*> *exprvec;
    std::vector<NStatement*> *stmtvec;
    std::string *string;
    int token;
}

/* Define our terminal symbols (tokens). This should
   match our tokens.l lex file. We also define the node type
   they represent.
 */
%token <string> TIDENTIFIER TCLASSID TINTEGER TDOUBLE TSTRING TCHAR
%token <token> TCEQ TCNE TCLT TCLE TCGT TCGE TEQUAL TAND TOR TAS  TPRINT
%token <token> TLPAREN TRPAREN TLBRACE TRBRACE TLBRACKET TRBRACKET TSTRINGIFY
%token <token> TCOMMA TDOT TEND TSEMICOLON TBAR TREFARROW TDOUBLECOLON
%token <token> TPLUS TMINUS TMUL TMOD TDIV TDEREF TPTR TAT TADDR TTEMPLATE
%token <token> TRETURN TEXTERN TSTRUCT TNEW TCLASS TPOLYMORPH TDEFAULT
%token <token> TIF TELSE TDO TTHEN TFOR TOF TINC TDEC TWHILE TLINE TEXIT
%token <token> TCOLON TRARROW TLARROW TFUNC TVAR TTYPE TISTYPE TSIZEOF TTYPEOF

/* Define the type of node our nonterminal symbols represent.
   The types refer to the %union declaration above. Ex: when
   we call an ident (defined by union type ident) we are really
   calling an (NIdentifier*). It makes the compiler happy.
 */
%type <ident> ident
%type <expr> primitives expr pointer_exprs function_pointer_exprs
%type <varvec> type_block
%type <idvec> func_args
%type <exprvec> call_args
%type <signature> signature
%type <type_collection> type_collection
%type <indices> indices
%type <block> program stmts block class_block
%type <stmt> stmt var_decl func_decl class_decl if_stmt for_stmt
%type <stmt> assign_stmt prototype_decl
%type <token> comparison

/* Operator precedence for mathematical operators */
%left TREFARROW TEQUAL
%left TAND TOR
%left TCLT TCLE TCGT TCGE TCEQ TCNE
%left TMOD
%left TPLUS TMINUS
%left TMUL TDIV
%left TAT TAS
%left TDOT TSIZEOF

%locations
%start program

%%

program : stmts { programBlock = $1; }
        ;
        
stmts : stmt { $$ = new NBlock(); $$->statements.push_back($<stmt>1); }
      | stmts stmt { $1->statements.push_back($<stmt>2); }
      ;

stmt : var_decl
     | func_decl
     | prototype_decl
     | class_decl 
     | if_stmt 
     | for_stmt
     | assign_stmt
     | expr { $$ = new NExpressionStatement(*$1); }
     | TRETURN expr { $$ = new NReturnStatement(*$2); }
     | stmt TSEMICOLON { $$ = $1; }
     ;

block : stmts TEND { $$ = $1; }
      | TEND { $$ = new NBlock(); }
      ;

for_stmt : TFOR var_decl TCOMMA expr TDO block
            { $$ = new NForStatement($2, *$4, *$6); }
         | TFOR var_decl TCOMMA expr TCOMMA stmt TDO block
            { $$ = new NForStatement($2, *$4, $6, *$8); }
         | TWHILE expr TDO block
            { $$ = new NForStatement(*$2, *$4); }
         ;

if_stmt : TIF expr TTHEN block { $$ = new NIfStatement(*$2, *$4); }
        | TIF expr TTHEN block TELSE block { $$ = new NIfStatement(*$2, *$4, $6); }
        // This allows else if statements
        | TIF expr TTHEN block TELSE stmt {
            auto bb = new NBlock();
            bb->statements.push_back($<stmt>6);
            $$ = new NIfStatement(*$2, *$4, bb);
        }
        ;

 // Prototypes and new ways to declare types
signature   : ident { END_COLUMN; $$ = new NBasicType(GET_DEBUG_INFO(@1), $1->name); }
            | ident TCLT type_collection TCGT {END_COLUMN; $$ = new NTemplateType(GET_DEBUG_INFO(@1), *$1, *$3, false);}
            | signature TPTR { $$ = new NPointerType(*$1); }
            | TTYPEOF TLPAREN expr TRPAREN { $$ = new NTypeofExpression($3); }
            | TTYPEOF TCOLON signature { $$ = new NTypeofExpression($3); }
            //| signature TLBRACKET indices TRBRACKET { $$ = new NArrayType(*$1, *$3); }
            | TLPAREN type_collection TRPAREN TRARROW signature { $$ = new NFunctionType(*$2, *$5); }
            | type_block TEND { $$ = new NClassType(*$1, *prototypes); prototypes = NULL; }
            ;
    
type_collection : signature { $$ = new TypeList(); $$->push_back($<signature>1); }
                | type_collection TCOMMA signature { $1->push_back($<signature>3); }
                | /* empty */ { $$ = new TypeList(); }
                ;

 // A type block is a collection of variable and prototype declarations.
type_block      : ident TCOLON signature {
                    $$ = new VariableList();
                    prototypes = new StatementList();
                    $$->push_back(new NVariableDeclaration(*$1, *$3)); 
                }
                | type_block ident TCOLON signature { $1->push_back(new NVariableDeclaration(*$2, *$4)); }
                | type_block TFUNC ident TCOLON signature { prototypes->push_back(new NFunctionPrototype(*$3, *$5, false)); }
                | type_block TPOLYMORPH ident TCOLON signature { prototypes->push_back(new NFunctionPrototype(*$3, *$5, true)); }
                | TPOLYMORPH ident TCLT type_collection TCGT {$$ = new VariableList(); prototypes = new StatementList(); END_COLUMN; prototypes->push_back(new NPredeclareVariant(*new NTemplateType(GET_DEBUG_INFO(@1), *$2, *$4, true)));}
                | type_block TPOLYMORPH ident TCLT type_collection TCGT {END_COLUMN; prototypes->push_back(new NPredeclareVariant(*new NTemplateType(GET_DEBUG_INFO(@2), *$3, *$5, true)));}
                ;

prototype_decl : TFUNC ident TCOLON signature { $$ = new NFunctionPrototype(*$2, *$4, false); }
               | TCLASS ident TCOLON signature { $$ = new NClassPrototype(*$2, *$4); }
               | TTYPE ident TCOLON signature { $$ = new NTypeDefinition(*$2, *$4); }
               | TEXTERN ident TCOLON signature { $$ = new NExternDeclaration(*$2, *$4); }
               | TVAR ident TCOLON signature TEQUAL expr { $$ = new NGlobalDeclaration(*$2, *$4, *$6); }
               | TTEMPLATE ident TBAR func_args TBAR TCOLON signature { END_COLUMN; $$ = new NTemplateDeclaration(GET_DEBUG_INFO(@1), false, *$2, *$4, *$7); }
               // Creates variants of classes
               | TPOLYMORPH TCLASSID TBAR func_args TBAR TCOLON signature { END_COLUMN; $$ = new NTemplateDeclaration(GET_DEBUG_INFO(@1), true, *new NIdentifier(GET_DEBUG_INFO(@1), *$2, 0), *$4, *$7); }
               // Creates variants of functions
               | TPOLYMORPH ident TCOLON signature { $$ = new NFunctionPrototype(*$2, *$4, true); }
               // Predeclare a variant
               | TPOLYMORPH ident TCLT type_collection TCGT {END_COLUMN; $$ = new NPredeclareVariant(*new NTemplateType(GET_DEBUG_INFO(@1), *$2, *$4, true));}
               ;

pointer_exprs : expr TDOT ident { END_COLUMN; $$ = new NDotOperator(GET_DEBUG_INFO(@1), *$1, *$3); }
              // TODO: replace expr with expr list
              | expr TLBRACKET expr TRBRACKET { $$ = new NBracketOperator(*$1, *$3); }
              | TADDR expr { $$ = new NAddressAtOperator(*$2); }
              | TAT expr { $$ = new NIndirectionOperator(*$2); }
              ;

var_decl : ident TCOLON signature { END_COLUMN; $$ = new NVariableDeclaration(*$1, *$3); }
         | ident TCOLON signature TEQUAL expr { END_COLUMN; $$ = new NVariableDeclaration(*$1, *$3, $5); }
         | ident TCOLON signature TREFARROW expr { END_COLUMN; $$ = new NVariableDeclaration(*$1, *$3, new NAddressAtOperator(*$5)); }
         | ident TCOLON signature TLARROW expr { END_COLUMN; $$ = new NVariableDeclaration(*$1, *$3, new NIndirectionOperator(*$5)); }
         ;

func_args : /*blank*/  { $$ = new IDList(); }
          | ident { $$ = new IDList(); $$->push_back($<ident>1); }
          | func_args TCOMMA ident { $1->push_back($<ident>3); }
          ;

func_decl : TIDENTIFIER TBAR func_args TBAR block 
            { END_COLUMN; $$ = new NFunctionDeclaration(GET_DEBUG_INFO(@1), *new NIdentifier(GET_DEBUG_INFO(@1), *$1, 0), *$3, *(new NBasicType(GET_DEBUG_INFO(@1), "void")), *$5); delete $3; }
          | TIDENTIFIER TBAR func_args TBAR TCOLON signature block
            { END_COLUMN; $$ = new NFunctionDeclaration(GET_DEBUG_INFO(@1), *new NIdentifier(GET_DEBUG_INFO(@1), *$1, 0), *$3, *$6, *$7); delete $3; }
          ;

ident     : TIDENTIFIER { END_COLUMN; $$ = new NIdentifier(GET_DEBUG_INFO(@1), *$1, 0); delete $1; }
          | TCLASSID { END_COLUMN; $$ = new NIdentifier(GET_DEBUG_INFO(@1), *$1, 0); delete $1; }
          ;

indices   : TINTEGER { $$ = new std::vector<int>{atoi($1->c_str())}; }
          | indices TCOMMA TINTEGER { $1->push_back(atoi($3->c_str())); }
          ;

primitives  : TINTEGER { $$ = new NInteger(atol($1->c_str())); delete $1; }
            | TDOUBLE { $$ = new NDouble(atof($1->c_str())); delete $1; }
            | TSTRING { $$ = new NString(unescape($1->substr(1, $1->length()-2))); delete $1; }
            | TCHAR { $$ = new NChar(unescape($1->substr(1, $1->length()-2))[0]); delete $1; }
            | TLINE { $$ = new NInteger(@1.last_line); }
            ;

function_pointer_exprs : expr TDOT ident TLPAREN call_args TRPAREN
                            { $$ = new NMemberCall(GET_DEBUG_INFO(@1), *$1, *$3, *$5); }
                       ;

expr : ident TLPAREN call_args TRPAREN { END_COLUMN; $$ = new NMethodCall(GET_DEBUG_INFO(@1), *$1, *$3); delete $3; }
     //| TLPAREN expr TRPAREN TLPAREN call_args TRPAREN { END_COLUMN; $$ = new NFunctionPointerCall(GET_DEBUG_INFO(@1), *$2, *$5); delete $5; }
     | ident { $<ident>$ = $1; }
     | pointer_exprs { $$ = $1; }
     | function_pointer_exprs { $$ = $1; }

     | TLBRACE call_args TRBRACE { $$ = new NArrayExpression(GET_DEBUG_INFO(@1), *$2); }

     // Three builtin "functions"
     | TSIZEOF TLPAREN expr TRPAREN { $$ = new NSizeofExpression($3); }
     | TSIZEOF TCOLON signature { $$ = new NSizeofExpression($3); }

     | TSTRINGIFY TLPAREN expr TRPAREN { END_COLUMN; $$ = new NStringifyExpression(GET_DEBUG_INFO(@1), $3); }
     | TSTRINGIFY TCOLON signature { END_COLUMN; $$ = new NStringifyExpression(GET_DEBUG_INFO(@1), $3); }

     | TPRINT TLPAREN expr TRPAREN { END_COLUMN; $$ = new NPrintExpression(GET_DEBUG_INFO(@1), $3); }
     | TEXIT TLPAREN expr TRPAREN { END_COLUMN; $$ = new NExitExpression(GET_DEBUG_INFO(@1), $3); }

     | TINC ident { $$ = new NIncrementExpression(*$2); }
     | TDEC ident { $$ = new NDecrementExpression(*$2); }

     // New expressions
     // For structs
     //| TNEW signature { END_COLUMN; $$ = new NNewExpression(GET_DEBUG_INFO(@1), *$3, *new ExpressionList(), false); }
     // For classes
     | TNEW signature TLPAREN call_args TRPAREN { END_COLUMN; $$ = new NNewExpression(GET_DEBUG_INFO(@1), *$2, *$4); delete $4; }
     // For class variant
     //| TNEW signature TCLT type_collection TCGT TLPAREN call_args TRPAREN { END_COLUMN; $$ = new NNewExpression(GET_DEBUG_INFO(@1), *$2, $4, *$7); delete $7; }
     
     // Checking types
     | expr TISTYPE signature { $$ = new NIsTypeOperator(*$1, *$3); }

     | primitives
     // Casting
     | expr TAS signature { $$ = new NBitCast(*$1, *$3); }
     // TODO: create custom operators
     | expr TMUL expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
     | expr TDIV expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
     | expr TMOD expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
     | expr TPLUS expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
     | expr TMINUS expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
     | expr comparison expr { $$ = new NBinaryOperator(*$1, $2, *$3); }


     | TLPAREN expr TRPAREN { $$ = $2; }
     ;

assign_stmt : expr TEQUAL expr { $$ = new NPointerAssignment(*$1, *$3); }
            | ident TEQUAL expr { $$ = new NAssignment(*$<ident>1, *$3); }
            | expr TREFARROW expr { $$ = new NPointerAssignment(*$1, *new NAddressAtOperator(*$3)); }
            | ident TREFARROW expr { $$ = new NAssignment(*$<ident>1, *new NAddressAtOperator(*$3)); }
            | expr TLARROW expr { $$ = new NPointerAssignment(*new NIndirectionOperator(*$1), *$3); }
            ;
    
call_args : /*blank*/  { $$ = new ExpressionList(); }
          | expr { $$ = new ExpressionList(); $$->push_back($1); }
          | call_args TCOMMA expr  { $1->push_back($3); }
          ;

class_decl  : TCLASSID TBAR func_args TBAR { class_stmts = new StatementList(); } class_block TEND
             { END_COLUMN; $$ = new NClassDeclaration(GET_DEBUG_INFO(@1), *new NIdentifier(GET_DEBUG_INFO(@1), *$1, 0), *$3, *(new NBasicType(GET_DEBUG_INFO(@1), "void")), *$6, *class_stmts); delete $3; class_stmts = NULL;}
            // A constructor variant friendly way
            | TCLASSID TCOLON TCLASS { class_stmts = new StatementList(); } class_block TEND
             { END_COLUMN; $$ = new NClassDeclaration(GET_DEBUG_INFO(@1), *new NIdentifier(GET_DEBUG_INFO(@1), *$1, 0), *(new NBasicType(GET_DEBUG_INFO(@1), "void")), *$5, *class_stmts); class_stmts = NULL;}
            // Essentially inheritance over here?
            | signature TFUNC TIDENTIFIER TBAR func_args TBAR block 
                { END_COLUMN; $$ = new NInheritedClassMethodDeclaration(GET_DEBUG_INFO(@1), *$1, *new NIdentifier(GET_DEBUG_INFO(@1), *$3, 0), *$5, *(new NBasicType(GET_DEBUG_INFO(@1), "void")), *$7, false); delete $5; }
            | signature TFUNC TIDENTIFIER TBAR func_args TBAR TCOLON signature block 
                { END_COLUMN; $$ = new NInheritedClassMethodDeclaration(GET_DEBUG_INFO(@1), *$1, *new NIdentifier(GET_DEBUG_INFO(@1), *$3, 0), *$5, *$8, *$9, true); delete $5; }
            ;
class_block : /*blank*/  { $$ = new NBlock(); }
            | assign_stmt { $$ = new NBlock(); $$->statements.push_back($<stmt>1); }
            | var_decl { $$ = new NBlock(); $$->statements.push_back($<stmt>1); }
            | class_block assign_stmt { $1->statements.push_back($<stmt>2); }
            | class_block var_decl { $1->statements.push_back($<stmt>2); }
            | class_block expr { $1->statements.push_back(new NExpressionStatement(*$2)); }
            | class_block TIDENTIFIER TBAR func_args TBAR block 
                { END_COLUMN; class_stmts->push_back(new NClassMethodDeclaration(GET_DEBUG_INFO(@1), *new NIdentifier(GET_DEBUG_INFO(@1), *$2, 0), *$4, *(new NBasicType(GET_DEBUG_INFO(@1), "void")), *$6, false)); delete $4; }
            | class_block TIDENTIFIER TBAR func_args TBAR TCOLON signature block 
                { END_COLUMN; class_stmts->push_back(new NClassMethodDeclaration(GET_DEBUG_INFO(@1), *new NIdentifier(GET_DEBUG_INFO(@1), *$2, 0), *$4, *$7, *$8, true)); delete $4; }
            | TIDENTIFIER TBAR func_args TBAR block 
                { END_COLUMN; class_stmts->push_back(new NClassMethodDeclaration(GET_DEBUG_INFO(@1), *new NIdentifier(GET_DEBUG_INFO(@1), *$1, 0), *$3, *(new NBasicType(GET_DEBUG_INFO(@1), "void")), *$5, false)); delete $3; }
            | TIDENTIFIER TBAR func_args TBAR TCOLON signature block 
                { END_COLUMN; class_stmts->push_back(new NClassMethodDeclaration(GET_DEBUG_INFO(@1), *new NIdentifier(GET_DEBUG_INFO(@1), *$1, 0), *$3, *$6, *$7, true)); delete $3; }
            ;
           ;

comparison : TCEQ | TCNE | TCLT | TCLE | TCGT | TCGE | TAND | TOR ;

%%
