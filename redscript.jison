/*******************************************************************************
  Project name: RedScript to JavaScript compiler
  Authors: 
    Kamil Jamroz
  Version: 0.9.0
*******************************************************************************/

/*******************************************************************************
  Definitions
*******************************************************************************/

%lex

%options flex case-insensitive

/*******************************************************************************
  Included code
*******************************************************************************/

%{

/* load all external modules */
ClassYYCodeGenerator  = require('./lib/redscript.ClassYYCodeGenerator.js').getInstance;
ClassYYTreeNode       = require('./lib/redscript.ClassYYTreeNode.js').getInstance;
ClassYYTree           = require('./lib/redscript.ClassYYTree.js').getInstance;
ClassYYDriver         = require('./lib/redscript.ClassYYDriver.js').getInstance;

/* Initialize objects */
YYCode    = new ClassYYCodeGenerator();
YYTree    = new ClassYYTree(new ClassYYTreeNode());
YYDriver  = new ClassYYDriver(YYTree);

%}

/*******************************************************************************
  Rules
*******************************************************************************/

%%

"//"(.*)                    /* skip comments */
\s+                         /* skip whitespaces */
"NEW"                       { return 'NEW'; }
"FUNC"                      { return 'FUNCTION'; }
"END"                       { return 'END'; }
"ELSE IF"                   { return 'ELSIF'; }
"ELSEIF"                    { return 'ELSIF'; }
"IF"                        { return 'IF'; }
"ELSE"                      { return 'ELSE'; }
"DO"                        { return 'DO'; }
"WHILE"                     { return 'WHILE'; }
"FOR"                       { return 'FOR'; }
"UNTIL"                     { return 'UNTIL'; }
"CONTINUE"                  { return 'CONTINUE'; }
"BREAK"                     { return 'BREAK'; }
"RETURN"                    { return 'RETURN'; }
"WHEN"                      { return 'WHEN'; }
"SWITCH"                    { return 'SWITCH'; }
"DEFAULT"                   { return 'DEFAULT'; }
"THEN"                      { return 'THEN'; }
"THROW"                     { return 'THROW'; }
"TRY"                       { return 'TRY'; }
"CATCH"                     { return 'CATCH'; }
"FINALLY"                   { return 'FINALLY'; }
"DEBUGGER"                  { return 'DEBUGGER'; }
"WITH"                      { return 'WITH'; }
"INSTANCEOF"                { return 'INSTANCEOF'; }
"TYPEOF"                    { return 'TYPEOF'; }
"PRIVATE"                   { return 'PRIVATE'; }
"CLASS"                     { return 'CLASS'; }
"DEFINE"                    { return 'DEFINE'; }
"DEF"                       { return 'DEF'; }
"EXTENDS"                   { return 'EXTENDS'; }
"OBJECT"                    { return 'OBJECT'; }
"CLONES"                    { return 'CLONES'; }
"EXPORT"                    { return 'EXPORT'; }
"FROM"                      { return 'FROM'; }
"CONST"                     { return 'CONST'; }
"VOID"                      { return 'VOID'; }
"DELETE"                    { return 'DELETE'; }
"IN"                        { return 'IN'; }
"OF"                        { return 'OF'; }
"THIS"                      { return 'THISTOKEN'; }
"TRUE"                      { return 'TRUETOKEN'; }
"FALSE"                     { return 'FALSETOKEN'; }
"NULL"                      { return 'NULLTOKEN'; }
"==="                       { return 'STREQ'; }
"!=="                       { return 'STRNEQ'; }
"=="                        { return 'EQEQ'; }
"!="                        { return 'NEQ'; }
"&&"                        { return 'AND'; }
"||"                        { return 'OR'; }
"+="                        { return 'PLUSEQUAL'; }
"-="                        { return 'MINUSEQUAL'; }
"<="                        { return 'LEQ'; }
">="                        { return 'GEQ'; }
"/="                        { return 'DIVEQUAL'; }
">>>="                      { return 'URSHIFTEQUAL'; }
"<<="                       { return 'LSHIFTEQUAL'; }
">>="                       { return 'RSHIFTEQUAL'; }
">>>"                       { return 'URSHIFT'; }
"<<"                        { return 'LSHIFT'; }
">>"                        { return 'RSHIFT'; }
"&="                        { return 'ANDEQUAL'; }
"^="                        { return 'XOREQUAL'; }
"|="                        { return 'OREQUAL'; }
"%="                        { return 'MODEQUAL'; }
"*="                        { return 'MULTEQUAL'; }
"++"                        { return 'PLUSPLUS'; }
"--"                        { return 'MINUSMINUS'; }
"{"                         { return 'OPENBRACE'; }
"}"                         { return 'CLOSEBRACE'; }
"["                         { return '['; }
"]"                         { return ']'; }
"("                         { return '('; }
")"                         { return ')'; }
","                         { return ','; }
"."                         { return '.'; }
":"                         { return ':'; }
";"                         { return ';'; }
"!"                         { return '!'; }
"?"                         { return '?'; }
"&"                         { return '&'; }
"|"                         { return '|'; }
"^"                         { return '^'; }
"="                         { return '='; }
"~"                         { return '~'; }
"+"                         { return '+'; }
"-"                         { return '-'; }
"/"                         { return '/'; }
"*"                         { return '*'; }
"%"                         { return '%'; }
">"                         { return '>'; }
"<"                         { return '<'; }
"@"                         { return '@'; }
[0-9]+(\.[0-9]+)?\b         { return 'NUMBER'; }
"0x"[0-9a-fA-F]+\b          { return 'HEX_NUMBER'; }
"0"[0-7]+\b                 { return 'OCT_NUMBER'; }
[a-zA-Z_$@][a-zA-Z0-9_$]*   { return 'IDENTIFIER'; }
\"([^\"]*?)\"               { return 'STRING'; }
\'([^\']*?)\'               { return 'STRING'; }
<<EOF>>                     { return 'EOF'; }
.                           { YYDriver.LogError('Invalid character'); }

/*******************************************************************************
  Subroutines
*******************************************************************************/

/lex

/*******************************************************************************
  Operators associations and precedence
*******************************************************************************/

%nonassoc                   IF_WITHOUT_ELSE
%nonassoc                   ELSE

%nonassoc                   MODULE_WITHOUT_EXPORT
%nonassoc                   DEFINE

%start                      Program 

/*******************************************************************************
  Language grammar
*******************************************************************************/

%%

Literal
    : NULLTOKEN                                                                   { $$ = null; }
    | TRUETOKEN                                                                   { $$ = true; }
    | FALSETOKEN                                                                  { $$ = false; }
    | NUMBER                                                                      { $$ = ($1 % 1 == 0) ? parseInt($1) : parseFloat($1); }
    | HEX_NUMBER                                                                  { $$ = $1; }
    | OCT_NUMBER                                                                  { $$ = $1; }
    | STRING                                                                      { $$ = $1; }
    ;
    
PrivateBlock
    : PRIVATE SourceElements END                                                  { $$ = new ClassYYTreeNode([ 'PRIVATE' ]).AddChildrenSet($2); }      
    ;
        
VariableStatement
    : VariableDeclarationList                                                     { $$ = $1; }
    ;
    
VariableDeclarationList
    : IDENTIFIER                                                                  { $$ = new ClassYYTreeNode([ 'VARIABLE', $1 ]); }
    | IDENTIFIER Initializer                                                      { $$ = new ClassYYTreeNode([ 'VARIABLE', $1 ]).AddChildrenSet($2); }
    ; 
    
Initializer
    : '=' AssignmentExpr                                                          { $$ = new ClassYYTreeNode([ 'OP', null, $1 ]).AddChildrenSet($2); } 
    ;   
    
AssignmentExpr   
    : ConditionalExpr                                                             { $$ = $1; }
    | LeftHandSideExpr AssignmentOperator AssignmentExpr                          { $$ = $1.AddChildrenSet(new ClassYYTreeNode([ 'OP', null, $2 ]).AddChildrenSet($3)); }
    ;
    
ConditionalExpr
    : LogicalORExpr                                                               { $$ = $1; }
    | LogicalORExpr '?' AssignmentExpr ':' AssignmentExpr                         { $$ = new ClassYYTreeNode([ 'CHECK' ]).AddHandles($1).AddChildrenSet($3).AddChildrenSet($5); }
    ;
    
LogicalORExpr
    : LogicalANDExpr                                                              { $$ = $1; }
    | LogicalORExpr OR LogicalANDExpr                                             { $$ = new ClassYYTreeNode([ 'OP', null, '||', null, 'SYM' ]).AddChildrenSet($1).AddChildrenSet($3); }
    ;                                                 
    
LogicalANDExpr
    : BitwiseORExpr                                                               { $$ = $1; }
    | LogicalANDExpr AND BitwiseORExpr                                            { $$ = new ClassYYTreeNode([ 'OP', null, '&&', null, 'SYM' ]).AddChildrenSet($1).AddChildrenSet($3); }
    ;
    
BitwiseORExpr
    : BitwiseXORExpr                                                              { $$ = $1; }
    | BitwiseORExpr '|' BitwiseXORExpr                                            { $$ = new ClassYYTreeNode([ 'OP', null, '|', null, 'SYM' ]).AddChildrenSet($1).AddChildrenSet($3); }
    ;
    
BitwiseXORExpr
    : BitwiseANDExpr                                                              { $$ = $1; }
    | BitwiseXORExpr '^' BitwiseANDExpr                                           { $$ = new ClassYYTreeNode([ 'OP', null, '^', null, 'SYM' ]).AddChildrenSet($1).AddChildrenSet($3); }
    ;
    
BitwiseANDExpr
    : EqualityExpr                                                                { $$ = $1; }
    | BitwiseANDExpr '&' EqualityExpr                                             { $$ = new ClassYYTreeNode([ 'OP', null, '&', null, 'SYM' ]).AddChildrenSet($1).AddChildrenSet($3); }
    ;
    
EqualityExpr
    : RelationalExpr                                                              { $$ = $1; }
    | EqualityExpr EQEQ RelationalExpr                                            { $$ = new ClassYYTreeNode([ 'OP', null, '===', null, 'SYM' ]).AddChildrenSet($1).AddChildrenSet($3); }
    | EqualityExpr NEQ RelationalExpr                                             { $$ = new ClassYYTreeNode([ 'OP', null, '!==', null, 'SYM' ]).AddChildrenSet($1).AddChildrenSet($3); }
    | EqualityExpr STREQ RelationalExpr                                           { $$ = new ClassYYTreeNode([ 'OP', null, '===', null, 'SYM' ]).AddChildrenSet($1).AddChildrenSet($3); }
    | EqualityExpr STRNEQ RelationalExpr                                          { $$ = new ClassYYTreeNode([ 'OP', null, '!==', null, 'SYM' ]).AddChildrenSet($1).AddChildrenSet($3); }
    ;
    
RelationalExpr
    : ShiftExpr                                                                   { $$ = $1; }
    | RelationalExpr '<' ShiftExpr                                                { $$ = new ClassYYTreeNode([ 'OP', null, '<', null, 'SYM' ]).AddChildrenSet($1).AddChildrenSet($3); }
    | RelationalExpr '>' ShiftExpr                                                { $$ = new ClassYYTreeNode([ 'OP', null, '>', null, 'SYM' ]).AddChildrenSet($1).AddChildrenSet($3); }
    | RelationalExpr LEQ ShiftExpr                                                { $$ = new ClassYYTreeNode([ 'OP', null, '<=', null, 'SYM' ]).AddChildrenSet($1).AddChildrenSet($3); }
    | RelationalExpr GEQ ShiftExpr                                                { $$ = new ClassYYTreeNode([ 'OP', null, '>=', null, 'SYM' ]).AddChildrenSet($1).AddChildrenSet($3); }
    | RelationalExpr INSTANCEOF ShiftExpr                                         { $$ = new ClassYYTreeNode([ 'OP', null, 'INSTANCEOF', null, 'SYM' ]).AddChildrenSet($1).AddChildrenSet($3); }
    | RelationalExpr TYPEOF ShiftExpr                                             { $$ = new ClassYYTreeNode([ 'OP', null, 'TYPEOF', null, 'SYM' ]).AddChildrenSet($1).AddChildrenSet($3); }
    | RelationalExpr IN ShiftExpr                                                 { $$ = new ClassYYTreeNode([ 'OP', null, 'IN', null, 'SYM' ]).AddChildrenSet($1).AddChildrenSet($3); }
    ; 
    
ShiftExpr
    : AdditiveExpr                                                                { $$ = $1; }
    | ShiftExpr LSHIFT AdditiveExpr                                               { $$ = new ClassYYTreeNode([ 'OP', null, '<<', null, 'SYM' ]).AddChildrenSet($1).AddChildrenSet($3); }
    | ShiftExpr RSHIFT AdditiveExpr                                               { $$ = new ClassYYTreeNode([ 'OP', null, '>>', null, 'SYM' ]).AddChildrenSet($1).AddChildrenSet($3); }
    | ShiftExpr URSHIFT AdditiveExpr                                              { $$ = new ClassYYTreeNode([ 'OP', null, '>>>', null, 'SYM' ]).AddChildrenSet($1).AddChildrenSet($3); }
    ; 
    
AdditiveExpr                                          
    : MultiplicativeExpr                                                          { $$ = $1; }
    | AdditiveExpr '+' MultiplicativeExpr                                         { $$ = new ClassYYTreeNode([ 'OP', null, '+', null, 'SYM' ]).AddChildrenSet($1).AddChildrenSet($3); }
    | AdditiveExpr '-' MultiplicativeExpr                                         { $$ = new ClassYYTreeNode([ 'OP', null, '-', null, 'SYM' ]).AddChildrenSet($1).AddChildrenSet($3); }
    ;
    
MultiplicativeExpr
    : UnaryExpr                                                                   { $$ = $1; }
    | MultiplicativeExpr '*' UnaryExpr                                            { $$ = new ClassYYTreeNode([ 'OP', null, '*', null, 'SYM' ]).AddChildrenSet($1).AddChildrenSet($3); }
    | MultiplicativeExpr '/' UnaryExpr                                            { $$ = new ClassYYTreeNode([ 'OP', null, '/', null, 'SYM' ]).AddChildrenSet($1).AddChildrenSet($3); }
    | MultiplicativeExpr '%' UnaryExpr                                            { $$ = new ClassYYTreeNode([ 'OP', null, '%', null, 'SYM' ]).AddChildrenSet($1).AddChildrenSet($3); }
    ; 
    
UnaryExpr
    : PostfixExpr                                                                 { $$ = $1; }
    | UnaryExprCommon                                                             { $$ = $1; }
    ;
    
PostfixExpr
    : LeftHandSideExpr                                                            { $$ = $1; }
    | LeftHandSideExpr PLUSPLUS                                                   { $$ = new ClassYYTreeNode([ 'OP', null, '++', null, 'AFT' ]).AddChildrenSet($1); }
    | LeftHandSideExpr MINUSMINUS                                                 { $$ = new ClassYYTreeNode([ 'OP', null, '--', null, 'AFT' ]).AddChildrenSet($1); }
    ;
    
UnaryExprCommon
    : VOID UnaryExpr                                                              { $$ = new ClassYYTreeNode([ 'OP', null, 'VOID' ]).AddChildrenSet($2); }
    | TYPEOF UnaryExpr                                                            { $$ = new ClassYYTreeNode([ 'OP', null, 'TYPEOF' ]).AddChildrenSet($2); }
    | PLUSPLUS UnaryExpr                                                          { $$ = new ClassYYTreeNode([ 'OP', null, '++' ]).AddChildrenSet($2); }
    | MINUSMINUS UnaryExpr                                                        { $$ = new ClassYYTreeNode([ 'OP', null, '--' ]).AddChildrenSet($2); }
    | '+' UnaryExpr                                                               { $$ = new ClassYYTreeNode([ 'OP', null, '+' ]).AddChildrenSet($2); }
    | '-' UnaryExpr                                                               { $$ = new ClassYYTreeNode([ 'OP', null, '-' ]).AddChildrenSet($2); }
    | '~' UnaryExpr                                                               { $$ = new ClassYYTreeNode([ 'OP', null, '~' ]).AddChildrenSet($2); }
    | '!' UnaryExpr                                                               { $$ = new ClassYYTreeNode([ 'OP', null, '!' ]).AddChildrenSet($2); }
    ;    

AssignmentOperator
    : '='                                                                         { $$ = $1; }
    | PLUSEQUAL                                                                   { $$ = $1; }
    | MINUSEQUAL                                                                  { $$ = $1; }
    | MULTEQUAL                                                                   { $$ = $1; }
    | DIVEQUAL                                                                    { $$ = $1; }
    | LSHIFTEQUAL                                                                 { $$ = $1; }
    | RSHIFTEQUAL                                                                 { $$ = $1; }
    | URSHIFTEQUAL                                                                { $$ = $1; }
    | ANDEQUAL                                                                    { $$ = $1; }
    | XOREQUAL                                                                    { $$ = $1; }
    | OREQUAL                                                                     { $$ = $1; }
    | MODEQUAL                                                                    { $$ = $1; }
    ;

LeftHandSideExpr
    : NewExpr                                                                     { $$ = $1; }
    ;  
        
NewExpr
    : MemberExpr                                                                  { $$ = $1; }
    | NEW NewExpr                                                                 { $$ = new ClassYYTreeNode([ 'OP', null, 'NEW' ]).AddChildrenSet($2); }
    ;

MemberExpr
    : PrimaryExpr                                                                 { $$ = $1; }
    | FunctionExpr                                                                { $$ = $1; }
    | MemberExpr '[' Expr ']'                                                     { $$ = $1.AddChildrenSet(new ClassYYTreeNode([ 'OP', null, '[', ']' ]).AddChildrenSet($3)); }
    | MemberExpr '.' IDENTIFIER                                                   { $$ = $1.AddChildrenSet(new ClassYYTreeNode([ 'OP', null, '.' ]).AddChildrenSet(new ClassYYTreeNode([ 'VARIABLE', $3 ]))); }
    | MemberExpr Arguments                                                        { $$ = $1.AddChildrenSet(new ClassYYTreeNode([ 'OP', null, '(', ')' ]).AddChildrenSet($2).SetJoin(',')); }
    ;
    
FunctionExpr
    : FUNCTION '(' ')' FunctionBody END                                           { $$ = new ClassYYTreeNode([ 'FUNCTION', null, [] ]).AddChildrenSet($4); }
    | FUNCTION '(' FormalParameterList ')' FunctionBody END                       { $$ = new ClassYYTreeNode([ 'FUNCTION', null, $3 ]).AddChildrenSet($5); }
    | FUNCTION FunctionBody END                                                   { $$ = new ClassYYTreeNode([ 'FUNCTION', null, [] ]).AddChildrenSet($2); }
    ;                                             
    
PrimaryExpr
    : PrimaryExprNoBrace                                                          { $$ = $1; }
    | OPENBRACE CLOSEBRACE                                                        { $$ = new ClassYYTreeNode([ 'BRACES', null, $1, $2 ]); }
    | OPENBRACE PropertyList CLOSEBRACE                                           { $$ = new ClassYYTreeNode([ 'BRACES', null, $1, $3 ]).AddChildrenSet($2).SetJoin(','); }
    | OPENBRACE PropertyList ',' CLOSEBRACE                                       { $$ = new ClassYYTreeNode([ 'BRACES', null, $1, $4 ]).AddChildrenSet($2).SetJoin(','); }
    ;
    
PrimaryExprNoBrace
    : THISTOKEN                                                                   { $$ = new ClassYYTreeNode([ 'VALUE', null, $1 ]); }
    | Literal                                                                     { $$ = new ClassYYTreeNode([ 'VALUE', null, $1 ]); }
    | ArrayLiteral                                                                { $$ = new ClassYYTreeNode([ 'ARRAY', null ]).AddChildrenSet($1).SetJoin(','); }
    | IDENTIFIER                                                                  { $$ = new ClassYYTreeNode([ 'VARIABLE', $1 ]); }
    | '(' Expr ')'                                                                { $$ = new ClassYYTreeNode([ 'OP', null, '(', ')' ]).AddChildrenSet($2); }
    ;
    
ArrayLiteral
    : '[' ElisionOpt ']'                                                          { $$ = []; }
    | '[' ElementList ']'                                                         { $$ = ($2 instanceof Array) ? $2 : [ $2 ]; }
    | '[' ElementList ',' ElisionOpt ']'                                          { $$ = ($2 instanceof Array) ? $2 : [ $2 ]; }
    ;

ElementList
    : ElisionOpt AssignmentExpr                                                   { $$ = $2; }
    | ElementList ',' ElisionOpt AssignmentExpr                                   { $$ = [ $4 ].concat($1); }
    ;

ElisionOpt
    :                                                                             { $$ = null; }
    | Elision                                                                     { $$ = $1; }
    ;

Elision
    : ','                                                                         { $$ = null; }
    | Elision ','                                                                 { $$ = $1; }
    ; 
    
Expr
    : AssignmentExpr                                                              { $$ = $1; }
    | Expr ',' AssignmentExpr                                                     { $$ = [ $3 ].concat($1); }
    ;
                   
PropertyList
    : Property                                                                    { $$ = $1; }
    | PropertyList ',' Property                                                   { $$ = [ $3 ].concat($1); }
    ;                                                     
    
Property
    : IDENTIFIER ':' AssignmentExpr                                               { $$ = new ClassYYTreeNode([ 'PROPERTY', $1 ]).AddChildrenSet($3); }
    | STRING ':' AssignmentExpr                                                   { $$ = new ClassYYTreeNode([ 'PROPERTY', $1 ]).AddChildrenSet($3); }
    | NUMBER ':' AssignmentExpr                                                   { $$ = new ClassYYTreeNode([ 'PROPERTY', $1 ]).AddChildrenSet($3); }
    ;
    
FormalParameterList
    : IDENTIFIER                                                                  { $$ = $1; }
    | FormalParameterList ',' IDENTIFIER                                          { $$ = [ $3 ].concat($1); }
    ;

FunctionBody
    :                                                                             { $$ = null; }
    | SourceElements                                                              { $$ = $1; }
    ;            

Arguments
    : '(' ')'                                                                     { $$ = []; }
    | '(' ArgumentList ')'                                                        { $$ = ($2 instanceof Array) ? $2 : [ $2 ]; }
    ;
    
ArgumentList
    : AssignmentExpr                                                              { $$ = $1; }
    | ArgumentList ',' AssignmentExpr                                             { $$ = [ $3 ].concat($1); }
    ;
    
ConstStatement
    : CONST ConstDeclarationList                                                  { $$ = $2; }
    ; 
    
ConstDeclarationList
    : ConstDeclaration                                                            { $$ = $1; }
    | ConstDeclarationList ',' ConstDeclaration                                   { $$ = [ $3 ].concat($1); }
    ;
    
ConstDeclaration
    : IDENTIFIER                                                                  { $$ = new ClassYYTreeNode([ 'CONST_VARIABLE', $1 ]); }
    | IDENTIFIER Initializer                                                      { $$ = new ClassYYTreeNode([ 'CONST_VARIABLE', $1 ]).AddChildrenSet($2); }
    ;
    
DeleteStatement
    : DELETE IDENTIFIER                                                           { $$ = new ClassYYTreeNode([ 'KEYWORD', 'DELETE', $2 ]); }
    ;  
    
FunctionDeclaration
    : FUNCTION IDENTIFIER '(' ')' FunctionBody END                                { $$ = new ClassYYTreeNode([ 'FUNCTION', $2, [] ]).AddChildrenSet($5); }
    | FUNCTION IDENTIFIER '(' FormalParameterList ')' FunctionBody END            { $$ = new ClassYYTreeNode([ 'FUNCTION', $2, $4 ]).AddChildrenSet($6); }
    | FUNCTION IDENTIFIER FunctionBody END                                        { $$ = new ClassYYTreeNode([ 'FUNCTION', $2, [] ]).AddChildrenSet($3); }
    ;
    
FunctionCall
    : IDENTIFIER Arguments                                                        { $$ = new ClassYYTreeNode([ 'VARIABLE', $1 ]).AddChildrenSet(new ClassYYTreeNode([ 'OP', null, '(', ')' ]).AddChildrenSet($2).SetJoin(',')); }
    | IDENTIFIER '.' FunctionCall                                                 { $$ = new ClassYYTreeNode([ 'VARIABLE', $1 ]).AddChildrenSet(new ClassYYTreeNode([ 'OP', null, '.' ]).AddChildrenSet($3)); }  
    ;
            
IfStatement
    : IF Expr StatementAllowEmpty END %prec IF_WITHOUT_ELSE                       { $$ = new ClassYYTreeNode([ 'IF', null ]).AddHandles($2).AddChildrenSet($3); }
    | IF Expr StatementAllowEmpty ELSE StatementAllowEmpty END                    { $$ = new ClassYYTreeNode([ 'IF', null, 'ELSE' ]).AddHandles($2).AddChildrenSet($3).AddChildrenSet($5); }
    ;  

EmptyStatement
    :                                                                             { $$ = null; }
    ;

StatementAllowEmpty
    : SourceElements                                                              { $$ = $1; }
    | EmptyStatement                                                              { $$ = $1; }
    ;
    
IterationStatement
    : WHILE Expr StatementAllowEmpty END                                          { $$ = new ClassYYTreeNode([ 'LOOP', 'WHILE' ]).AddHandles($2).AddChildrenSet($3); }
    | UNTIL Expr StatementAllowEmpty END                                          { $$ = new ClassYYTreeNode([ 'LOOP', 'UNTIL' ]).AddHandles($2).AddChildrenSet($3); }
    | FOR IDENTIFIER OF Expr StatementAllowEmpty END                              { $$ = new ClassYYTreeNode([ 'LOOP', 'FOR', 'OF', $2 ]).AddHandles($4).AddChildrenSet($5); }
    | FOR IDENTIFIER IN Expr StatementAllowEmpty END                              { $$ = new ClassYYTreeNode([ 'LOOP', 'FOR', 'IN', $2 ]).AddHandles($4).AddChildrenSet($5); }
    | FOR IDENTIFIER ',' IDENTIFIER IN Expr StatementAllowEmpty END               { $$ = new ClassYYTreeNode([ 'LOOP', 'FOR', 'IN', $2, $4]).AddHandles($6).AddChildrenSet($7); }
    ;
    
ExprOpt
    :                                                                             { $$ = null; }
    | Expr                                                                        { $$ = $1; }
    ;
    
ContinueStatement
    : CONTINUE                                                                    { $$ = new ClassYYTreeNode([ 'KEYWORD', 'CONTINUE' ]); }
    ;
    
BreakStatement
    : BREAK                                                                       { $$ = new ClassYYTreeNode([ 'KEYWORD', 'BREAK' ]); }
    ;    
    
ReturnStatement
    : RETURN Expr                                                                 { $$ = new ClassYYTreeNode([ 'KEYWORD', 'RETURN' ]).AddHandles($2); }
    ;
        
WithStatement
    : WITH Expr StatementAllowEmpty END                                           { $$ = new ClassYYTreeNode([ 'WITH' ]).AddHandles($2).AddChildrenSet($3); }
    ;  
    
SwitchStatement
    : SWITCH Expr CaseBlock END                                                   { $$ = new ClassYYTreeNode([ 'SWITCH' ]).AddHandles($2).AddChildrenSet($3); }
    ; 
    
CaseBlock
    : CaseClausesOpt                                                              { $$ = $1; }
    | CaseClausesOpt DefaultClause                                                { $$ = [ $2 ].concat($1); }
    ;
    
CaseClausesOpt
    :                                                                             { $$ = null; }
    | CaseClauses                                                                 { $$ = $1; }
    ;
    
CaseClauses
    : CaseClause                                                                  { $$ = $1; }
    | CaseClauses CaseClause                                                      { $$ = [ $2 ].concat($1); }
    ;
    
CaseClause
    : WHEN Expr                                                                   { $$ = new ClassYYTreeNode([ 'WHEN' ]).AddHandles($2); }
    | WHEN Expr SourceElements                                                    { $$ = new ClassYYTreeNode([ 'WHEN' ]).AddHandles($2).AddChildrenSet($3); }
    | WHEN Expr THEN Statement                                                    { $$ = new ClassYYTreeNode([ 'WHEN', null, 'THEN' ]).AddHandles($2).AddChildrenSet($4); }
    ;
    
DefaultClause
    : DEFAULT SourceElements                                                      { $$ = new ClassYYTreeNode([ 'DEFAULT' ]).AddChildrenSet($2); }                 
    ;
    
LabelledBody
    :                                                                             { $$ = null; }
    | SourceElements                                                              { $$ = $1; }
    ;
    
LabelledStatement
    : IDENTIFIER ':' LabelledBody END                                             { $$ = new ClassYYTreeNode([ 'LABEL', $1 ]).AddChildrenSet($3); }
    ;
    
ThrowStatement
    : THROW Expr                                                                  { $$ = new ClassYYTreeNode([ 'KEYWORD', 'THROW' ]).AddHandles($2); }
    ; 
    
TryStatement                                        
    : TRY SourceElements CATCH Expr SourceElements END                            { $$ = new ClassYYTreeNode([ 'TRY', 'CATCH' ]).AddHandles($4).AddChildrenSet($2).AddChildrenSet($5); }
    | TRY SourceElements CATCH Expr SourceElements FINALLY SourceElements END     { $$ = new ClassYYTreeNode([ 'TRY', 'CATCH', 'FINALLY' ]).AddHandles($4).AddChildrenSet($2).AddChildrenSet($5).AddChildrenSet($7); }      
    ;
    
DebuggerStatement
    : DEBUGGER                                                                    { $$ = new ClassYYTreeNode([ 'KEYWORD', 'DEBUGGER' ]); }
    ; 
    
ClassStatement
    : CLASS IDENTIFIER ObjectBody END                                             { $$ = new ClassYYTreeNode([ 'CLASS', $2 ]).AddChildrenSet($3).SetJoin(','); }
    | CLASS IDENTIFIER EXTENDS IDENTIFIER ObjectBody END                          { $$ = new ClassYYTreeNode([ 'CLASS', $2, $4 ]).AddChildrenSet($5).SetJoin(','); } 
    ;
    
ObjectBody
    : ObjectBodyStatement                                                         { $$ = $1; }
    | ObjectBody ObjectBodyStatement                                              { $$ = [ $2 ].concat($1); }
    ;
    
ObjectBodyStatement
    : Property                                                                    { $$ = $1; }
    | MethodDeclaration                                                           { $$ = $1; }
    ;
    
MethodDeclaration
    : DEF IDENTIFIER '(' ')' FunctionBody END                                     { $$ = new ClassYYTreeNode([ 'METHOD', $2, [] ]).AddChildrenSet($5); }
    | DEF IDENTIFIER '(' FormalParameterList ')' FunctionBody END                 { $$ = new ClassYYTreeNode([ 'METHOD', $2, $4 ]).AddChildrenSet($6); }
    | DEF IDENTIFIER FunctionBody END                                             { $$ = new ClassYYTreeNode([ 'METHOD', $2, [] ]).AddChildrenSet($3); }
    ;
    
MethodOuterStatement
    : DEF IDENTIFIER '.' IDENTIFIER '(' ')' FunctionBody END                      { $$ = new ClassYYTreeNode([ 'METHOD_OUT', $2 + '.' + $4, [] ]).AddChildrenSet($7); }
    | DEF IDENTIFIER '.' IDENTIFIER '(' FormalParameterList ')' FunctionBody END  { $$ = new ClassYYTreeNode([ 'METHOD_OUT', $2 + '.' + $4, $6 ]).AddChildrenSet($8); }
    | DEF IDENTIFIER '.' IDENTIFIER FunctionBody END                              { $$ = new ClassYYTreeNode([ 'METHOD_OUT', $2 + '.' + $4, [] ]).AddChildrenSet($5); }
    ;
    
PrototypeExtendenceStatement
    : DEF IDENTIFIER RSHIFT IDENTIFIER '(' ')' FunctionBody END                     { $$ = new ClassYYTreeNode([ 'METHOD_EXT', $2 + '.prototype.' + $4, [] ]).AddChildrenSet($7); }
    | DEF IDENTIFIER RSHIFT IDENTIFIER '(' FormalParameterList ')' FunctionBody END { $$ = new ClassYYTreeNode([ 'METHOD_EXT', $2 + '.prototype.' + $4, $6 ]).AddChildrenSet($8); }
    | DEF IDENTIFIER RSHIFT IDENTIFIER FunctionBody END                             { $$ = new ClassYYTreeNode([ 'METHOD_EXT', $2 + '.prototype.' + $4, [] ]).AddChildrenSet($5); }
    ;
    
ObjectStatement
    : OBJECT IDENTIFIER ObjectBody END                                            { $$ = new ClassYYTreeNode([ 'OBJECT', $2 ]).AddChildrenSet($3).SetJoin(',');  }
    | OBJECT IDENTIFIER CLONES IDENTIFIER ObjectBody END                          { $$ = new ClassYYTreeNode([ 'OBJECT', $2, $4 ]).AddChildrenSet($5).SetJoin(',');  }
    ;    
     
ModuleStatement
    : DEFINE IDENTIFIER StatementAllowEmpty END                                   { $$ = new ClassYYTreeNode([ 'MODULE', $2 ]).AddChildrenSet($3); }
    | DEFINE IDENTIFIER StatementAllowEmpty EXPORT ExportList END                 { $$ = new ClassYYTreeNode([ 'MODULE', $2, 'EXPORT' ]).AddHandles($5).AddChildrenSet($3).SetJoinH(','); }
    ;
    
ExportList
    : ExportOpt                                                                   { $$ = $1; }
    | ExportList ExportOpt                                                        { $$ = [ $2 ].concat($1); }
    ;
    
ExportOpt
    : IDENTIFIER                                                                  { $$ = new ClassYYTreeNode([ 'PROPERTY', $1 ]); }
    | IDENTIFIER FROM Expr                                                        { $$ = new ClassYYTreeNode([ 'PROPERTY', $1 ]).AddChildrenSet($3); }
    ;
     
Statement
    : ModuleStatement                                                             { $$ = $1.StopFlag(); }
    | PrivateBlock                                                                { $$ = $1.StopFlag(); }
    | VariableStatement                                                           { $$ = $1.StopFlag(); }
    | DeleteStatement                                                             { $$ = $1.StopFlag(); }
    | ConstStatement                                                              { $$ = $1.StopFlag(); }
    | FunctionDeclaration                                                         { $$ = $1.StopFlag(); }
    | FunctionCall                                                                { $$ = $1.StopFlag(); }
    | IfStatement                                                                 { $$ = $1.StopFlag(); }
    | IterationStatement                                                          { $$ = $1.StopFlag(); }
    | ContinueStatement                                                           { $$ = $1.StopFlag(); }
    | BreakStatement                                                              { $$ = $1.StopFlag(); }
    | ReturnStatement                                                             { $$ = $1.StopFlag(); }    
    | WithStatement                                                               { $$ = $1.StopFlag(); }
    | SwitchStatement                                                             { $$ = $1.StopFlag(); }
    | LabelledStatement                                                           { $$ = $1.StopFlag(); }
    | ThrowStatement                                                              { $$ = $1.StopFlag(); }
    | TryStatement                                                                { $$ = $1.StopFlag(); }
    | DebuggerStatement                                                           { $$ = $1.StopFlag(); }
    | ClassStatement                                                              { $$ = $1.StopFlag(); }
    | ObjectStatement                                                             { $$ = $1.StopFlag(); }
    | MethodOuterStatement                                                        { $$ = $1.StopFlag(); }
    | PrototypeExtendenceStatement                                                { $$ = $1.StopFlag(); }      
    ;

SourceElements
    : Statement                                                                   { $$ = $1; }
    | SourceElements Statement                                                    { $$ = [ $2 ].concat($1); }      
    ;

Program
    : EOF                                                                         { YYTree.root.AddChildrenSet(null); return YYDriver.Log(); }
    | SourceElements EOF                                                          { YYTree.root.AddChildrenSet($1);   return YYDriver.Log(); }
    ; 
