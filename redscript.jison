/*******************************************************************************
  Project name: RedScript to JavaScript compiler
  Authors: 
    Kamil Jamroz
  Version: 0.7.8
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

/* Some globals */
_yyDelimiter = '\n';

/* Declare CodeGenerator */
ClassYYCodeGenerator = function() {
  /* Generate Code */
  this.Generate = function(data) {
    var s;  
  
    if (typeof data === 'undefined' || data === null || data === false) {
      return '';
    }

    switch (data.type) {
      case 'VARIABLE':        s = this.ParseVariable(data); break;
      case 'OP':              s = this.ParseOp(data);       break;
      case 'VALUE':           s = this.ParseValue(data);    break;
      case 'BRACES':          s = this.ParseBraces(data);   break;
      case 'ARRAY':           s = this.ParseArray(data);    break;
      case 'PROPERTY':        s = this.ParseProperty(data); break;
      case 'CONST_VARIABLE':  s = this.ParseConst(data);    break;
      case 'KEYWORD':         s = this.ParseKeyword(data);  break;
      case 'IF':              s = this.ParseIf(data);       break;
      case 'FUNCTION':        s = this.ParseFunction(data); break;
      case 'LOOP':            s = this.ParseLoop(data);     break;
      case 'WITH':            s = this.ParseWith(data);     break;
      case 'SWITCH':          s = this.ParseSwitch(data);   break;
      case 'WHEN':            s = this.ParseWhen(data);     break;
      case 'DEFAULT':         s = this.ParseDefault(data);  break;
      case 'LABEL':           s = this.ParseLabel(data);    break;
      case 'TRY':             s = this.ParseTry(data);      break;
      case 'CLASS':           s = this.ParseClass(data);    break;
      case 'OBJECT':          s = this.ParseObject(data);   break;
      case 'METHOD':          s = this.ParseMethod(data);   break;
      case 'MODULE':          s = this.ParseModule(data);   break;
      case 'CHECK':           s = this.ParseCheck(data);    break;       
      default:                s = '';      
    }
    return s;
  };
  
  /* Parsers */
  this.ParseVariable = function(data) {
    return data.name;
  };
  
  this.ParseOp = function(data) {
    return data.data[0];
  };
  
  this.ParseValue = function(data) {
    var s = '';
    
    if (typeof data.data[1] != 'undefined') { s = data.data[1] + ' '; }
    s += new String(data.data[0]);
  
    return s;
  };
  
  this.ParseBraces = function(data) {
    return '';
  };
  
  this.ParseArray = function(data) {
    return '';
  };

  this.ParseProperty = function(data) {
    return '';
  }; 
  
  this.ParseConst = function(data) {
    return '';
  };
  
  this.ParseKeyword = function(data) {
    return '';
  };
  
  this.ParseIf = function(data) {
    return '';
  };
  
  this.ParseFunction = function(data) {
    return '';
  };
  
  this.ParseLoop = function(data) {
    return '';
  };
  
  this.ParseWith = function(data) {
    return '';
  };
  
  this.ParseSwitch = function(data) {
    return '';
  };

  this.ParseWhen = function(data) {
    return '';
  };
  
  this.ParseDefault = function(data) {
    return '';
  };
  
  this.ParseLabel = function(data) {
    return '';
  };
  
  this.ParseTry = function(data) {
    return '';
  };
  
  this.ParseClass = function(data) {
    return '';
  };
  
  this.ParseObject = function(data) {
    return '';
  };
  
  this.ParseMethod = function(data) {
    return '';
  };     
  
  this.ParseModule = function(data) {
    return '';
  };          
  
  this.ParseCheck = function(data) {
    return '';
  };                
  
  return this;
};

/* Declare TreeNode */
ClassYYTreeNode = function(data) {
  this.handles = [];
  this.children = [];  
  this.data = false;
  
  /* Add helper handles */
  this.AddHandles = function(nodes) {
    if (nodes instanceof Array) {
      for (i in nodes) {
        this.AddChild(nodes[i]);
      }
    } else {
      this.AddChild(nodes);
    }
    return this;  
  };
  
  /* Add helper */
  this.AddHandle = function(handle) {
    this.handles.push(handle);
    return this;
  };
  
  /* Add Children */
  this.AddChildren = function(nodes) {
    if (nodes instanceof Array) {
      for (i in nodes) {
        this.AddChild(nodes[i]);
      }
    } else {
      this.AddChild(nodes);
    }
    return this;
  };
      
  /* Add Child Node */
  this.AddChild = function(node) {
    this.children.push(node);
    return this;
  };
  
  /* Set data value of current node */
  this.Val = function(data) {
    this.data = {
      'type': data[0],
      'name': data[1]
    };
    data = data.splice(2);
    if (data.length > 0) {
      this.data.data = data;
    }
    return this;
  }; 
  
  /* Generate code */
  this.GenerateCode = function(ci) {
    var i, s, len, endOfCommand, emptyCommand;
  
    s = [];
    endOfCommand = false;
    emptyCommand = false;
    
    if (this.data !== false) {
      s.push(YYCode.Generate(this.data));
    } else {
      emptyCommand = true;
    }
    
    //console.log('Generate...[INDENT: ' + ci + '][' + this.children.length + ']\n');
    
    len = this.handles.length;
    if (len > 0) {
      for (i=len-1; i>=0; i--) {
        s.push(this.handles[i].GenerateCode(ci));
      }
    }    
    
    len = this.children.length;
    if (len > 0) {
      for (i=len-1; i>=0; i--) {
        s.push(this.children[i].GenerateCode(ci+1));
      }
    } else {
      endOfCommand = true;
    }
    
    d = (endOfCommand && !emptyCommand) ? ';\n' : '';

    return s.join(' ') + d;
  };
  
  if (typeof data != 'undefined' && data !== false) {
    this.Val(data);
  }  

  return this;
};

/* Declare TreeWalker */
ClassYYTree = function() {
  this.root = new ClassYYTreeNode();
                                   
  /* Add Node */
  this.SetRoot = function(node) {
    this.root = node;
    return this;
  }; 
  
  /* Generate output code */
  this.GenerateCode = function() {
    //return this.root.GenerateCode();
    return JSON.stringify(this.root, false, '\t');
    //return this.root.GenerateCode(0) + '\n\n' + JSON.stringify(this.root, false, '\t');
  };
  
  return this; 
};

/* Declare Driver  */
ClassYYDriver = function(TreeWalker) {
  this.TreeWalker = TreeWalker;
  this.errors = [];
  
  this.acceptNewlines = false;
  this.commentLine = false;

  /* Print generated code */
  this.Log = function() {
    code = this.TreeWalker.GenerateCode();
    typeof console !== 'undefined' ? console.log(code) : print(code);    
    return code;
  };
  
  /* Log error */
  this.LogError = function(msg) {
    this.errors.push(msg);
    return msg;
  };
  
  return this;
};

/* Initialize */
YYCode = new ClassYYCodeGenerator();
YYTree = new ClassYYTree();
YYDriver = new ClassYYDriver(YYTree);

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
[a-zA-Z_$][a-zA-Z0-9_$]*    { return 'IDENTIFIER'; }
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

%start                      Program 

/*******************************************************************************
  Language grammar
*******************************************************************************/

%%

Literal
    : NULLTOKEN                                     { $$ = null; }
    | TRUETOKEN                                     { $$ = true; }
    | FALSETOKEN                                    { $$ = false; }
    | NUMBER                                        { $$ = ($1 % 1 == 0) ? parseInt($1) : parseFloat($1); }
    | HEX_NUMBER                                    { $$ = $1; }
    | OCT_NUMBER                                    { $$ = $1; }
    | STRING                                        { $$ = $1; }
    ;
    
PrivateBlock
    : PRIVATE SourceElements END                    { $$ = new ClassYYTreeNode([ 'PRIVATE' ]).AddChildren($2); }      
    ;
        
VariableStatement
    : VariableDeclarationList                       { $$ = $1; }
    ;
    
VariableDeclarationList
    : IDENTIFIER                                    { $$ = new ClassYYTreeNode([ 'VARIABLE', $1 ]); }
    | IDENTIFIER Initializer                        { $$ = new ClassYYTreeNode([ 'VARIABLE', $1 ]).AddChildren($2); }
    ; 
    
Initializer
    : '=' AssignmentExpr                            { $$ = new ClassYYTreeNode([ 'OP', null, $1 ]).AddChildren($2); } 
    ;   
    
AssignmentExpr   
    : ConditionalExpr                                     { $$ = $1; }
    | LeftHandSideExpr AssignmentOperator AssignmentExpr  { $$ = $1.AddChildren(new ClassYYTreeNode([ 'OP', $2 ]).AddChildren($3)); }
    ;
    
ConditionalExpr
    : LogicalORExpr                                       { $$ = $1; }
    | LogicalORExpr '?' AssignmentExpr ':' AssignmentExpr { $$ = new ClassYYTreeNode([ 'CHECK' ]).AddHandles($1).AddChildren($3).AddChildren($5); }
    ;
    
LogicalORExpr
    : LogicalANDExpr                                { $$ = $1; }
    | LogicalORExpr OR LogicalANDExpr               { $$ = new ClassYYTreeNode([ 'OP', null, 'OR' ]).AddChildren($1).AddChildren($3); }
    ;
    
LogicalANDExpr
    : BitwiseORExpr                                 { $$ = $1; }
    | LogicalANDExpr AND BitwiseORExpr              { $$ = new ClassYYTreeNode([ 'OP', null, 'AND' ]).AddChildren($1).AddChildren($3); }
    ;
    
BitwiseORExpr
    : BitwiseXORExpr                                { $$ = $1; }
    | BitwiseORExpr '|' BitwiseXORExpr              { $$ = new ClassYYTreeNode([ 'OP', null, '|' ]).AddChildren($1).AddChildren($3); }
    ;
    
BitwiseXORExpr
    : BitwiseANDExpr                                { $$ = $1; }
    | BitwiseXORExpr '^' BitwiseANDExpr             { $$ = new ClassYYTreeNode([ 'OP', null, '^' ]).AddChildren($1).AddChildren($3); }
    ;
    
BitwiseANDExpr
    : EqualityExpr                                  { $$ = $1; }
    | BitwiseANDExpr '&' EqualityExpr               { $$ = new ClassYYTreeNode([ 'OP', null, '&' ]).AddChildren($1).AddChildren($3); }
    ;
    
EqualityExpr
    : RelationalExpr                                { $$ = $1; }
    | EqualityExpr EQEQ RelationalExpr              { $$ = new ClassYYTreeNode([ 'OP', null, 'EQEQ' ]).AddChildren($1).AddChildren($3); }
    | EqualityExpr NEQ RelationalExpr               { $$ = new ClassYYTreeNode([ 'OP', null, 'NEQ' ]).AddChildren($1).AddChildren($3); }
    | EqualityExpr STREQ RelationalExpr             { $$ = new ClassYYTreeNode([ 'OP', null, 'STREQ' ]).AddChildren($1).AddChildren($3); }
    | EqualityExpr STRNEQ RelationalExpr            { $$ = new ClassYYTreeNode([ 'OP', null, 'STRNEQ' ]).AddChildren($1).AddChildren($3); }
    ;
    
RelationalExpr
    : ShiftExpr                                     { $$ = $1; }
    | RelationalExpr '<' ShiftExpr                  { $$ = new ClassYYTreeNode([ 'OP', null, '<' ]).AddChildren($1).AddChildren($3); }
    | RelationalExpr '>' ShiftExpr                  { $$ = new ClassYYTreeNode([ 'OP', null, '>' ]).AddChildren($1).AddChildren($3); }
    | RelationalExpr LEQ ShiftExpr                  { $$ = new ClassYYTreeNode([ 'OP', null, 'LEQ' ]).AddChildren($1).AddChildren($3); }
    | RelationalExpr GEQ ShiftExpr                  { $$ = new ClassYYTreeNode([ 'OP', null, 'GEQ' ]).AddChildren($1).AddChildren($3); }
    | RelationalExpr INSTANCEOF ShiftExpr           { $$ = new ClassYYTreeNode([ 'OP', null, 'INSTANCEOF' ]).AddChildren($1).AddChildren($3); }
    | RelationalExpr TYPEOF ShiftExpr               { $$ = new ClassYYTreeNode([ 'OP', null, 'TYPEOF' ]).AddChildren($1).AddChildren($3); }
    | RelationalExpr IN ShiftExpr                   { $$ = new ClassYYTreeNode([ 'OP', null, 'IN' ]).AddChildren($1).AddChildren($3); }
    ; 
    
ShiftExpr
    : AdditiveExpr                                  { $$ = $1; }
    | ShiftExpr LSHIFT AdditiveExpr                 { $$ = new ClassYYTreeNode([ 'OP', null, 'LSHIFT' ]).AddChildren($1).AddChildren($3); }
    | ShiftExpr RSHIFT AdditiveExpr                 { $$ = new ClassYYTreeNode([ 'OP', null, 'RSHIFT' ]).AddChildren($1).AddChildren($3); }
    | ShiftExpr URSHIFT AdditiveExpr                { $$ = new ClassYYTreeNode([ 'OP', null, 'URSHIFT' ]).AddChildren($1).AddChildren($3); }
    ; 
    
AdditiveExpr
    : MultiplicativeExpr                            { $$ = $1; }
    | AdditiveExpr '+' MultiplicativeExpr           { $$ = new ClassYYTreeNode([ 'OP', null, '+' ]).AddChildren($1).AddChildren($3); }
    | AdditiveExpr '-' MultiplicativeExpr           { $$ = new ClassYYTreeNode([ 'OP', null, '-' ]).AddChildren($1).AddChildren($3); }
    ;
    
MultiplicativeExpr
    : UnaryExpr                                     { $$ = $1; }
    | MultiplicativeExpr '*' UnaryExpr              { $$ = new ClassYYTreeNode([ 'OP', null, '*' ]).AddChildren($1).AddChildren($3); }
    | MultiplicativeExpr '/' UnaryExpr              { $$ = new ClassYYTreeNode([ 'OP', null, '/' ]).AddChildren($1).AddChildren($3); }
    | MultiplicativeExpr '%' UnaryExpr              { $$ = new ClassYYTreeNode([ 'OP', null, '%' ]).AddChildren($1).AddChildren($3); }
    ; 
    
UnaryExpr
    : PostfixExpr                                   { $$ = $1; }
    | UnaryExprCommon                               { $$ = $1; }
    ;
    
PostfixExpr
    : LeftHandSideExpr                              { $$ = $1; }
    | LeftHandSideExpr PLUSPLUS                     { $$ = new ClassYYTreeNode([ 'OP', null, '%' ]).AddChildren($2); }
    | LeftHandSideExpr MINUSMINUS                   { $$ = new ClassYYTreeNode([ 'OP', null, '%' ]).AddChildren($2); }
    ;
    
UnaryExprCommon
    : VOID UnaryExpr                                { $$ = new ClassYYTreeNode([ 'OP', null, 'VOID' ]).AddChildren($2); }
    | TYPEOF UnaryExpr                              { $$ = new ClassYYTreeNode([ 'OP', null, 'TYPEOF' ]).AddChildren($2); }
    | PLUSPLUS UnaryExpr                            { $$ = new ClassYYTreeNode([ 'OP', null, 'PLUSPLUS' ]).AddChildren($2); }
    | MINUSMINUS UnaryExpr                          { $$ = new ClassYYTreeNode([ 'OP', null, 'MINUSMINUS' ]).AddChildren($2); }
    | '+' UnaryExpr                                 { $$ = new ClassYYTreeNode([ 'OP', null, '+' ]).AddChildren($2); }
    | '-' UnaryExpr                                 { $$ = new ClassYYTreeNode([ 'OP', null, '-' ]).AddChildren($2); }
    | '~' UnaryExpr                                 { $$ = new ClassYYTreeNode([ 'OP', null, '~' ]).AddChildren($2); }
    | '!' UnaryExpr                                 { $$ = new ClassYYTreeNode([ 'OP', null, '!' ]).AddChildren($2); }
    ;    

AssignmentOperator
    : '='                                           { $$ = $1; }
    | PLUSEQUAL                                     { $$ = $1; }
    | MINUSEQUAL                                    { $$ = $1; }
    | MULTEQUAL                                     { $$ = $1; }
    | DIVEQUAL                                      { $$ = $1; }
    | LSHIFTEQUAL                                   { $$ = $1; }
    | RSHIFTEQUAL                                   { $$ = $1; }
    | URSHIFTEQUAL                                  { $$ = $1; }
    | ANDEQUAL                                      { $$ = $1; }
    | XOREQUAL                                      { $$ = $1; }
    | OREQUAL                                       { $$ = $1; }
    | MODEQUAL                                      { $$ = $1; }
    ;

LeftHandSideExpr
    : NewExpr                                       { $$ = $1; }
    ;  
    
NewExpr
    : MemberExpr                                    { $$ = $1; }
    | NEW NewExpr                                   { $$ = new ClassYYTreeNode([ 'OP', null, $1 ]).AddChildren($2); }
    ;

MemberExpr
    : PrimaryExpr                                   { $$ = $1; }
    | FunctionExpr                                  { $$ = $1; }
    | MemberExpr '[' Expr ']'                       { $$ = new ClassYYTreeNode([ 'OP', null, $2, $4 ]).AddChildren($3); }
    | MemberExpr '.' IDENTIFIER                     { $$ = new ClassYYTreeNode([ 'OP', null, $2 ]).AddChildren($3); }
    | MemberExpr Arguments                          { $$ = new ClassYYTreeNode([ 'OP', null, '(', ')' ]).AddChildren($2); }
    ;
    
FunctionExpr
    : FUNCTION '(' ')' FunctionBody END                     { $$ = new ClassYYTreeNode([ 'FUNCTION', null, [] ]).AddChildren($4); }
    | FUNCTION '(' FormalParameterList ')' FunctionBody END { $$ = new ClassYYTreeNode([ 'FUNCTION', null, $3 ]).AddChildren($5); }
    | FUNCTION FunctionBody END                             { $$ = new ClassYYTreeNode([ 'FUNCTION', null, [] ]).AddChildren($2); }
    ;                                             
    
PrimaryExpr
    : PrimaryExprNoBrace                            { $$ = $1; }
    | OPENBRACE CLOSEBRACE                          { $$ = new ClassYYTreeNode([ 'BRACES', null, $1, $2 ]); }
    | OPENBRACE PropertyList CLOSEBRACE             { $$ = new ClassYYTreeNode([ 'BRACES', null, $1, $3 ]).AddChildren($2); }
    | OPENBRACE PropertyList ',' CLOSEBRACE         { $$ = new ClassYYTreeNode([ 'BRACES', null, $1, $4 ]).AddChildren($2); }
    ;
    
PrimaryExprNoBrace
    : THISTOKEN                                     { $$ = new ClassYYTreeNode([ 'VALUE', null, $1 ]); }
    | Literal                                       { $$ = new ClassYYTreeNode([ 'VALUE', null, $1 ]); }
    | ArrayLiteral                                  { $$ = new ClassYYTreeNode([ 'ARRAY', null, $1 ]); }
    | IDENTIFIER                                    { $$ = new ClassYYTreeNode([ 'VARIABLE', $1 ]); }
    | '@' IDENTIFIER                                { $$ = new ClassYYTreeNode([ 'VARIABLE', 'this.'.$2 ]); }
    | '(' Expr ')'                                  { $$ = $2; }
    ;
    
ArrayLiteral
    : '[' ElisionOpt ']'                            { $$ = []; }
    | '[' ElementList ']'                           { $$ = ($2 instanceof Array) ? $2 : [ $2 ]; }
    | '[' ElementList ',' ElisionOpt ']'            { $$ = ($2 instanceof Array) ? $2 : [ $2 ]; }
    ;

ElementList
    : ElisionOpt AssignmentExpr                     { $$ = $2; }
    | ElementList ',' ElisionOpt AssignmentExpr     { $$ = [ $4 ].concat($1); }
    ;

ElisionOpt
    :                                               { $$ = null; }
    | Elision                                       { $$ = null; }
    ;

Elision
    : ','                                           { $$ = null; }
    | Elision ','                                   { $$ = null; }
    ; 
    
Expr
    : AssignmentExpr                                { $$ = $1; }
    | Expr ',' AssignmentExpr                       { $$ = [ $3 ].concat($1); }
    ;
                   
PropertyList
    : Property                                      { $$ = $1; }
    | PropertyList ',' Property                     { $$ = [ $3 ].concat($1); }
    ;                                                     
    
Property
    : IDENTIFIER ':' AssignmentExpr                 { $$ = new ClassYYTreeNode([ 'PROPERTY', $1 ]).AddChildren($3); }
    | STRING ':' AssignmentExpr                     { $$ = new ClassYYTreeNode([ 'PROPERTY', $1 ]).AddChildren($3); }
    | NUMBER ':' AssignmentExpr                     { $$ = new ClassYYTreeNode([ 'PROPERTY', $1 ]).AddChildren($3); }
    ;
    
FormalParameterList
    : IDENTIFIER                                    { $$ = $1; }
    | FormalParameterList ',' IDENTIFIER            { $$ = [ $3 ].concat($1); }
    ;

FunctionBody
    :                                               { $$ = null; }
    | SourceElements                                { $$ = $1; }
    ;            

Arguments
    : '(' ')'                                       { $$ = []; }
    | '(' ArgumentList ')'                          { $$ = ($2 instanceof Array) ? $2 : [ $2 ]; }
    ;
    
ArgumentList
    : AssignmentExpr                                { $$ = $1; }
    | ArgumentList ',' AssignmentExpr               { $$ = [ $3 ].concat($1); }
    ;
    
ConstStatement
    : CONST ConstDeclarationList                    { $$ = $1; }
    ; 
    
ConstDeclarationList
    : ConstDeclaration                              { $$ = $1; }
    | ConstDeclarationList ',' ConstDeclaration     { $$ = [ $3 ].concat($1); }
    ;
    
ConstDeclaration
    : IDENTIFIER                                    { $$ = new ClassYYTreeNode([ 'CONST_VARIABLE', $1 ]); }
    | IDENTIFIER Initializer                        { $$ = new ClassYYTreeNode([ 'CONST_VARIABLE', $1 ]).AddChildren($2); }
    ;
    
DeleteStatement
    : DELETE IDENTIFIER                             { $$ = new ClassYYTreeNode([ 'KEYWORD', 'DELETE', $2 ]); }
    ;  
    
FunctionDeclaration
    : FUNCTION IDENTIFIER '(' ')' FunctionBody END                      { $$ = new ClassYYTreeNode([ 'FUNCTION', $2, [] ]).AddChildren($5); }
    | FUNCTION IDENTIFIER '(' FormalParameterList ')' FunctionBody END  { $$ = new ClassYYTreeNode([ 'FUNCTION', $2, $4 ]).AddChildren($6); }
    | FUNCTION IDENTIFIER FunctionBody END                              { $$ = new ClassYYTreeNode([ 'FUNCTION', $2, [] ]).AddChildren($3); }
    ;
        
IfStatement
    : IF Expr StatementAllowEmpty END %prec IF_WITHOUT_ELSE           { $$ = new ClassYYTreeNode([ 'IF' ]).AddChildren($2).AddChildren($3); }
    | IF Expr StatementAllowEmpty ELSE StatementAllowEmpty END        { $$ = new ClassYYTreeNode([ 'IF' ]).AddChildren($2).AddChildren($3).AddChildren($5); }
    ;  

EmptyStatement
    :                                               { $$ = null; }
    ;

StatementAllowEmpty
    : Statement                                     { $$ = $1; }
    | EmptyStatement                                { $$ = $1; }
    ;
    
IterationStatement
    : WHILE Expr StatementAllowEmpty END                                                                    { $$ = new ClassYYTreeNode([ 'LOOP', 'WHILE' ]).AddHandles($2).AddChildren($3); }
    | UNTIL Expr StatementAllowEmpty END                                                                    { $$ = new ClassYYTreeNode([ 'LOOP', 'UNTIL' ]).AddHandles($2).AddChildren($3); }
    | FOR IDENTIFIER OF Expr StatementAllowEmpty END                                                        { $$ = new ClassYYTreeNode([ 'LOOP', 'FOR', 'OF', $2 ]).AddHandles($4).AddChildren($5); }
    | FOR IDENTIFIER IN Expr StatementAllowEmpty END                                                        { $$ = new ClassYYTreeNode([ 'LOOP', 'FOR', 'IN', $2 ]).AddHandles($4).AddChildren($5); }
    | FOR IDENTIFIER ',' IDENTIFIER IN Expr StatementAllowEmpty END                                         { $$ = new ClassYYTreeNode([ 'LOOP', 'FOR', 'IN', [$2, $4]]).AddHandles($6).AddChildren($7); }
    ;
    
ExprOpt
    :                                               { $$ = null; }
    | Expr                                          { $$ = $1; }
    ;
    
ContinueStatement
    : CONTINUE                                      { $$ = new ClassYYTreeNode([ 'KEYWORD', 'CONTINUE' ]); }
    | CONTINUE IDENT                                { $$ = new ClassYYTreeNode([ 'KEYWORD', 'CONTINUE', $2 ]); }
    ;
    
BreakStatement
    : BREAK                                         { $$ = new ClassYYTreeNode([ 'KEYWORD', 'BREAK' ]); }
    | BREAK IDENT                                   { $$ = new ClassYYTreeNode([ 'KEYWORD', 'BREAK', $2 ]); }
    ;    
    
ReturnStatement
    : RETURN Expr END                               { $$ = new ClassYYTreeNode([ 'KEYWORD', 'RETURN' ]).AddHandles($2); }
    ;
        
WithStatement
    : WITH Expr StatementAllowEmpty END             { $$ = new ClassYYTreeNode([ 'WITH' ]).AddHandles($2).AddChildren($3); }
    ;  
    
SwitchStatement
    : SWITCH Expr CaseBlock END                     { $$ = new ClassYYTreeNode([ 'SWITCH' ]).AddHandles($2).AddChildren($3); }
    ; 
    
CaseBlock
    : CaseClausesOpt                                { $$ = $1; }
    | CaseClausesOpt DefaultClause                  { $$ = [ $2 ].concat($1); }
    ;
    
CaseClausesOpt
    :                                               { $$ = null; }
    | CaseClauses                                   { $$ = $1; }
    ;
    
CaseClauses
    : CaseClause                                    { $$ = $1; }
    | CaseClauses CaseClause                        { $$ = [ $2 ].concat($1); }
    ;
    
CaseClause
    : WHEN Expr                                     { $$ = new ClassYYTreeNode([ 'WHEN' ]).AddHandles($2); }
    | WHEN Expr SourceElements                      { $$ = new ClassYYTreeNode([ 'WHEN' ]).AddHandles($2).AddChildren($3); }
    | WHEN Expr THEN Statement                      { $$ = new ClassYYTreeNode([ 'WHEN', null, 'THEN' ]).AddHandles($2).AddChildren($4); }
    ;
    
DefaultClause
    : DEFAULT SourceElements                        { $$ = new ClassYYTreeNode([ 'DEFAULT' ]).AddChildren($2); }                 
    ;
    
LabelledBody
    :                                               { $$ = null; }
    | SourceElements                                { $$ = $1; }
    ;
    
LabelledStatement
    : IDENTIFIER ':' LabelledBody END               { $$ = new ClassYYTreeNode([ 'LABEL', $1 ]).AddChildren($3); }
    ;
    
ThrowStatement
    : THROW Expr                                    { $$ = new ClassYYTreeNode([ 'KEYWORD', 'THROW' ]).AddHandles($2); }
    ; 
    
TryStatement                                        
    : TRY SourceElements FINALLY SourceElements END                           { $$ = new ClassYYTreeNode([ 'TRY', 'FINALLY' ]).AddChildren($2).AddChildren($4); }
    | TRY SourceElements CATCH Expr SourceElements END                        { $$ = new ClassYYTreeNode([ 'TRY', 'CATCH' ]).AddHandles($4).AddChildren($2).AddChildren($5); }
    | TRY SourceElements CATCH Expr SourceElements FINALLY SourceElements END { $$ = new ClassYYTreeNode([ 'TRY', 'CATCH', 'FINALLY' ]).AddHandles($4).AddChildren($2).AddChildren($5).AddChildren($7); }      
    ;
    
DebuggerStatement
    : DEBUGGER                                      { $$ = new ClassYYTreeNode([ 'KEYWORD', 'DEBUGGER' ]); }
    ; 
    
ClassStatement
    : CLASS IDENTIFIER ObjectBody END                    { $$ = new ClassYYTreeNode([ 'CLASS', $2 ]).AddChildren($3); }
    | CLASS IDENTIFIER EXTENDS IDENTIFIER ObjectBody END { $$ = new ClassYYTreeNode([ 'CLASS', $2, $4 ]).AddChildren($5); } 
    ;
    
ObjectBody
    : ObjectBodyStatement                           { $$ = $1; }
    | ObjectBody ObjectBodyStatement                { $$ = [ $2 ].concat($1); }
    ;
    
ObjectBodyStatement
    : Property                                      { $$ = $1; }
    | MethodDeclaration                             { $$ = $1; }
    ;
    
MethodDeclaration
    : DEF IDENTIFIER '(' ')' FunctionBody END                      { $$ = new ClassYYTreeNode([ 'METHOD', $2, [] ]).AddChildren($5); }
    | DEF IDENTIFIER '(' FormalParameterList ')' FunctionBody END  { $$ = new ClassYYTreeNode([ 'METHOD', $2, $4 ]).AddChildren($6); }
    | DEF IDENTIFIER FunctionBody END                              { $$ = new ClassYYTreeNode([ 'METHOD', $2, [] ]).AddChildren($3); }
    ;
    
MethodOuterStatement
    : DEF IDENTIFIER '.' IDENTIFIER '(' ')' FunctionBody END                     { $$ = new ClassYYTreeNode([ 'METHOD', $4, [], $2 ]).AddChildren($7); }
    | DEF IDENTIFIER '.' IDENTIFIER '(' FormalParameterList ')' FunctionBody END { $$ = new ClassYYTreeNode([ 'METHOD', $4, $6, $2 ]).AddChildren($8); }
    | DEF IDENTIFIER '.' IDENTIFIER FunctionBody END                             { $$ = new ClassYYTreeNode([ 'METHOD', $4, [], $2 ]).AddChildren($5); }
    ;
    
PrototypeExtendenceStatement
    : DEF IDENTIFIER '>>' IDENTIFIER '(' ')' FunctionBody END                     { $$ = new ClassYYTreeNode([ 'METHOD', $2, [], $4 ]).AddChildren($7); }
    | DEF IDENTIFIER '>>' IDENTIFIER '(' FormalParameterList ')' FunctionBody END { $$ = new ClassYYTreeNode([ 'METHOD', $2, $6, $4 ]).AddChildren($8); }
    | DEF IDENTIFIER '>>' IDENTIFIER FunctionBody END                             { $$ = new ClassYYTreeNode([ 'METHOD', $2, [], $4 ]).AddChildren($5); }
    ;
    
ObjectStatement
    : OBJECT IDENTIFIER ObjectBody END                    { $$ = new ClassYYTreeNode([ 'OBJECT', $2 ]).AddChildren($3);  }
    | OBJECT IDENTIFIER CLONES IDENTIFIER ObjectBody END  { $$ = new ClassYYTreeNode([ 'OBJECT', $2, $4 ]).AddChildren($5);  }
    ;    
     
ModuleStatement
    : DEFINE IDENTIFIER StatementAllowEmpty END                   { $$ = new ClassYYTreeNode([ 'MODULE', $2 ]).AddChildren($3); }
    | DEFINE IDENTIFIER StatementAllowEmpty EXPORT ExportList END { $$ = new ClassYYTreeNode([ 'MODULE', $2 ]).AddHandles($5).AddChildren(3); }
    ;
    
ExportList
    : ExportOpt                                     { $$ = $1; }
    | ExportList ExportOpt                          { $$ = [ $2 ].concat($1); }
    ;
    
ExportOpt
    : IDENTIFIER                                    { $$ = new ClassYYTreeNode([ 'VARIABLE', $1 ]); }
    | IDENTIFIER FROM Expr                          { $$ = new ClassYYTreeNode([ 'VARIABLE', $1 ]).AddChildren($3); }
    ;
     
Statement
    : ModuleStatement                               { $$ = $1; }
    | PrivateBlock                                  { $$ = $1; }
    | VariableStatement                             { $$ = $1; }
    | DeleteStatement                               { $$ = $1; }
    | ConstStatement                                { $$ = $1; }
    | FunctionDeclaration                           { $$ = $1; }
    | IfStatement                                   { $$ = $1; }
    | IterationStatement                            { $$ = $1; }
    | ContinueStatement                             { $$ = $1; }
    | BreakStatement                                { $$ = $1; }
    | ReturnStatement                               { $$ = $1; }    
    | WithStatement                                 { $$ = $1; }
    | SwitchStatement                               { $$ = $1; }
    | LabelledStatement                             { $$ = $1; }
    | ThrowStatement                                { $$ = $1; }
    | TryStatement                                  { $$ = $1; }
    | DebuggerStatement                             { $$ = $1; }
    | ClassStatement                                { $$ = $1; }
    | ObjectStatement                               { $$ = $1; }
    | MethodOuterStatement                          { $$ = $1; }
    | PrototypeExtendenceStatement                  { $$ = $1; }       
    ;

SourceElements
    : Statement                                     { $$ = $1; }
    | SourceElements Statement                      { $$ = [ $2 ].concat($1); }      
    ;

Program
    : EOF                                           { YYTree.root.AddChildren(null); return YYDriver.Log(); }
    | SourceElements EOF                            { YYTree.root.AddChildren($1);   return YYDriver.Log(); }
    ; 
