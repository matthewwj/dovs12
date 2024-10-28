%{

%}

// end of file
%token EOF
// string literals
%token <string> STRING_LIT   (* Strings quoted with "" *)
// integer literals
%token <int64> INT_LIT
// booleans
%token TRUE FALSE
// length operation; for arrays and strings
%token LENGTHOF
// arithmetic oprations
%token PLUS MINUS MUL DIV REM
// comparison operators
%token LT LE GT GE
// logical operations
%token LOR LAND LNOT
// equality
%token EQ NEQ
// assignment
%token ASSIGN
// punctuation
%token QUESTIONMARK COLON COMMA SEMICOLON
// accessors
%token DOT LBRACKET RBRACKET
// braces
%token LBRACE RBRACE
// parentheses
%token LPAREN RPAREN
// identifiers
%token <string> IDENT
// keywords
%token NIL VAR LET IF ELSE WHILE FOR BREAK CONTINUE RETURN NEW
// types
%token INT BOOL STRING BYTE VOID RECORD

%start <(token * Lexing.position) list> tokens

%left ADD MINUS
%left MUL DIV



%%
single_token:
| i = INT { (INT i, $startpos) }
| PLUS    { (PLUS, $startpos) }
| MINUS   { (MINUS, $startpos) }
| TIMES   { (TIMES, $startpos) }
| DIVIDE  { (DIVIDE, $startpos) }
| LPAREN  { (LPAREN, $startpos) }
| RPAREN  { (RPAREN, $startpos) }

tokens:
 tks = single_token* EOF  { tks }
 