%{
    open Ast
    let l = Location.make_location
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

(*%start <(token * Lexing.position) list> tokens*)
(*
%start <Ast.expr> prog
*)

%nonassoc COLON QUESTIONMARK
%left LOR LAND LNOT
%nonassoc EQ NEQ
%nonassoc LT LE GT GE
%left ADD MINUS
%left MUL DIV REM
%right LENGTHOF
%right ASSIGN 
%right DOT LBRACKET
%right LPAREN

%nonassoc IF
%nonassoc ELSE

(*%type <exp> exp
*)
%type <statement list> stmts
%type <statement> stmt
(*
%type <single_declaration> decl 
%type <declaration_block> decl_block 
%type <exp option> option(exp)
*)
%start <program> main


%%

%inline ident:
    i = IDENT {Ident {name = i; loc = l $loc}}

%inline binop:
| PLUS {Plus {loc = l $loc}}
| MINUS {Minus {loc = l $loc}}
| MUL {Mul {loc = l $loc}}
| DIV {Div {loc = l $loc}}
| REM {Rem {loc = l $loc}}
| LE {Le {loc = l $loc}}
| LT {Lt {loc = l $loc}}
| GE {Ge {loc = l $loc}}
| GT {Gt {loc = l $loc}}
| EQ {Eq {loc = l $loc}}
| NEQ {NEq {loc = l $loc}}
| LAND {Land {loc = l $loc}}
| LOR {Lor {loc = l $loc}}

%inline unop:
| MINUS {Neg {loc = l $loc}}
| LNOT {Lnot {loc = l $loc}}


exp:
i = INT_LIT {Integer {int = i; loc = l $loc}}

(*
| i = INT {Integer {int = i; loc = $startpos}}
| left=exp PLUS right=exp  { BinOp {op = Plus {loc = $startpos}; left; right} }
| left=exp MUL right=exp  { BinOp {op = MUL {pos = $startpos}; left; right} }
| left=exp DIV right=exp  { BinOp {op = DIV {pos = $startpos}; left; right} }
| left=exp MINUS right=exp  { BinOp {op = Minus {pos = $startpos}; left; right} }
*)
(*
prog:
  e=exp EOF { e }
*)

(*
single_token:
| i = INT { (INT, $startpos) }
| PLUS    { (PLUS, $startpos) }
| MINUS   { (MINUS, $startpos) }
| MUL   { (MUL, $startpos) }
| DIV  { (DIV, $startpos) }
| LPAREN  { (LPAREN, $startpos) }
| RPAREN  { (RPAREN, $startpos) }

tokens:
 tks = single_token* EOF  { tks }
*)


(*
type_helper:
|

type_def:
|

decl:
|

decl_block:
|

for_init:
|
*)

stmt:
 | RETURN e = exp SEMICOLON { ReturnStm {ret = e; loc = l $loc}}

stmts:
r = list(stmt) 
{r}

main:
| res = stmts EOF
    {res}