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

%left LOR 
%left LAND
%nonassoc EQ NEQ
%nonassoc LT LE GT GE 
%left PLUS MINUS
%left MUL DIV REM
%left LNOT



%type <expr> exp
%type <statement list> stmts
%type <statement> stmt
%type <single_declaration> decl 
%type <declaration_block> decl_block 
%type <expr option> option(exp)
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
| i = INT_LIT {Integer {int = i; loc = l $loc}}
| FALSE {Boolean {bool = false; loc = l $loc}}
| TRUE {Boolean {bool = true; loc = l $loc}}
| left = exp op = binop right = exp {BinOp {left; op; right; loc = l $loc}}
| op = unop i = exp {UnOp {op; operand = i; loc = l $loc}}
| fname = IDENT LPAREN args = separated_list(COMMA, exp) RPAREN { Call {fname = Ident {name = fname; loc = l $loc}; args = args; loc = l $loc} }
| LPAREN e = exp RPAREN {e}
| i = IDENT {Lval (Var (Ident {name = i; loc = l $loc})) }


type_helper:
| INT {Int {loc = l $loc}}
| BOOL {Bool {loc = l $loc}}

type_def:
| COLON t = type_helper {Some(t)}
| {None}

decl:
| name = ident tp = type_def ASSIGN e = exp
    {Declaration {name; tp; body = e; loc = l $loc}}

decl_block:
 VAR e = separated_list(COMMA, decl) {DeclBlock {declarations = e; loc = l $loc}}

for_init:
| e = exp {FIExpr e}
| e = decl_block {FIDecl e}


stmt:
 | RETURN e = exp SEMICOLON { ReturnStm {ret = e; loc = l $loc}}
 | e = decl_block SEMICOLON
    {VarDeclStm e}
 | LBRACE s = stmts RBRACE {CompoundStm {stms = s; loc = l $loc}}
 | e = option(exp) SEMICOLON {ExprStm {expr = e; loc = l $loc}}
 | IF LPAREN cond = exp RPAREN thbr = stmt elbro = option(ELSE elseStmt = stmt {elseStmt})
    {IfThenElseStm {cond; thbr; elbro; loc = l $loc}}
 | WHILE LPAREN e = exp RPAREN s = stmt {WhileStm {cond = e; body = s; loc = l $loc}}
 | FOR LPAREN init = option(for_init) SEMICOLON cond = option(exp) SEMICOLON update = option(exp) RPAREN body = stmt
    {ForStm {init; cond; update; body; loc = l $loc}}
 | BREAK SEMICOLON {BreakStm {loc = l $loc}}
 | CONTINUE SEMICOLON {ContinueStm {loc = l $loc}}

stmts:
r = list(stmt) 
{r}

main:
| res = stmts EOF
    {res}