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
// Functions
%token FUNC


%left COMMA
%nonassoc DECL
%left LOR 
%left LAND
%nonassoc EQ NEQ
%nonassoc LT LE GT GE 
%left PLUS MINUS
%left MUL DIV REM
%left LNOT
%left UMINUS
%left ASSIGN
%nonassoc ELSE



%type <expr> exp
%type <statement list> stmts
%type <statement> stmt
%type <single_declaration> decl 
%type <declaration_block> decl_block 
%type <expr option> option(exp)
%type <record_decl> record_decl
%type <field> field


%type <func_decl> func_decl
%type <param list> param_list
%type <param> param
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

field:
    i = ident ASSIGN e = commaexprcontainer SEMICOLON {{name = i; expr = e; loc = l $loc}}

new_expr:
    id = ident LBRACE fields = list(field) RBRACE
    {NewExpr {typ = Struct {id; loc = l $loc} ; obj = Record {fields} ; loc = l $loc}}
    | typ = type_helper LBRACKET size = commaexprcontainer RBRACKET {NewExpr {typ; obj = Array {size}; loc = l $loc}}

lval:
    | i = IDENT {(Var (Ident {name = i; loc = l $loc}))}
    | arr = exp LBRACKET index = commaexprcontainer RBRACKET {Idx {arr ; index ; loc = l $loc}}
    | rcrd = exp DOT field = ident {Fld {rcrd ; field; loc = l $loc}}

(*exp:
    | i = INT_LIT {Integer { int = i; loc = l $loc}}
    | str = STRING_LIT {String {str ; loc = l $loc}}
    | TRUE {Boolean {bool = true; loc = l $loc}}
    | FALSE {Boolean { bool = false; loc = l $loc}}
    | LPAREN e = commaexprcontainer RPAREN
        {e}
    | left = exp op = binop right = exp
        {BinOp {left; op; right; loc = l $loc}}
    | op = unop operand = exp %prec UMINUS
        {UnOp {op; operand ; loc = l $loc}}
    | fname = ident LPAREN args = separated_list(COMMA, exp) RPAREN
        {Call {fname; args; loc = l $loc}}
    | LENGTHOF LPAREN a = exp RPAREN {LengthOf {expr = a; loc = l $loc}}
    | i = lval ASSIGN rhs = exp
        {Assignment {lvl = i; rhs; loc = l $loc}}
    | i = lval {Lval i}
    | NEW a = new_expr {a}
    | NIL {Nil {loc = l $loc}} *)

exp:
| i = IDENT ASSIGN e = exp %prec ASSIGN { Assignment {lvl = Var (Ident {name = i; loc = l $loc}); rhs = e; loc = l $loc} }
| i = INT_LIT {Integer {int = i; loc = l $loc}}
| FALSE {Boolean {bool = false; loc = l $loc}}
| TRUE {Boolean {bool = true; loc = l $loc}}
| left = exp op = binop right = exp {BinOp {left; op; right; loc = l $loc}}
| op = unop i = exp {UnOp {op; operand = i; loc = l $loc}}
| fname = IDENT LPAREN args = separated_list(COMMA, exp) RPAREN { Call {fname = Ident {name = fname; loc = l $loc}; args = args; loc = l $loc} }
| i = IDENT {Lval (Var (Ident {name = i; loc = l $loc})) }
(*| LPAREN e = exp RPAREN {e}
| LPAREN le = exp COMMA re = exp RPAREN {CommaExpr {lhs = le; rhs = re; loc = l $loc}} *)
| LPAREN e = commaexprcontainer RPAREN {e}
| s = STRING_LIT { String {str = s; loc = l $loc}}
| NEW a = new_expr {a}
| NIL {Nil {loc = l $loc}}
| i = lval ASSIGN rhs = exp
        {Assignment {lvl = i; rhs; loc = l $loc}}
| i = lval {Lval i}


commaexprcontainer:
| le = exp COMMA re = exp  {CommaExpr {lhs = le; rhs = re; loc = l $loc}} 
| le = exp COMMA re = commaexprcontainer {CommaExpr {lhs = le; rhs = re; loc = l $loc}} 
| e = exp {e}

type_helper:
| INT {Int {loc = l $loc}}
| BOOL {Bool {loc = l $loc}}
| VOID {Void {loc = l $loc}}
| id = ident {Struct {id ; loc = l $loc}}
| STRING {Str {loc = l $loc}}
| LBRACKET typ = type_helper RBRACKET {ArrayType {typ ; loc = l $loc}}


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
| IF LPAREN cond = exp RPAREN  thbr = stmt elbro = option( ELSE elseStmt = stmt {elseStmt})
    {IfThenElseStm {cond; thbr; elbro; loc = l $loc}}
 | RETURN e = exp SEMICOLON { ReturnStm {ret = e; loc = l $loc}}
 | e = decl_block SEMICOLON {VarDeclStm e}
 | LBRACE s = stmts RBRACE {CompoundStm {stms = s; loc = l $loc}}
 | e = option(exp) SEMICOLON {ExprStm {expr = e; loc = l $loc}}
 | WHILE LPAREN e = exp RPAREN s = stmt {WhileStm {cond = e; body = s; loc = l $loc}}
 | FOR LPAREN init = option(for_init) SEMICOLON cond = option(exp) SEMICOLON update = option(exp) RPAREN body = stmt
    {ForStm {init; cond; update; body; loc = l $loc}}
 | BREAK SEMICOLON {BreakStm {loc = l $loc}}
 | CONTINUE SEMICOLON {ContinueStm {loc = l $loc}}


param:
| name = ident COLON tp = type_helper { Param {name; tp; loc = l $loc} }

param_list:
| p = separated_list(COMMA, param) { p }

stmts:
r = list(stmt) 
{r}

func_decl:
| ret_type = type_helper fname = ident LPAREN params = param_list RPAREN LBRACE body = stmts RBRACE
    { {fname; params; ret_type; body; loc = l $loc} }

record_decl:
    RECORD name = ident LBRACE fields = list(p = param SEMICOLON {p}) RBRACE
    {{name = name; fields = fields; loc = l $loc}}

(*record:
| id = ident {Struct {id ; loc = l $loc}}*)

global_elements:
    | f = func_decl { Function f }
    | r = record_decl { 
        Record r 
      }


global_scope:
    res = list(global_elements) {res}

main:
| EOF { Program [] }
| res = global_scope EOF
    { Program res }

(*
main:
| funcs = list(func_decl) EOF { { funcs } }
*)
(*
main:
| res = stmts EOF
    {res}*)