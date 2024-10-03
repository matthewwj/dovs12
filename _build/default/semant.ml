module Ast = Ast
module TAst = TypedAst
module Env = Env
module Errors = Errors
module Sym = Symbol

exception Unimplemented (* your code should eventually compile without this exception *)

let typecheck_typ = function
| Ast.Int -> TAst.Int
| Ast.Bool -> TAst.Bool

let typecheck_op op = 
  match op with 
  | Ast.Plus -> TAst.Plus
  | _ -> raise Unimplemented


(* should return a pair of a typed expression and its inferred type. you can/should use typecheck_expr inside infertype_expr. *)
let rec infertype_expr env expr =
  match expr with
  | Ast.Integer {int} ->
    (TAst.Integer {int}, TAst.Int)

  | Ast.Boolean {bool} -> 
    (TAst.Boolean {bool}, TAst.Bool)

  | Ast.BinOp {left; op; right} -> 
    let (left_expr, left_type) = infertype_expr env left in
    let (right_expr, right_type) = infertype_expr env right in
    let optyp = typecheck_op op in
    (match op with 
    | Ast.Plus | Ast.Minus | Ast.Mul | Ast.Div | Rem -> 
      if left_type = TAst.Int && right_type = TAst.Int then
        (TAst.BinOp {left = left_expr; op = optyp; right = right_expr; tp = TAst.Int}, TAst.Int)
      else 
        raise (Invalid_argument (Errors.error_to_string (Errors.TypeMismatch {expected = left_type; actual = right_type})))

    | Lt | Le | Gt | Ge -> 
      if left_type = TAst.Int && right_type = TAst.Int then
        (TAst.BinOp {left = left_expr; op = optyp; right = right_expr; tp = TAst.Bool}, TAst.Bool)
      else 
        raise (Invalid_argument (Errors.error_to_string (Errors.TypeMismatch {expected = left_type; actual = right_type})))

    | Eq | NEq -> 
      if left_type = right_type then
        (TAst.BinOp {left = left_expr; op = optyp; right = right_expr; tp = TAst.Bool}, TAst.Bool)
      else
        raise (Invalid_argument (Errors.error_to_string (Errors.TypeMismatch {expected = left_type; actual = right_type})))

    | Lor | Land ->
      if left_type = TAst.Bool && right_type = TAst.Bool then
        (TAst.BinOp {left = left_expr; op = optyp; right = right_expr; tp = TAst.Bool}, TAst.Bool)
      else 
        raise (Invalid_argument (Errors.error_to_string (Errors.TypeMismatch {expected = left_type; actual = right_type})))
    )
    
  | Ast.UnOp {op; operand} -> 
    let (operand_expr, operand_type) = infertype_expr env operand in
    let optyp = match op with
    | Ast.Neg when operand_type = TAst.Int -> TAst.Neg
    | Ast.Lnot when operand_type = TAst.Bool -> TAst.Lnot
    | _ -> raise (Invalid_argument (Errors.error_to_string (Errors.TypeMismatch {expected = operand_type; actual = operand_type}))) in
      (TAst.UnOp {op = optyp; operand = operand_expr; tp = operand_type}, operand_type)

  | Ast.Lval lvl -> 
    let (typed_lval, lval_type) = infertype_lval env lvl in
    (TAst.Lval typed_lval, lval_type)

  | Ast.Assignment {lvl; rhs} -> 
    raise Unimplemented

  | Ast.Call {fname; args} -> 
    let sym = Sym.symbol name in 
    (* Lookup the function in the environment *)
    Env.lookup_var_fun env (match fname with Ident {name} -> name) 
    raise Unimplemented


and infertype_lval env lvl =
  match lvl with
  | _ -> raise Unimplemented
(* checks that an expression has the required type tp by inferring the type and comparing it to tp. *)
and typecheck_expr env expr tp =
  let texpr, texprtp = infertype_expr env expr in
  if texprtp <> tp then raise Unimplemented;
  texpr

(* should check the validity of a statement and produce the corresponding typed statement. Should use typecheck_expr and/or infertype_expr as necessary. *)
let rec typecheck_statement env stm =
  match stm with
  | _ -> raise Unimplemented
(* should use typecheck_statement to check the block of statements. *)
and typecheck_statement_seq env stms = raise Unimplemented

(* the initial environment should include all the library functions, no local variables, and no errors. *)
let initial_environment = raise Unimplemented

(* should check that the program (sequence of statements) ends in a return statement and make sure that all statements are valid as described in the assignment. Should use typecheck_statement_seq. *)
let typecheck_prog prg = raise Unimplemented



let test =
  [
    Ast.Integer {int = 5L};
  ]
