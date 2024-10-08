module Ast = Ast
module TAst = TypedAst
module Env = Env
module Errors = Errors
module Sym = Symbol
module RunTimeBindings = RunTimeBindings
module B = CfgBuilder
module Ll = Ll

exception Unimplemented (* your code should eventually compile without this exception *)
exception UnexpectedInput of string

type cg_env = 
  { cfgb: CfgBuilder.cfg_builder ref
  ; locals: (Ll.ty * Ll.operand) Sym.Table.t}

let emit env b =
  let current_builder = !(env.cfgb) in
  let new_builder = b current_builder in
  env.cfgb := new_builder

let fresh_symbol = 
  let c = ref 0 in
  fun initial -> 
    let n = !c in c := n + 1; Sym.symbol (initial ^ (string_of_int n))

let binop_op_match (op : TAst.binop) : Ll.bop =
  match op with
  | Plus -> Add
  | Minus -> Sub
  | Mul -> Mul
  | Div -> SDiv
  | Rem -> SRem
  | Lor -> Or
  | Land -> And
  | _ -> raise Unimplemented

let comparison_op_match (op : TAst.binop) : Ll.cnd =
  match op with
  | Lt -> Slt
  | Le -> Sle
  | Gt -> Sgt
  | Ge -> Sge
  | Eq -> Eq
  | NEq -> Ne
  | _ -> raise Unimplemented


let type_op_match (tp : TAst.typ) : Ll.ty = 
  match tp with 
  | Void -> Void 
  | Int -> I64
  | Bool -> I1
  | ErrorType -> raise @@ UnexpectedInput "Not void/int/bool type!"

let type_of_expr (expr : TAst.expr) : Ll.ty =
  match expr with
  | Integer _ -> I64
  | Boolean _ -> I1
  | BinOp { tp; _ } -> type_op_match tp
  | UnOp { tp; _ } -> type_op_match tp
  | Lval (Var { tp; _ }) -> type_op_match tp
  | Assignment _ -> Void
  | Call { tp; _ } -> type_op_match tp


  
let rec codegen_expr env expr =
  let emit = emit env in
  let emit_insn_with_fresh hint inst = 
    let tmp = fresh_symbol hint in
    emit @@ CfgBuilder.add_insn (Some tmp, inst);
    Ll.Id tmp in

  
  match expr with
  | TAst.BinOp {left; op; right; _} -> (
    let left = codegen_expr env left in
    let right = codegen_expr env right in
    match op with 
      | Plus -> emit_insn_with_fresh "tmp_add" @@ Ll.Binop (Ll.Add, Ll.I64, left, right)
      | _ -> raise Unimplemented )
    
  | _ -> raise Unimplemented





let rec codegen_stmt env stm = 
  raise Unimplemented




let codegen_stmt_list env stmts = raise Unimplemented




let codegen_prog typed_prog = raise Unimplemented