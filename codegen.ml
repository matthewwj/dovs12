module Ast = Ast
module TAst = TypedAst
module Env = Env
module Errors = Errors
module Sym = Symbol
module RunTimeBindings = RunTimeBindings
module B = CfgBuilder
module Ll = Ll

exception Unimplemented (* your code should eventually compile without this exception *)

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