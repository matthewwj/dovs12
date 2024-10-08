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

  (* Helper functions below*)
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
  | Int -> Ll.I64
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


(* Codegen for expressions *)
let rec codegen_expr env expr =
  let emit = emit env in
  let emit_insn_with_fresh hint inst = 
    let tmp = fresh_symbol hint in
    emit @@ CfgBuilder.add_insn (Some tmp, inst);
    Ll.Id tmp in
  match expr with
  | TAst.Integer {int} -> Ll.IConst64 int
  | TAst.Boolean {bool} -> Ll.BConst bool
  | TAst.BinOp {left; op; right; _} -> (
    let cleft = codegen_expr env left in
    let cright = codegen_expr env right in
    let paramtyp = type_of_expr left in
    match op with 
    | TAst.Plus | TAst.Minus | TAst.Mul | TAst.Div | TAst.Rem ->
      emit_insn_with_fresh "temp_name" @@ Ll.Binop (binop_op_match op, Ll.I64, cleft, cright)
    | TAst.Lt | TAst.Le | TAst.Gt | TAst.Ge -> 
      emit_insn_with_fresh "temp_name" @@ Ll.Icmp (comparison_op_match op, Ll.I64, cleft, cright)
    | _ -> raise Unimplemented (*Missing Logical operators still*)
      )
  | TAst.UnOp {op; operand; _} -> (
    let coperand = codegen_expr env operand in
    match op with
    | TAst.Neg -> 
      emit_insn_with_fresh "neg" @@ Ll.Binop (Sub, Ll.I64, Ll.IConst64 0L, coperand)
    | TAst.Lnot -> 
      emit_insn_with_fresh "not" @@ Ll.Icmp (Eq, Ll.I1, Ll.BConst true, coperand)
  )
  | TAst.Lval (Var {ident = Ident {sym}; tp}) -> (
    let llty = type_op_match tp in
    match Sym.Table.find_opt sym env.locals with
    | Some (llty, llop) -> 
      (* Load the value from the memory address *)
      emit_insn_with_fresh "load" @@ Ll.Load (llty, llop);
    | None -> raise @@ UnexpectedInput "Variable not found"
  ) 
  | TAst.Assignment {lvl = Var {ident = Ident {sym}; tp}; rhs; _} -> (
    let crhs = codegen_expr env rhs in
    match Sym.Table.find_opt sym env.locals with
    | Some (llty, llop) ->
      emit @@ CfgBuilder.add_insn (None, Ll.Store (llty, crhs, llop));
      crhs
    | None -> raise @@ UnexpectedInput "Variable not found"
  )
  | TAst.Call {fname = Ident { sym }; args; tp} -> (
    let llty = type_op_match tp in
    let carglist = List.map (fun arg -> 
      let carg = codegen_expr env arg in
      let carg_ty = type_of_expr arg in
      (carg_ty, carg)) args in
    emit_insn_with_fresh "call" @@ Ll.Call (llty, Ll.Gid sym, carglist)
  )



let rec codegen_stmt env stm = 
  let emit = emit env in
  match stm with
  | TAst.VarDeclStm {name = Ident {sym}; tp; body} ->
    let rhs_val = codegen_expr env body in
    let llty = type_op_match tp in
    let local_sym = fresh_symbol (Sym.name sym) in
    let ptr = Ll.Id local_sym in
    emit @@ CfgBuilder.add_alloca (local_sym, llty);
    let current_locals = env.locals in
    let new_locals = Sym.Table.add sym (llty, ptr) current_locals in
    let new_env = {env with locals = new_locals} in
    emit @@ CfgBuilder.add_insn (None, Ll.Store (llty, rhs_val, ptr));
    new_env
  | TAst.ExprStm {expr} ->
    (match expr with 
    | Some expr -> 
      let _ = codegen_expr env expr in
      env
    | None -> env)
  | TAst.CompoundStm {stms} ->
    List.fold_left codegen_stmt env stms
  | TAst.ReturnStm {ret} ->
    let cret = codegen_expr env ret in
    emit @@ CfgBuilder.term_block (Ll.Ret (I64, Some cret));
    env
  
  | TAst.IfThenElseStm {cond; thbr; elbro} ->
    raise Unimplemented


let codegen_stmt_list env stmts = List.fold_left codegen_stmt env stmts




let codegen_prog = raise Unimplemented