module Ast = Ast
module TAst = TypedAst
module Env = Env
module Errors = Errors
module Sym = Symbol
module RunTimeBindings = RunTimeBindings
module B = CfgBuilder
module Ll = Ll
module Semant = Semant

exception Unimplemented (* your code should eventually compile without this exception *)
exception UnexpectedInput of string

let array_type : Ll.ty = Ll.Struct [Ll.I64; Ll.Array (0, Ll.I8)]
let array_type_name = Sym.symbol "array_type"
let array_type : Ll.ty = Ll.Namedt array_type_name


let array_type_of_length n : Ll.ty = Ll.Struct [Ll.I64; Ll.Array (n, Ll.I8)]



type loops = {
  break_label: Sym.symbol;
  continue_label: Sym.symbol;
}

type cg_env = 
  { cfgb: CfgBuilder.cfg_builder ref
  ; locals: (Ll.ty * Ll.operand) Sym.Table.t
  ; loop: loops list
  ; str_constants: (string, Ll.gid) Hashtbl.t
  ; gdecls: (Ll.gid * Ll.gdecl) list ref }


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
  | Bool -> Ll.I1
  | ErrorType -> raise @@ UnexpectedInput "Not void/int/bool type!"
  | Str -> Ll.Ptr array_type  (* Use the named type here *)
  


let type_of_expr (expr : TAst.expr) : Ll.ty =
  match expr with
  | Integer _ -> I64
  | Boolean _ -> I1
  | BinOp { tp; _ } -> type_op_match tp
  | UnOp { tp; _ } -> type_op_match tp
  | Lval (Var { tp; _ }) -> type_op_match tp
  | Assignment _ -> Void
  | Call { tp; _ } -> type_op_match tp
  | CommaExpr {lhs; rhs; tp} -> type_op_match tp
  | String _ -> Ll.Ptr Ll.I8


(* Codegen for expressions *)
let rec codegen_expr env expr =
  let emit = emit env in
  let emit_insn_with_fresh hint inst = 
    let tmp = fresh_symbol hint in
    emit @@ CfgBuilder.add_insn (Some tmp, inst);
    Ll.Id tmp 
  in
  let cexp = codegen_expr env in
  let rec logic_help left right is_or = 
    let short_circuit_label = fresh_symbol "logic_short_circ" in
    let cond_label = fresh_symbol "logic_next" in
    let end_label = fresh_symbol "logic_end" in
    let left_true, left_false =
      if is_or then short_circuit_label, cond_label else cond_label, short_circuit_label
    in
    let short_circuit = if is_or then Ll.BConst true else Ll.BConst false in  
    let left_op = cexp left in 
    emit @@ CfgBuilder.term_block (Ll.Cbr (left_op, left_true, left_false));
    emit @@ CfgBuilder.start_block short_circuit_label;
    emit @@ CfgBuilder.term_block (Ll.Br end_label);
    emit @@ CfgBuilder.start_block cond_label;
    let right_op, from_label =
      match right with
      | TAst.BinOp {left; op = Lor; right; _} -> logic_help left right true
      | TAst.BinOp {left; op = Land; right; _} -> logic_help left right false
      | _ -> cexp right, cond_label  
    in
    emit @@ CfgBuilder.term_block (Ll.Br end_label);
    emit @@ CfgBuilder.start_block end_label;
    let tmp = fresh_symbol "logic_res" in
    emit @@ CfgBuilder.add_insn (Some tmp, Ll.PhiNode (I1, [right_op, from_label; short_circuit, short_circuit_label]));
    Ll.Id tmp, end_label
  in
  match expr with
  | TAst.Integer {int} -> Ll.IConst64 int
  | TAst.Boolean {bool} -> Ll.BConst bool
  | TAst.BinOp { left; op = Lor; right; _ } -> fst @@ logic_help left right true
  | TAst.BinOp { left; op = Land; right; _ } -> fst @@ logic_help left right false
  | TAst.BinOp {left; op; right; _} -> (
    let cleft = codegen_expr env left in
    let cright = codegen_expr env right in
    let ltyp = type_of_expr left in
    let rtyp = type_of_expr right in
    match op with 
    | TAst.Plus | TAst.Minus | TAst.Mul | TAst.Div | TAst.Rem ->
      emit_insn_with_fresh "temp_name" @@ Ll.Binop (binop_op_match op, Ll.I64, cleft, cright)
    | TAst.Lt | TAst.Le | TAst.Gt | TAst.Ge -> 
      emit_insn_with_fresh "temp_name" @@ Ll.Icmp (comparison_op_match op, Ll.I64, cleft, cright)
    | TAst.Eq | TAst.NEq ->
        if ltyp = Ll.Ptr array_type && rtyp = Ll.Ptr array_type then
            (* String equality *)
            let call_inst = Ll.Call (Ll.I1, Ll.Gid (Sym.symbol "compare_strings"), [
            (Ll.Ptr (Ll.Struct [Ll.I64; Ll.Array (0, Ll.I8)]), cleft);
            (Ll.Ptr (Ll.Struct [Ll.I64; Ll.Array (0, Ll.I8)]), cright)
            ]) in
            let eq_res = emit_insn_with_fresh "streq" call_inst in
            if op = TAst.Eq then
                eq_res
            else
                (* For NEq, invert the result *)
                emit_insn_with_fresh "neq" @@ Ll.Icmp (Ll.Eq, Ll.I1, eq_res, Ll.BConst false)
        else if ltyp = Ll.I64 && rtyp = Ll.I64 then
            (* Integer comparison *)
            let cond = comparison_op_match op in
            emit_insn_with_fresh "icmp" @@ Ll.Icmp (cond, Ll.I64, cleft, cright)
        else
            raise @@ UnexpectedInput "Invalid types for equality operator"
        
    | _ -> raise Unimplemented)
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
  | TAst.Assignment {lvl = Var {ident = Ident {sym}; _}; rhs; _} -> (
    let crhs = codegen_expr env rhs in
    match Sym.Table.find_opt sym env.locals with
    | Some (llty, llop) ->
      emit @@ CfgBuilder.add_insn (None, Ll.Store (llty, crhs, llop));
      crhs
    | None -> raise @@ UnexpectedInput "Variable not found"
  )
  | TAst.CommaExpr {lhs; rhs; tp} -> (
    let _ = codegen_expr env lhs in
    let rhs = codegen_expr env rhs in
    rhs
  )
  | TAst.Call {fname = Ident { sym }; args; tp} -> (
    let llty = type_op_match tp in
    let carglist = List.map (fun arg -> 
      let carg = codegen_expr env arg in
      let carg_ty = type_of_expr arg in
      (carg_ty, carg)) args in
    match llty with
    | Ll.Void ->
        emit @@ CfgBuilder.add_insn (None, Ll.Call (llty, Ll.Gid sym, carglist));
        Null
    | _ ->
        emit_insn_with_fresh "call" @@ Ll.Call (llty, Ll.Gid sym, carglist)
  )
  | TAst.String {str} ->
    let gid =
      try Hashtbl.find env.str_constants str
      with Not_found ->
        let gid = fresh_symbol "str" in
        let str_bytes = str in 
        let str_len = String.length str_bytes in
        let ty = array_type_of_length str_len in
        let gdecl = (ty, Ll.GStruct [
          (Ll.I64, Ll.GInt (str_len));
          (Ll.Array (str_len, Ll.I8), Ll.GString str_bytes)
        ]) in
        Hashtbl.add env.str_constants str gid;
        env.gdecls := (gid, gdecl) :: !(env.gdecls);
        gid
    in
    let str_ptr = emit_insn_with_fresh "str_bitcast" @@
      Ll.Bitcast (Ll.Ptr (array_type_of_length (String.length str)), Ll.Gid gid, Ll.Ptr (array_type))
    in
    str_ptr
  
  | _ -> raise Unimplemented


let rec codegen_stmt env stm = 
  let emit = emit env in
  match stm with
  | TAst.VarDeclStm (DeclBlock decls) ->
    List.fold_left (fun env (TAst.Declaration {name = Ident {sym}; tp; body}) ->
      let rhs_val = codegen_expr env body in
      let llty = type_op_match tp in
      let local_sym = fresh_symbol (Sym.name sym) in
      let ptr = Ll.Id local_sym in
      (*emit @@ CfgBuilder.add_alloca (local_sym, llty);*)
      emit @@ CfgBuilder.add_alloca (local_sym, Ll.Ptr array_type);
      let current_locals = env.locals in
      let new_locals = Sym.Table.add sym (llty, ptr) current_locals in
      let new_env = { env with locals = new_locals } in
      emit @@ CfgBuilder.add_insn (None, Ll.Store (llty, rhs_val, ptr));      
      new_env
    )
    env decls

  | TAst.ExprStm {expr} ->
    (match expr with 
    | Some expr -> 
      let _ = codegen_expr env expr in
      env
    | None -> env)
  | TAst.CompoundStm {stms} ->
    let original_locals = env.locals in
    let inner_env = { env with locals = original_locals } in
    let final_env = List.fold_left codegen_stmt inner_env stms in
    (* Restore original locals after compound block *)
    { final_env with locals = original_locals }
  | TAst.ReturnStm {ret} ->
    let cret = codegen_expr env ret in
    emit @@ CfgBuilder.term_block (Ll.Ret (I64, Some cret));
    env
  | TAst.IfThenElseStm {cond; thbr; elbro} ->
    
    let then_block = fresh_symbol "then" in
    let else_block = fresh_symbol "else" in
    let final_block = fresh_symbol "final" in
    let cond_op = codegen_expr env cond in
    let ll_sym = fresh_symbol ("condSym") in
    
    let b = CfgBuilder.add_alloca (ll_sym, Ll.I1) in (* Can only ever be boolean *)
    emit b;
    
    emit @@ CfgBuilder.add_insn (None, Ll.Store (Ll.I1, cond_op, Ll.Id ll_sym));
    let cond_val = fresh_symbol ("condValue") in
    emit @@ CfgBuilder.add_insn (Some cond_val, Ll.Load (Ll.I1, Ll.Id ll_sym));

    emit @@ CfgBuilder.term_block (Ll.Cbr (Ll.Id cond_val, then_block, else_block));
    

    emit @@ CfgBuilder.start_block then_block;
    let env_after = codegen_stmt env thbr in
    emit @@ CfgBuilder.term_block (Ll.Br (final_block));
    
    let env_after =
      match elbro with
      | Some else_branch -> 
        emit @@ CfgBuilder.start_block else_block;
        codegen_stmt env else_branch;
      | None -> env
      in
    emit @@ CfgBuilder.term_block (Ll.Br (final_block));
    emit @@ CfgBuilder.start_block final_block;
    env_after
  | TAst.BreakStm -> 
    emit @@ CfgBuilder.term_block (Ll.Br (List.hd env.loop).break_label);
    emit @@ CfgBuilder.start_block (fresh_symbol "post_break");
    env

  | TAst.ContinueStm -> 
    emit @@ CfgBuilder.term_block (Ll.Br (List.hd env.loop).continue_label);
    emit @@ CfgBuilder.start_block (fresh_symbol "post_continue");
    env

  | TAst.WhileStm {cond; body} ->
    let continue_label = fresh_symbol "while_continue" in
    let body_block = fresh_symbol "while_body" in
    let break_label = fresh_symbol "exit_while_loop" in
    emit @@ CfgBuilder.term_block (Ll.Br continue_label);
    emit @@ CfgBuilder.start_block continue_label;
    let cond_val = codegen_expr env cond in
    emit @@ CfgBuilder.term_block (Ll.Cbr (cond_val, body_block, break_label));
    emit @@ CfgBuilder.start_block body_block;
    let new_env = {env with loop = { break_label; continue_label } :: env.loop} in

    let a = codegen_stmt new_env body in
    emit @@ CfgBuilder.term_block (Ll.Br continue_label);
    emit @@ CfgBuilder.start_block break_label;
    env

  | TAst.ForStm {init; cond; update; body} ->
    let continue_label = fresh_symbol "for_continue" in
    let body_block = fresh_symbol "for_body" in
    let break_label = fresh_symbol "exit_for_loop" in

    let env_after_init = 
      match init with
      | Some (FIExpr expr) -> 
          let _ = codegen_expr env expr in
          env
      | Some (FIDecl DeclBlock decls) -> 
          let decl_statements = List.map (fun decl -> TAst.VarDeclStm (DeclBlock [decl])) decls in
          List.fold_left codegen_stmt env decl_statements
      | None -> 
          env
    in
    emit @@ CfgBuilder.term_block (Ll.Br continue_label);
    emit @@ CfgBuilder.start_block continue_label;
    let cond_val = match cond with
      | Some expr -> codegen_expr env_after_init expr
      | None -> Ll.BConst true
    in
    emit @@ CfgBuilder.term_block (Ll.Cbr (cond_val, body_block, break_label));
    emit @@ CfgBuilder.start_block body_block;
    let new_env = {env_after_init with loop = { break_label; continue_label } :: env.loop} in
    let env_after_body = codegen_stmt new_env body in

    let env_after_update = match update with
      | Some expr -> 
          let _ = codegen_expr env_after_body expr in
          env_after_body
      | None -> env_after_body
    in
    emit @@ CfgBuilder.term_block (Ll.Br continue_label);
    emit @@ CfgBuilder.start_block break_label;
    env

let codegen_stmt_list env stmts = List.fold_left codegen_stmt env stmts

let getNameOfFunc (TAst.Ident {sym}) = Symbol.name sym

let rec codegen_param (env: cg_env) (p : TAst.param) : Ll.uid * Ll.ty * cg_env=
  let emit = emit env in
  match p with 
  | TAst.Param {paramname = TAst.Ident {sym}; typ} ->
    let name_param = Symbol.name sym in 
    let name_ll = fresh_symbol (name_param ^ "_param") in
    let name_var = fresh_symbol (name_param ^ "_var" ) in 
    let typ_ll = type_op_match typ in 
    let _ = emit @@ CfgBuilder.add_alloca (name_var, typ_ll) in
    let new_locals = Sym.Table.add sym (typ_ll, Ll.Id name_var) env.locals in
    let new_env = { env with locals = new_locals } in
    emit @@ CfgBuilder.add_insn (None, Ll.Store (typ_ll, Ll.Id name_ll, Ll.Id name_var));
    name_ll, typ_ll, new_env;;



let codegen_record (record : TAst.record_decl) : Ll.gid * Ll.gdecl =
  match record with
  | { name = TAst.Ident { sym }; fields } ->
    let record_name = Sym.symbol (Symbol.name sym) in
    let field_types = List.map (fun (TAst.Param { typ; _ }) -> type_op_match typ) fields in
    let field_initializers = 
      List.map2
        (fun typ init ->
          match typ with
          | Ll.I64 -> (Ll.I64, Ll.GInt 0) 
          | Ll.Ptr _ -> (typ, Ll.GNull) 
          | Ll.I1 -> (Ll.I1, Ll.GInt 0) 
          | _ -> raise @@ UnexpectedInput "Unsupported field type in record")
        field_types field_types
    in
    let struct_type = Ll.Struct field_types in
    (record_name, (struct_type, Ll.GStruct field_initializers))



let codegen_func (func : TAst.func_decl) : Ll.gid * Ll.fdecl * (Ll.gid * Ll.gdecl) list =
  let empty_environment = {
    cfgb = ref CfgBuilder.empty_cfg_builder;
    locals = Sym.Table.empty;
    loop = [];
    str_constants = Hashtbl.create 10;
    gdecls = ref [];
  } in

  (* Codegen parameters *)
  let names, typs, env =
    List.fold_left (fun (ns, ts, env) p ->
      let n, t, e = codegen_param env p in
      (ns @ [n], ts @ [t], e)
    ) ([], [], empty_environment) func.params
  in

  let env = codegen_stmt_list env func.body in

  let cfg = CfgBuilder.get_cfg !(env.cfgb) in
  let fun_name = getNameOfFunc func.fname in
  let fun_name = if fun_name = "main" then "dolphin_fun_main" else fun_name in
  let fdecl : Ll.fdecl = {
    Ll.fty = (typs, type_op_match func.ret_type);
    Ll.param = names;
    Ll.cfg = cfg
  } in
  (Sym.symbol fun_name, fdecl, !(env.gdecls))



(*
let codegen_prog (tprog: TAst.program) =
  let open Ll in
  let func_decls =
    match tprog with
    | TAst.Program funcs -> funcs
  in
  (* Generate code for each function declaration *)
  let fdecls_with_gdecls = List.map codegen_func func_decls in
  let fdecls = List.map (fun (gid, fdecl, _) -> (gid, fdecl)) fdecls_with_gdecls in
  let global_gdecls = List.flatten (List.map (fun (_, _, gdecls) -> gdecls) fdecls_with_gdecls) in
  let extfuns = [
    (Sym.symbol "print_integer", ([I64], Void));
    (Sym.symbol "read_integer", ([], I64));
    (Sym.symbol "compare_strings", ([Ll.Ptr array_type; Ll.Ptr array_type], Ll.I64));
  ] in
  {
    tdecls = [(array_type_name, Ll.Struct [Ll.I64; Ll.Array (0, Ll.I8)])];
    extgdecls = [];
    gdecls = global_gdecls;
    extfuns = extfuns;
    fdecls = fdecls;
  }
*)
let codegen_prog (tprog: TAst.program) =
  let open Ll in
  let funcs, records =
    match tprog with
    | TAst.Program globals ->
      List.fold_left
        (fun (funcs, records) elem ->
          match elem with
          | TAst.Function func -> (func :: funcs, records)
          | TAst.Record record -> (funcs, record :: records))
        ([], [])
        globals
  in

  let func_results = List.map codegen_func funcs in
  let fdecls = List.map (fun (gid, fdecl, _) -> (gid, fdecl)) func_results in
  let record_results = List.map codegen_record records in
  let global_gdecls =
    List.flatten (List.map (fun (_, _, gdecls) -> gdecls) func_results) @ record_results
  in

  let extfuns = [
    (Sym.symbol "print_integer", ([I64], Void));
    (Sym.symbol "read_integer", ([], I64));
    (Sym.symbol "compare_strings", ([Ll.Ptr array_type; Ll.Ptr array_type], Ll.I1));
  ] in
  {
    tdecls = [(array_type_name, Ll.Struct [Ll.I64; Ll.Array (0, Ll.I8)])];
    extgdecls = [];
    gdecls = global_gdecls;
    extfuns = extfuns;
    fdecls = fdecls;
  }







