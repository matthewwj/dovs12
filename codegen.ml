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

let ll_string_type = Ll.Ptr (Ll.Namedt (Sym.symbol "string_type"))
let ll_string_type_name = Sym.symbol "string_type"

let ident_to_sym (TAst.Ident { sym }) = sym

let ic32 i = Ll.IConst32 (Int32.of_int i)

type sym_sym_typ = int Sym.Table.t Sym.Table.t

type loops = {
  break_label: Sym.symbol;
  continue_label: Sym.symbol;
}

type cg_env = 
  { cfgb: CfgBuilder.cfg_builder ref
  ; locals: (Ll.ty * Ll.operand) Sym.Table.t
  ; loop: loops list
  ; str_constants: sym_sym_typ (*(string, Ll.gid) Hashtbl.t*)
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

let rec type_op_match (tp : TAst.typ) : Ll.ty = 
  match tp with 
  | Void -> Void 
  | Int -> Ll.I64
  | Bool -> Ll.I1
  | ErrorType -> raise @@ UnexpectedInput "Not void/int/bool type!"
  | Str -> ll_string_type
  | Int8 -> Ll.I8 
  | Struct s -> Ll.Ptr (Ll.Namedt (Sym.symbol s))
  | Ptr Void -> Ll.Ptr Ll.I8
  | Ptr e -> Ll.Ptr (type_op_match e)
  | Array e -> Ll.Ptr (type_op_match e)
  | NilType -> Ll.Void
  | Int32 -> Ll.I32

let struct_tp_to_sym tp =
  match tp with
  | TAst.Struct s -> s
  | TAst.Ptr (TAst.Struct s) -> s
  | _ -> raise Unimplemented


let without_ptr e =
  match e with
  | Ll.Ptr a -> a
  | _ -> failwith "not a pointer"

let rec type_of_expr (expr : TAst.expr) : Ll.ty =
  match expr with
  | Integer _ -> Ll.I64
  | Boolean _ -> Ll.I1
  | BinOp { tp; _ } -> type_op_match tp
  | UnOp { tp; _ } -> type_op_match tp
  | Lval (Idx { arr; tp; _ }) ->
      (* Get the element type for the array *)
      (match type_of_expr arr with
       | Ll.Ptr (Ll.Array (_, elem_ty)) -> elem_ty
       | Ll.Ptr elem_ty -> elem_ty
       | other -> raise @@ UnexpectedInput ("Invalid array type: " ^ Ll.string_of_ty other))
  | Lval (Var { tp; _ }) -> type_op_match tp
  | Lval (Fld { tp; _ }) -> type_op_match tp
  | Assignment _ -> Ll.Void
  | Call { tp; _ } -> type_op_match tp
  | CommaExpr { tp; _ } -> type_op_match tp
  | NewExpr { tp; _ } -> type_op_match tp
  | String _ -> ll_string_type
  | LengthOf _ -> Ll.I64
  | Nil { typ } -> type_op_match typ



(* Codegen for expressions *)
let rec codegen_expr env expr =
  let emit = emit env in
  let emit_insn_with_fresh hint inst = 
    let tmp = fresh_symbol hint in
    emit @@ CfgBuilder.add_insn (Some tmp, inst);
    Ll.Id tmp 
  in
  let ( >> ) a b = emit_insn_with_fresh a b in
  let cexp = codegen_expr env in
  (* Define codegen_lval *)
  let rec codegen_lval lval =
    match lval with
    | TAst.Var { ident = Ident { sym }; tp } -> (
        let llty = type_op_match tp in
        match Sym.Table.find_opt sym env.locals with
        | Some (_, llop) -> (llty, llop)
        | None -> raise @@ UnexpectedInput "Variable not found"
      )
    | TAst.Idx { arr; index; tp } -> (
        let arr_op = codegen_expr env arr in
        let index_op = codegen_expr env index in
        match type_of_expr arr with
        | Ll.Ptr (Ll.Array (n, elem_ty)) ->
            (* Statically sized array *)
            let ptr =
              emit_insn_with_fresh "gep_idx"
              @@ Ll.Gep (Ll.Array (n, elem_ty), arr_op, [ Ll.IConst32 0l; index_op ])
            in
            (elem_ty, ptr)
        | Ll.Ptr elem_ty ->
            (* Dynamically allocated array *)
            let ptr =
              emit_insn_with_fresh "gep_idx"
              @@ Ll.Gep (Ll.Ptr elem_ty, arr_op, [ index_op ])
            in
            (elem_ty, ptr)
        | other -> raise @@ UnexpectedInput ("Invalid array type: " ^ Ll.string_of_ty other)
      )    
    | TAst.Fld { rcrd; field = Ident { sym = field_sym }; tp; rcrd_tp } -> (
        let rcrd_op = codegen_expr env rcrd in
        let rcrd_ty = type_of_expr rcrd in
        let elem_ty = type_op_match tp in
        let field_index =
          match Sym.Table.find_opt (Sym.symbol rcrd_tp) env.str_constants with
          | Some field_table -> (
              match Sym.Table.find_opt field_sym field_table with
              | Some idx -> idx
              | None -> raise @@ UnexpectedInput "Field not found in record"
            )
          | None -> raise @@ UnexpectedInput "Record type not found"
        in
        let ptr =
          emit_insn_with_fresh "gep_fld"
          @@ Ll.Gep
               (rcrd_ty, rcrd_op, [ Ll.IConst32 0l; Ll.IConst32 (Int32.of_int field_index) ])
        in
        (elem_ty, ptr)
      )
  in
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
        if ltyp = ll_string_type && rtyp = ll_string_type then
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
  | TAst.Lval lval -> (
      let (llty, ptr) = codegen_lval lval in
      emit_insn_with_fresh "load" @@ Ll.Load (llty, ptr)
    )
    | TAst.Assignment { lvl; rhs; _ } -> (
      let crhs = codegen_expr env rhs in
      let (llty, ptr) = codegen_lval lvl in
      emit @@ CfgBuilder.add_insn (None, Ll.Store (llty, crhs, ptr));
      crhs
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
  | TAst.String { str } ->
    let size = String.length str in
    let gSym = fresh_symbol "string_" in
    let gStruct = fresh_symbol "string_struct_" in
    let arr = Ll.Array (size, Ll.I8) in
    let gString = gSym, (arr, Ll.GString str) in
    let strStruct =
      ( gStruct
      , ( Ll.Struct [ I64; Ptr arr ]
        , Ll.GStruct [ Ll.I64, Ll.GInt size; Ll.Ptr arr, Ll.GGid gSym ] ) )
    in
    env.gdecls := !(env.gdecls) @ [ gString; strStruct ];
    "bitcast"
    >> Ll.Bitcast
         ( Ll.Ptr (Ll.Struct [ I64; Ll.Ptr (Ll.Array (size, I8)) ])
         , Ll.Gid gStruct
         , Ll.Ptr (Ll.Namedt (Symbol.symbol "string_type")) )
  
  | TAst.Nil _ -> Null

  | TAst.NewExpr { tp; obj } ->
    let open Ll in
    let typ = type_op_match tp in
    (match obj with
     | TAst.Record { fields } ->
       let typ = without_ptr typ in
       let typ_name = struct_tp_to_sym tp in
       let obj_table = Sym.Table.find (Symbol.symbol typ_name) env.str_constants in
       let size_ptr = "size_ptr" >> Gep (typ, Null, [ ic32 1 ]) in
       let size = "size" >> Ptrtoint (typ, size_ptr, I32) in
       let alloc =
         "malloc_ptr" >> Call (Ptr I8, Gid (Symbol.symbol "allocate_record"), [ I32, size ])
       in
       let bitcast = typ_name ^ "_ptr" >> Bitcast (Ptr I8, alloc, Ptr typ) in
       (*set values*)
       let _ =
         List.map
           (fun ({ name; expr; ty } : TAst.field) ->
             let v = codegen_expr env expr in
             let index = Sym.Table.find (ident_to_sym name) obj_table in
             let gep = "gep" >> Ll.Gep (typ, bitcast, [ ic32 0; ic32 index ]) in
             emit @@ CfgBuilder.add_insn (None, Store (type_op_match ty, v, gep));
             ())
           fields
       in
       bitcast
       | TAst.Array { size } ->
        let size_ptr = "size_ptr" >> Gep (typ, Null, [ IConst64 1L ]) in
        let size_elem = "size" >> Ptrtoint (typ, size_ptr, I32) in
        let size = codegen_expr env size in
        let def_val =
          match tp with
          | TAst.Str -> Gid (Sym.symbol "dolphin_rc_empty_string")
          | TAst.Struct _ | TAst.Ptr _ | TAst.Array _ -> Null
          | _ -> Ll.IConst64 0L
        in
        let default_value = "default_val_" >> Ll.Alloca typ in
        let _ =
          emit @@ CfgBuilder.add_insn (None, Ll.Store (typ, def_val, default_value))
        in
        let bitcast_default_value =
          "bitcast" >> Ll.Bitcast (Ptr typ, default_value, Ptr I8)
        in
        let alloc =
          "malloc_ptr"
          >> Call
               ( Ptr I8
               , Gid (Sym.symbol "allocate_array")
               , [ I32, size_elem; I64, size; Ptr I8, bitcast_default_value ] )
        in
        let bitcast = "bitcast" >> Ll.Bitcast (Ptr I8, alloc, Ptr typ) in
        bitcast)     
  | TAst.LengthOf e ->
    let op = cexp e.expr in
    (match e.tp_expr with
     | TAst.Str ->
       "str_size" >> Ll.Call (Ll.I64, Ll.Gid (Symbol.symbol "string_length"), [ type_op_match e.tp_expr, op ])
     | TAst.Array _ ->
       let bitcast = "cast" >> Ll.Bitcast (type_op_match e.tp_expr, op, Ll.Ptr Ll.I64) in
       "arr_len"
       >> Ll.Call
            (Ll.I64, Ll.Gid (Symbol.symbol "dolphin_rc_get_array_length"), [ Ll.Ptr Ll.I64, bitcast ])
     | _ -> failwith "lengthof not arr or string - doesn't happen")



let rec codegen_stmt env stm = 
  let emit = emit env in
  match stm with
  | TAst.VarDeclStm (DeclBlock decls) ->
    List.fold_left (fun env (TAst.Declaration {name = Ident {sym}; tp; body}) ->
      let rhs_val = codegen_expr env body in
      let llty = type_op_match tp in
      let local_sym = fresh_symbol (Sym.name sym) in
      let ptr = Ll.Id local_sym in
      emit @@ CfgBuilder.add_alloca (local_sym, llty);  (* Use the correct type here *)
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

    let _ = codegen_stmt new_env body in
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



let codegen_record (record : TAst.record_decl) :
  (Sym.symbol * Ll.ty) * (Sym.symbol * int Sym.Table.t) =
  match record with
  | { name = TAst.Ident { sym }; fields } ->
      let record_name = sym in
      let field_types =
        List.map (fun (TAst.Param { typ; _ }) -> type_op_match typ) fields
      in
      let field_names =
        List.map (fun (TAst.Param { paramname = TAst.Ident { sym }; _ }) -> sym) fields
      in
      let field_indices =
        List.mapi (fun idx sym -> (sym, idx)) field_names
      in
      let field_table =
        List.fold_left
          (fun acc (sym, idx) -> Sym.Table.add sym idx acc)
          Sym.Table.empty field_indices
      in
      let struct_type = Ll.Struct field_types in
      ((record_name, struct_type), (record_name, field_table))



and codegen_func str_constants (func : TAst.func_decl) :
    Ll.gid * Ll.fdecl * (Ll.gid * Ll.gdecl) list =
  let empty_environment =
    {
      cfgb = ref CfgBuilder.empty_cfg_builder;
      locals = Sym.Table.empty;
      loop = [];
      str_constants = str_constants;
      gdecls = ref [];
    }
  in

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


let codegen_prog (tprog : TAst.program) =
  let open Ll in
  let funcs, records =
    match tprog with
    | TAst.Program globals ->
        List.fold_left
          (fun (funcs, records) elem ->
            match elem with
            | TAst.Function func -> (func :: funcs, records)
            | TAst.Record record -> (funcs, record :: records))
          ([], []) globals
  in

  (* Process records *)
  let record_results = List.map codegen_record records in
  let type_decls = List.map fst record_results in
  let field_indices = List.map snd record_results in

  (* Build str_constants *)
  let str_constants =
    List.fold_left
      (fun acc (record_sym, field_table) ->
        Sym.Table.add record_sym field_table acc)
      Sym.Table.empty field_indices
  in

  (* Process functions *)
  let func_results =
    List.map (codegen_func str_constants) funcs
  in

  let fdecls = List.map (fun (gid, fdecl, _) -> (gid, fdecl)) func_results in
  let global_gdecls =
    List.flatten (List.map (fun (_, _, gdecls) -> gdecls) func_results)
  in

  (* Combine all type declarations *)
  let tdecls = (ll_string_type_name, Ll.Struct [Ll.I64; Ll.Array (0, Ll.I8)]) :: type_decls in

  let extfuns = [
  (Sym.symbol "print_integer", ([I64], Void));
  (Sym.symbol "read_integer", ([], I64));
  (Sym.symbol "compare_strings", ([ll_string_type; ll_string_type], Ll.I1));
  (Sym.symbol "allocate_record", ([I32], Ll.Ptr Ll.I8));
  (Sym.symbol "allocate_array", ([I32; I64; Ll.Ptr Ll.I8], Ll.Ptr Ll.I8));
  (Sym.symbol "string_length", ([Ll.Ptr ll_string_type], Ll.I64)); 
  ] in

  {
    tdecls = tdecls;
    extgdecls = [Sym.symbol "dolphin_rc_empty_string", Ll.Namedt( Sym.symbol "string_type")];
    gdecls = global_gdecls;
    extfuns = extfuns;
    fdecls = fdecls;
  }








