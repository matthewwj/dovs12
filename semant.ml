module Ast = Ast
module TAst = TypedAst
module Env = Env
module Errors = Errors
module Sym = Symbol
module RunTimeBindings = RunTimeBindings

exception Unimplemented (* your code should eventually compile without this exception *)
exception UnimpRtrError (* temporary exception for no return error *)

let typecheck_typ = function
  | Ast.Int _loc -> TAst.Int
  | Ast.Bool _loc -> TAst.Bool

let typecheck_op op =
  match op with
  | Ast.Plus _loc -> TAst.Plus
  | Ast.Minus _loc -> TAst.Minus
  | Ast.Mul _loc -> TAst.Mul
  | Ast.Div _loc -> TAst.Div
  | Ast.Rem _loc -> TAst.Rem
  | Ast.Lt _loc -> TAst.Lt
  | Ast.Le _loc -> TAst.Le
  | Ast.Gt _loc -> TAst.Gt
  | Ast.Ge _loc -> TAst.Ge
  | Ast.Lor _loc -> TAst.Lor
  | Ast.Land _loc -> TAst.Land
  | Ast.Eq _loc -> TAst.Eq
  | Ast.NEq _loc -> TAst.NEq

let rec safe_zip l1 l2 =
  match l1, l2 with
  | a :: an, b :: bn -> (a, b) :: safe_zip an bn
  | [], [] -> []
  | [], _ | _, [] -> failwith "safe_zip: Lists have different lengths"

(* should return a pair of a typed expression and its inferred type. you can/should use typecheck_expr inside infertype_expr. *)
let rec infertype_expr env expr =
  match expr with
  | Ast.Integer {int; _} ->
    (TAst.Integer {int}, TAst.Int)
  
  | Ast.Boolean {bool; _} ->
    (TAst.Boolean {bool}, TAst.Bool)
  
  | Ast.BinOp {left; op; right; loc} ->
    let (left_expr, left_type) = infertype_expr env left in
    let (right_expr, right_type) = infertype_expr env right in
    let optyp = typecheck_op op in
    (match op with
    | Ast.Plus _ | Ast.Minus _ | Ast.Mul _ | Ast.Div _ | Rem _ ->
      if left_type = TAst.Int && right_type = TAst.Int then
        (TAst.BinOp {left = left_expr; op = optyp; right = right_expr; tp = TAst.Int}, TAst.Int)
      else
        raise (Invalid_argument (Errors.error_to_string (Errors.TypeMismatch {expected = TAst.Int; actual = right_type; loc = loc})))
    | Lt _ | Le _ | Gt _ | Ge _ ->
      if left_type = TAst.Int && right_type = TAst.Int then
        (TAst.BinOp {left = left_expr; op = optyp; right = right_expr; tp = TAst.Bool}, TAst.Bool)
      else
        raise (Invalid_argument (Errors.error_to_string (Errors.TypeMismatch {expected = left_type; actual = right_type; loc = loc})))
    | Eq _ | NEq _ ->
      if left_type = right_type then
        (TAst.BinOp {left = left_expr; op = optyp; right = right_expr; tp = TAst.Bool}, TAst.Bool)
      else
        raise (Invalid_argument (Errors.error_to_string (Errors.TypeMismatch {expected = left_type; actual = right_type; loc = loc})))
    | Lor _ | Land _ ->
      if left_type = TAst.Bool && right_type = TAst.Bool then
        (TAst.BinOp {left = left_expr; op = optyp; right = right_expr; tp = TAst.Bool}, TAst.Bool)
      else
        raise (Invalid_argument (Errors.error_to_string (Errors.TypeMismatch {expected = left_type; actual = right_type; loc = loc})))
    )
  | Ast.UnOp {op; operand; loc} ->
    let (operand_expr, operand_type) = infertype_expr env operand in
    let optyp = match op with
      | Ast.Neg _ when operand_type = TAst.Int -> TAst.Neg
      | Ast.Lnot _ when operand_type = TAst.Bool -> TAst.Lnot
      | _ -> raise (Invalid_argument (Errors.error_to_string (Errors.TypeMismatch {expected = operand_type; actual = operand_type; loc = loc})))
    in
    (TAst.UnOp {op = optyp; operand = operand_expr; tp = operand_type}, operand_type)
  | Ast.Lval lvl -> infertype_lval env lvl
  | Ast.Assignment {lvl; rhs; loc} ->
    let _, lvalType = infertype_lval env lvl in
    let rhsExpr = typecheck_expr env rhs lvalType loc in
    let lvlType : TAst.lval =
      match lvl with
      | Ast.Var (Ident {name; _}) ->
        TAst.Var {ident = TAst.Ident {sym = Symbol.symbol name}; tp = lvalType}
    in
    (TAst.Assignment {lvl = lvlType; rhs = rhsExpr; tp = lvalType}, lvalType)
  | Ast.Call {fname = Ident {name; _}; args; loc} ->
    let sym = Sym.symbol name in
    (* Lookup the function in the environment *)
    (match Env.lookup_var_fun env sym with
    | Some (Env.Fun (TAst.FunTyp {ret; params})) ->
      let param_len = List.length params in
      let args_len = List.length args in
      if param_len < args_len then
        raise (Invalid_argument (Errors.error_to_string (Errors.TooManyArguments {expected = param_len; actual = args_len; loc = loc})));
      if param_len > args_len then
        raise (Invalid_argument (Errors.error_to_string (Errors.MissingArguments {expected = param_len; actual = args_len; loc = loc})));
      let param_types = List.map (fun (TAst.Param {typ; _}) -> typ) params in
      let args_types = safe_zip args param_types in
      let type_args = List.map (fun (a, t) -> typecheck_expr env a t loc (*SUSSSSSSSSSSSSSSSSS*)) args_types in
      (TAst.Call {fname = TAst.Ident {sym}; args = type_args; tp = ret}, ret)
    | None ->
      raise (Invalid_argument (Errors.error_to_string (Errors.UndeclaredVariable {variablename = name; loc = loc})))
    | _ ->
      raise (Invalid_argument (Errors.error_to_string (Errors.NotAFunction {name; loc = loc})))
    )

and infertype_lval env lvl =
  match lvl with
  | Ast.Var (Ident {name; loc}) ->
    let varOpType = Env.lookup_var_fun env (Sym.symbol name) in
    let identName = TAst.Ident {sym = Sym.symbol name} in
    (match varOpType with
    | None ->
      raise (Invalid_argument (Errors.error_to_string (Errors.UndeclaredVariable {variablename = name; loc = loc})))
    | Some varOrFun ->
      (match varOrFun with
      | Env.Var typ -> (TAst.Lval (TAst.Var {ident = identName; tp = typ}), typ)
      | _ -> raise Unimplemented
      )
    )

and typecheck_expr env expr tp loc =
  let texpr, texprtp = infertype_expr env expr in
  if texprtp <> tp then
    raise (Invalid_argument (Errors.error_to_string (Errors.TypeMismatch {expected = tp; actual = texprtp; loc = loc})))
  else texpr

(* should check the validity of a statement and produce the corresponding typed statement. Should use typecheck_expr and/or infertype_expr as necessary. *)
let rec typecheck_statement env (stm : Ast.statement) : TAst.statement * Env.environment =
  match stm with
  | Ast.VarDeclStm (DeclBlock {declarations = decls; _}) -> 
    let typechecked_decls, updated_env =
      List.fold_left (fun (t_decls, current_env) decl ->
      match decl with
      | Ast.Declaration {name = Ast.Ident {name = sname; _}; tp; body; loc} ->
        let tname = TAst.Ident {sym = Sym.symbol sname} in
        let typeBody, inferred_type = 
        match tp with
          | None -> infertype_expr current_env body
          | Some expected_type -> 
            let body_type = typecheck_typ expected_type in
            let typechecked_body = typecheck_expr current_env body body_type loc in
            typechecked_body, body_type
        in
        let new_decl = TAst.Declaration {name = tname; tp = inferred_type; body = typeBody} in
        let updated_env = Env.insert_local_decl current_env (Sym.symbol sname) inferred_type in
        (new_decl :: t_decls, updated_env))
        ([], env) decls in
    (TAst.VarDeclStm (DeclBlock (List.rev typechecked_decls)), updated_env)
    
  | Ast.ExprStm {expr; _} ->
    (match expr with
    | Some expr ->
      let typed_expr, _ = infertype_expr env expr in
      (TAst.ExprStm {expr = Some typed_expr}, env)
    | None ->
      (TAst.ExprStm {expr = None}, env))

  | Ast.IfThenElseStm {cond; thbr; elbro; loc} ->
    let typed_cond = typecheck_expr env cond Bool loc in
    let typedThbr, _ = typecheck_statement env thbr in
    let tElbro =
      match elbro with
      | None -> None
      | Some branch ->
        let tElbr, _ = typecheck_statement env branch in
        Some tElbr
    in
    TAst.IfThenElseStm {cond = typed_cond; thbr = typedThbr; elbro = tElbro}, env

  | Ast.CompoundStm {stms; _} ->
    let type_stmts, _ =
      List.fold_left (fun (tsmts_now, env_now) stmt ->
        let tstmt, new_env = typecheck_statement env_now stmt in
        tstmt :: tsmts_now, new_env)
      ([], env)
      stms
    in
    TAst.CompoundStm {stms = List.rev type_stmts}, env

  | Ast.ReturnStm {ret; loc} ->
    let typed_ret, ret_type = infertype_expr env ret in
    if ret_type != Int then raise (Invalid_argument (Errors.error_to_string (Errors.UnexpectedReturnType {actual = ret_type; expected = Int; loc = loc})));
    (TAst.ReturnStm {ret = typed_ret}, env)
  
  | Ast.WhileStm {cond; body; loc} -> 
    let typed_while_cond = typecheck_expr env cond Bool loc in
    let env_in_loop = { env with loop = env.loop + 1 } in
    let typed_while_body, _ = typecheck_statement env_in_loop body in
    
    (TAst.WhileStm {cond = typed_while_cond; body = typed_while_body}, env)
    
  | Ast.ForStm { init; cond; update; body; loc} ->
    let tInit, temp_env =
      match init with
      | Some (Ast.FIDecl (DeclBlock {declarations = decls; _})) ->
        let rec process_decls env decls typed_decls =
          match decls with
          | [] -> List.rev typed_decls, env
          | decl :: rest ->
            match decl with
            | Ast.Declaration { name = Ident { name = sname; _ }; tp; body; loc } ->
              let sym = Sym.symbol sname in
              let tname = TAst.Ident { sym = sym } in
              let t_body, inferred_type =
                match tp with
                | Some btp ->
                    let expected_type = typecheck_typ btp in
                    let t_body = typecheck_expr env body expected_type loc in
                    t_body, expected_type
                | None ->
                    infertype_expr env body
              in
              let new_env = Env.insert_local_decl env sym inferred_type in
              let t_decl = TAst.Declaration { name = tname; tp = inferred_type; body = t_body } in
              process_decls new_env rest (t_decl :: typed_decls)
        in
        let typed_decls, new_env = process_decls env decls [] in
        Some (TAst.FIDecl (TAst.DeclBlock typed_decls)), new_env
      | Some (Ast.FIExpr expr) ->
        let exp = fst (infertype_expr env expr) in
        Some (TAst.FIExpr exp), env
      | None -> None, env
    in
    let tCond = Option.map (fun cond -> typecheck_expr temp_env cond Bool loc) cond in
    let tUpdate = Option.map (fun upd -> fst (infertype_expr temp_env upd)) update in
    let temp_env = { temp_env with loop = env.loop + 1 } in
    let tBody = fst (typecheck_statement temp_env body) in
    TAst.ForStm { init = tInit; cond = tCond; update = tUpdate; body = tBody }, env

  | Ast.BreakStm {loc}->
    if env.loop < 1 then raise (Invalid_argument (Errors.error_to_string (Errors.UnexpectedBreak {loc})));
    TAst.BreakStm, env

  | Ast.ContinueStm {loc} ->
    if env.loop < 1 then raise (Invalid_argument (Errors.error_to_string (Errors.UnexpectedContinue {loc})));
    TAst.ContinueStm, env
  

(* should use typecheck_statement to check the block of statements. *)
and typecheck_statement_seq env stms =
  let type_stmts, _ =
    List.fold_left (fun (tsmts_now, env_now) stmt ->
      let tstmt, new_env = typecheck_statement env_now stmt in
      tstmt :: tsmts_now, new_env)
    ([], env)
    stms
  in
  List.rev type_stmts, env

(* the initial environment should include all the library functions, no local variables, and no errors. *)
(*let initial_environment = raise Unimplemented*)

(* this method will check if the given stm is a return. this is used to check the last stm of a program. *)
let return_check stm =
  match stm with
  | TAst.ReturnStm _ -> 1
  | _ -> raise UnimpRtrError

(* should check that the program (sequence of statements) ends in a return statement and make sure that all statements are valid as described in the assignment. Should use typecheck_statement_seq. *)
let typecheck_prog prg : TAst.program * Errors.error list =
  let env = Env.make_env RunTimeBindings.library_functions in
  begin
    match List.rev prg with
    | Ast.ReturnStm _ :: _ -> ()
    | _ -> raise (Invalid_argument (Errors.error_to_string Errors.NotEndInRet))
  end;
  let tProg, env_res = typecheck_statement_seq env prg in
  tProg, !(env_res.errors)
