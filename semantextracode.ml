(*module Ast = Ast
module TAst = TypedAst
module Env = Env
module Errors = Errors
module Sym = Symbol

exception Unimplemented (* your code should eventually compile without this exception *)
exception UnimpRtrError (* temporary exception for no return error *)

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
    let _, lvalType = infertype_lval env lvl in 
    let rhsExpr = typecheck_expr env rhs lvalType in
    let lvlType : TAst.lval =
      match lvl with 
      | Ast.Var (Ident {name}) ->
        TAst.Var { ident = TAst.Ident {sym = Symbol.symbol name}; tp = lvalType}
    in 
    TAst.Assignment {lvl = lvlType; rhs = rhsExpr; tp = lvalType}, lvalType

  | Ast.Call {fname = Ident {name}; args} -> 
    (* Lookup the function in the environment *)
    match Env.lookup_var_fun env name with
    | _ -> raise Unimplemented

and infertype_lval env lvl =
  match lvl with
  | _ -> raise Unimplemented
(* checks that an expression has the required type tp by inferring the type and comparing it to tp. *)
and typecheck_expr env expr tp =
  let texpr, texprtp = infertype_expr env expr in
  if texprtp <> tp then raise (Invalid_argument (Errors.error_to_string (Errors.TypeMismatch{actual = texprtp ; expected = tp})));
  texpr

(* should check the validity of a statement and produce the corresponding typed statement. Should use typecheck_expr and/or infertype_expr as necessary. *)
let rec typecheck_statement env stm =
  match stm with
  | Ast.VarDeclStm {name = Ident {name}; tp; body} -> 
    raise Unimplemented

  

  | Ast.ExprStm {expr} -> 
    (match expr with
    | Some expr ->
      let typed_expr, _ = infertype_expr env expr in
      (TAst.ExprStm {expr = Some typed_expr}, env)
    | None -> 
      (TAst.ExprStm {expr = None}), env)

  | Ast.IfThenElseStm {cond; thbr; elbro} -> 
    let typed_cond, cond_type = infertype_expr env cond in
      if cond_type <> TAst.Bool then
        raise (Invalid_argument (Errors.error_to_string (Errors.TypeMismatch {expected = TAst.Bool; actual = cond_type})));
      let typed_thbr = typecheck_statement env thbr in
      let typed_elbro =
        match elbro with
        | Some el -> Some (typecheck_statement env el)
        | None -> None
      in
      raise Unimplemented
      (*(TAst.IfThenElseStm {cond = typed_cond; thbr = typed_thbr; elbro = typed_elbro}, env)*)


  | Ast.CompoundStm {stms} -> 
    let typed_stms = typecheck_statement_seq env stms in
    (TAst.CompoundStm {stms = typed_stms}, env)

  | Ast.ReturnStm {ret} -> 
    let typed_ret, ret_type = infertype_expr env ret in
    (TAst.ReturnStm {ret = typed_ret}, env)

  | _ -> raise Unimplemented
(* should use typecheck_statement to check the block of statements. *)
and typecheck_statement_seq env stms =
  List.map (typecheck_statement env) stms

(* the initial environment should include all the library functions, no local variables, and no errors. *)
let initial_environment = raise Unimplemented

(*this method will check if the given stm is a return. this is used to check the last stm of an program.*)
let return_check stm = 
  match stm with 
  | Ast.ReturnStm _ -> 1
  | _ -> raise UnimpRtrError

(* should check that the program (sequence of statements) ends in a return statement and make sure that all statements are valid as described in the assignment. Should use typecheck_statement_seq. *)
let typecheck_prog prg =
  (*let prgenv = Env.make_env in
  let _foldres = List.fold_left typecheck_statement prgenv prg in
  let prglength = (List.length prg) - 1 in
  let prgtail = List.nth prg prglength in
  return_check prgtail*)
  raise Unimplemented
  
  


let test =
  [
    Ast.Integer {int = 5L};
  ]
*)

(*
module Ast = Ast
module TAst = TypedAst
module Env = Env
module Errors = Errors
module Sym = Symbol
module RunTimeBindings = RunTimeBindings

exception Unimplemented (* your code should eventually compile without this exception *)
exception UnimpRtrError (* temporary exception for no return error *)

let typecheck_typ = function
  | Ast.Int -> TAst.Int
  | Ast.Bool -> TAst.Bool

let typecheck_op op = 
  match op with 
  | Ast.Plus -> TAst.Plus
  | _ -> raise Unimplemented

let rec safe_zip l1 l2 =
  match l1, l2 with
  | a :: an, b :: bn -> (a, b) :: safe_zip an bn
  | [], [] -> []
  | [], _ | _, [] -> failwith "safe_zip: Lists have different lengths"

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
      | _ -> raise (Invalid_argument (Errors.error_to_string (Errors.TypeMismatch {expected = operand_type; actual = operand_type}))) 
    in
    (TAst.UnOp {op = optyp; operand = operand_expr; tp = operand_type}, operand_type)

  | Ast.Lval lvl -> infertype_lval env lvl

  | Ast.Assignment {lvl; rhs} -> 
    let _, lvalType = infertype_lval env lvl in 
    let rhsExpr = typecheck_expr env rhs lvalType in
    let lvlType : TAst.lval =
      match lvl with 
      | Ast.Var (Ident {name}) ->
        TAst.Var { ident = TAst.Ident {sym = Symbol.symbol name}; tp = lvalType}
    in 
    (TAst.Assignment {lvl = lvlType; rhs = rhsExpr; tp = lvalType}, lvalType)

  | Ast.Call {fname = Ident {name}; args} -> 
    (* Lookup the function in the environment *)
    let sym = Sym.symbol name in
    let name_string = Sym.name sym in
    (match Env.lookup_var_fun env name_string with
     | Some (Env.Fun (TAst.FunTyp {ret; params})) ->
       let param_len = List.length params in
       let args_len = List.length args in
       if param_len < args_len then
         raise (Invalid_argument (Errors.error_to_string (Errors.TooManyArguments {expected = param_len; actual = args_len})));
       if param_len > args_len then 
         raise (Invalid_argument (Errors.error_to_string (Errors.MissingArguments {expected = param_len; actual = args_len})));
       let param_types = List.map (fun (TAst.Param {typ; _}) -> typ) params in
       let args_types = safe_zip args param_types in
       let type_args = List.map (fun (a, t) -> typecheck_expr env a t) args_types in 
       (TAst.Call {fname = TAst.Ident {sym}; args = type_args; tp = ret}, ret)
     | None ->
       raise (Invalid_argument (Errors.error_to_string (Errors.UndeclaredVariable {variablename = name})))
     | _ ->
       raise (Invalid_argument (Errors.error_to_string (Errors.NotAFunction {name})))
    )

and infertype_lval env lvl =
  match lvl with
  | Ast.Var (Ident {name}) ->
    let varOpType = Env.lookup_var_fun env name in
    let identName = TAst.Ident {sym = Sym.symbol name} in
    (match varOpType with 
     | None ->
       raise (Invalid_argument (Errors.error_to_string (Errors.UndeclaredVariable {variablename = name})))
     | Some varOrFun ->
       (match varOrFun with
        | Env.Var typ -> (TAst.Lval (TAst.Var {ident = identName; tp = typ}), typ)
        | _ -> raise UnimpRtrError      
       )
    )

and typecheck_expr env expr tp =
  let texpr, texprtp = infertype_expr env expr in
  if texprtp <> tp then
    raise (Invalid_argument (Errors.error_to_string (Errors.TypeMismatch {expected = tp; actual = texprtp})))
  else texpr

(* should check the validity of a statement and produce the corresponding typed statement. Should use typecheck_expr and/or infertype_expr as necessary. *)
let rec typecheck_statement env stm =
  match stm with
  | Ast.VarDeclStm {name = Ast.Ident {name = sname}; tp; body} -> 
    let tname = TAst.Ident {sym = Sym.symbol sname} in
    let typeBody, tbody_type = 
      match tp with 
      | None -> infertype_expr env body
      | Some btp -> typecheck_expr env body (typecheck_typ btp), typecheck_typ btp
    in
    (TAst.VarDeclStm {name = tname; tp = tbody_type; body = typeBody}, 
    Env.insert_local_decl env sname (Env.Var tbody_type)) 

  | Ast.ExprStm {expr} -> 
    (match expr with
    | Some expr ->
      let typed_expr, _ = infertype_expr env expr in
      (TAst.ExprStm {expr = Some typed_expr}, env)
    | None -> 
      (TAst.ExprStm {expr = None}), env)

    | Ast.IfThenElseStm {cond; thbr; elbro} -> 
      let typed_cond = typecheck_expr env cond Bool in 
      let typedThbr, _ = typecheck_statement env thbr in
      let tElbro = 
      match elbro with 
      | None -> None
      | Some branch -> 
        let tElbr, _ = typecheck_statement env branch in
        Some tElbr
      in  
    TAst.IfThenElseStm {cond = typed_cond; thbr = typedThbr; elbro = tElbro}, env

  | Ast.CompoundStm {stms} -> 
    let type_stmts, _ =
    List.fold_left(fun (tsmts_now, env_now) stmt ->
      let tstmt, new_env = typecheck_statement env_now stmt in
      tstmt :: tsmts_now, new_env)
    ([], env)
    stms
  in
    (TAst.CompoundStm {stms = List.rev type_stmts}, env)

  | Ast.ReturnStm {ret} -> 
    let typed_ret, ret_type = infertype_expr env ret in
    (TAst.ReturnStm {ret = typed_ret}, env)

(* should use typecheck_statement to check the block of statements. *)
and typecheck_statement_seq env stms =
  let type_stmts, _ =
    List.fold_left(fun (tsmts_now, env_now) stmt ->
      let tstmt, new_env = typecheck_statement env_now stmt in
      tstmt :: tsmts_now, new_env)
    ([], env)
    stms
  in
  List.rev type_stmts, env;;

(* the initial environment should include all the library functions, no local variables, and no errors. *)
(*let initial_environment = raise UnimpRtrError*)

(*this method will check if the given stm is a return. this is used to check the last stm of an program.*)
let return_check stm = 
  match stm with 
  | TAst.ReturnStm _ -> 1
  | _ -> raise UnimpRtrError

(* should check that the program (sequence of statements) ends in a return statement and make sure that all statements are valid as described in the assignment. Should use typecheck_statement_seq. *)
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
;;
*)


(*module Ast = Ast
module TAst = TypedAst
module Env = Env
module Errors = Errors
module Sym = Symbol
module RunTimeBindings = RunTimeBindings
  
exception Unimplemented (* your code should eventually compile without this exception *)
exception UnimpRtrError (* temporary exception for no return error *)
  
let typecheck_typ = function
  | Ast.Int -> TAst.Int
  | Ast.Bool -> TAst.Bool
  
let typecheck_op op = 
  match op with 
  | Ast.Plus -> TAst.Plus
  | Ast.Minus -> TAst.Minus
  | Ast.Mul -> TAst.Mul
  | Ast.Div -> TAst.Div
  | Ast.Rem -> TAst.Rem
  | Ast.Lt -> TAst.Lt
  | Ast.Le -> TAst.Le
  | Ast.Gt -> TAst.Gt
  | Ast.Ge -> TAst.Ge
  | Ast.Lor -> TAst.Lor
  | Ast.Land -> TAst.Land
  | Ast.Eq -> TAst.Eq
  | Ast.NEq -> TAst.NEq
  
let rec safe_zip l1 l2 =
  match l1, l2 with
  | a :: an, b :: bn -> (a, b) :: safe_zip an bn
  | [], [] -> []
  | [], _ | _, [] -> failwith "safe_zip: Lists have different lengths"
  
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
        | _ -> raise (Invalid_argument (Errors.error_to_string (Errors.TypeMismatch {expected = operand_type; actual = operand_type}))) 
      in
      (TAst.UnOp {op = optyp; operand = operand_expr; tp = operand_type}, operand_type)
  
    | Ast.Lval lvl -> infertype_lval env lvl
  
    | Ast.Assignment {lvl; rhs} -> 
      let _, lvalType = infertype_lval env lvl in 
      let rhsExpr = typecheck_expr env rhs lvalType in
      let lvlType : TAst.lval =
        match lvl with 
        | Ast.Var (Ident {name}) ->
          TAst.Var { ident = TAst.Ident {sym = Symbol.symbol name}; tp = lvalType}
      in 
      (TAst.Assignment {lvl = lvlType; rhs = rhsExpr; tp = lvalType}, lvalType)
  
    | Ast.Call {fname = Ident {name}; args} -> 
      (* Lookup the function in the environment *)
      let sym = Sym.symbol name in
      (match Env.lookup_var_fun env sym with
       | Some (Env.Fun (TAst.FunTyp {ret; params})) ->
         let param_len = List.length params in
         let args_len = List.length args in
         if param_len < args_len then
           raise (Invalid_argument (Errors.error_to_string (Errors.TooManyArguments {expected = param_len; actual = args_len})));
         if param_len > args_len then 
           raise (Invalid_argument (Errors.error_to_string (Errors.MissingArguments {expected = param_len; actual = args_len})));
         let param_types = List.map (fun (TAst.Param {typ; _}) -> typ) params in
         let args_types = safe_zip args param_types in
         let type_args = List.map (fun (a, t) -> typecheck_expr env a t) args_types in 
         (TAst.Call {fname = TAst.Ident {sym}; args = type_args; tp = ret}, ret)
       | None ->
         raise (Invalid_argument (Errors.error_to_string (Errors.UndeclaredVariable {variablename = name})))
       | _ ->
         raise (Invalid_argument (Errors.error_to_string (Errors.NotAFunction {name})))
      )
  
  and infertype_lval env lvl =
    match lvl with
    | Ast.Var (Ident {name}) ->
      let varOpType = Env.lookup_var_fun env (Sym.symbol name) in
      let identName = TAst.Ident {sym = Sym.symbol name} in
      (match varOpType with 
       | None ->
         raise (Invalid_argument (Errors.error_to_string (Errors.UndeclaredVariable {variablename = name})))
       | Some varOrFun ->
         (match varOrFun with
          | Env.Var typ -> (TAst.Lval (TAst.Var {ident = identName; tp = typ}), typ)
          | _ -> raise Unimplemented      
         )
      )
  
  and typecheck_expr env expr tp =
    let texpr, texprtp = infertype_expr env expr in
    if texprtp <> tp then
      raise (Invalid_argument (Errors.error_to_string (Errors.TypeMismatch {expected = tp; actual = texprtp})))
    else texpr
  
  (* should check the validity of a statement and produce the corresponding typed statement. Should use typecheck_expr and/or infertype_expr as necessary. *)
  let rec typecheck_statement env (stm : Ast.statement) : TAst.statement * Env.environment =
    match stm with
    | Ast.VarDeclStm {name = Ast.Ident {name = sname}; tp; body} -> 
      (*(* Check for shadowing *)
      if Env.lookup_var_fun env (Sym.symbol sname) <> None then
        raise (Invalid_argument (Errors.error_to_string (Errors.Shadowing {name = sname})));*)
  
      let tname = TAst.Ident {sym = Sym.symbol sname} in
      let typeBody, tbody_type = 
        match tp with 
        | None -> infertype_expr env body
        | Some btp -> typecheck_expr env body (typecheck_typ btp), typecheck_typ btp
      in
      (TAst.VarDeclStm {name = tname; tp = tbody_type; body = typeBody}, 
      Env.insert_local_decl env (Sym.symbol sname) tbody_type)
  
  
    | Ast.ExprStm {expr} -> 
      (match expr with
      | Some expr ->
        let typed_expr, _ = infertype_expr env expr in
        (TAst.ExprStm {expr = Some typed_expr}, env)
      | None -> 
        (TAst.ExprStm {expr = None}), env)
  
    | Ast.IfThenElseStm {cond; thbr; elbro} -> 
      let typed_cond = typecheck_expr env cond Bool in 
      let typedThbr, _ = typecheck_statement env thbr in
      let tElbro = 
        match elbro with 
        | None -> None
        | Some branch -> 
          let tElbr, _ = typecheck_statement env branch in
          Some tElbr
      in  
      TAst.IfThenElseStm {cond = typed_cond; thbr = typedThbr; elbro = tElbro}, env
  
    | Ast.CompoundStm {stms} -> 
      let type_stmts, _ =
        List.fold_left(fun (tsmts_now, env_now) stmt ->
          let tstmt, new_env = typecheck_statement env_now stmt in
          tstmt :: tsmts_now, new_env)
        ([], env)
        stms
      in
      TAst.CompoundStm { stms = List.rev type_stmts }, env
  
    | Ast.ReturnStm {ret} -> 
      let typed_ret, ret_type = infertype_expr env ret in
      if ret_type != Int then raise (Invalid_argument (Errors.error_to_string (Errors.UnexpectedReturnType {actual = ret_type; expected = Int})));
      (TAst.ReturnStm {ret = typed_ret}, env)
  
  
  
  (* should use typecheck_statement to check the block of statements. *)
and typecheck_statement_seq env stms =
  let type_stmts, _ =
    List.fold_left(fun (tsmts_now, env_now) stmt ->
      let tstmt, new_env = typecheck_statement env_now stmt in
      tstmt :: tsmts_now, new_env)
      ([], env)
      stms
  in
  List.rev type_stmts, env;;
  

(* the initial environment should include all the library functions, no local variables, and no errors. *)
let initial_environment = raise UnimpRtrError

(*this method will check if the given stm is a return. this is used to check the last stm of an program.*)
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
  tProg, !(env_res.errors);

*)