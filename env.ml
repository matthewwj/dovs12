(* Env module *)
module TAst = TypedAst
module Sym = Symbol

exception Unimplemented (* your code should eventually compile without this exception *)

type varOrFun =
  | Var of TAst.typ
  | Fun of TAst.funtype

type environment = {
  vars_and_funs : varOrFun Sym.Table.t;
  internal_funs : (Ll.ty * Ll.ty list) Sym.Table.t;
  record_decls : TAst.record_decl Sym.Table.t;
  errors : Errors.error list ref;
  loop : int;
}

(* Create an initial environment with the given functions defined *)
let make_env library_function_types internal_function_types =
  let vars_and_funs_table = Sym.Table.empty in
  let vars_and_funs_table = 
    List.fold_left (fun tbl (fsym, ftp) -> Sym.Table.add fsym (Fun ftp) tbl) vars_and_funs_table library_function_types
  in 
  let internal_funs_table = Sym.Table.empty in
  let internal_funs_table =
    List.fold_left (fun tbl (fsym, ret_ty, arg_tys) -> Sym.Table.add fsym (ret_ty, arg_tys) tbl) internal_funs_table internal_function_types
  in
  { 
    vars_and_funs = vars_and_funs_table; 
    internal_funs = internal_funs_table;  (* Initialize internal functions *)
    record_decls = Sym.Table.empty; 
    errors = ref []; 
    loop = 0 
  }

(* Insert a local declaration into the environment *)
let insert_local_decl env sym typ =
  { env with vars_and_funs = Sym.Table.add sym (Var typ) env.vars_and_funs }

let insert_local_func_decl env sym typ =
  let {vars_and_funs; _} = env in
  {env with vars_and_funs = Sym.Table.add sym (Fun typ) vars_and_funs}

(* Lookup variables and functions. 
   It first looks for a local variable and if not found then looks for a function. *)
let lookup_var_fun env sym =
  Sym.Table.find_opt sym env.vars_and_funs

(* Insert a record declaration into the environment *)
let insert_record_decl env sym record_decl =
  { env with record_decls = Sym.Table.add sym record_decl env.record_decls }

(* Lookup a record declaration in the environment *)
let lookup_record_decl env sym =
  Sym.Table.find_opt sym env.record_decls

(* Lookup internal functions *)
let lookup_internal_func env sym =
  Sym.Table.find_opt sym env.internal_funs