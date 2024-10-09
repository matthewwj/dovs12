(* Env module *)
module TAst = TypedAst
module Sym = Symbol

exception Unimplemented (* your code should eventually compile without this exception *)

type varOrFun =
  | Var of TAst.typ
  | Fun of TAst.funtype

type environment = {
  vars_and_funs : varOrFun Sym.Table.t;
  errors : Errors.error list ref;
}

(* Create an initial environment with the given functions defined *)
let make_env function_types =
  let emp = Sym.Table.empty in
  let env = 
    List.fold_left (fun env (fsym, ftp) -> Sym.Table.add fsym (Fun ftp) env) emp function_types
  in
  { vars_and_funs = env; errors = ref [] }

(* Insert a local declaration into the environment *)
let insert_local_decl env sym typ =
  { env with vars_and_funs = Sym.Table.add sym (Var typ) env.vars_and_funs }

(* Lookup variables and functions. 
   It first looks for a local variable and if not found then looks for a function. *)
let lookup_var_fun env sym =
  Sym.Table.find_opt sym env.vars_and_funs