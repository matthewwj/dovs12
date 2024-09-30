(* Env module *)
module TAst = TypedAst
module StrMap = Map.Make(String)

exception Unimplemented (* your code should eventually compile without this exception *)

type environment = StrMap

(* create an initial environment with the given functions defined *)
let make_env function_types = StrMap.add "init" ("type", "func/var") StrMap.empty

(* insert a local declaration into the environment *)
let insert_local_decl env sym (typ, funcvar)  = StrMap.add sym (typ, funcvar) env

(* lookup variables and functions. Note: it must first look for a local variable and if not found then look for a function. *)
let lookup_var_fun env sym = (StrMap.find sym env)