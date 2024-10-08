(* Errors module *)
module Sym = Symbol
module TAst = TypedAst
module TPretty = TypedPretty

type error =
| TypeMismatch of {expected : TAst.typ; actual : TAst.typ}
| NoReturnError
| UndeclaredVariable of { variablename : string }
| TooManyArguments of {expected: int ; actual: int}
| MissingArguments of {expected: int; actual: int}
| NotAFunction of {name : string}
| UnexpectedReturnType of { expected : TAst.typ; actual : TAst.typ}
| Shadowing of {name : string} 
| NotEndInRet
(* other errors to be added as needed. *)

(* Useful for printing errors *)
let error_to_string err =
  match err with
  | TypeMismatch {expected; actual; _} -> Printf.sprintf "Type mismatch: expected %s but found %s." (TPretty.typ_to_string expected) (TPretty.typ_to_string actual)
  | NoReturnError -> Printf.sprintf "program is missing a return statement"
  | UndeclaredVariable { variablename } -> Printf.sprintf "Undeclared variable %s" variablename
  | TooManyArguments {expected; actual} -> Printf.sprintf "Too many arguments: expected %d but found %d." expected actual
  | MissingArguments {expected; actual} -> Printf.sprintf "Too few arguments: expected %d but found %d." expected actual
  | NotAFunction {name} -> Printf.sprintf "Tried to call non-function %s" name 
  | UnexpectedReturnType {expected; actual} -> Printf.sprintf "Unexpected return type: expected %s but found %s." (TPretty.typ_to_string expected) (TPretty.typ_to_string actual)
  | NotEndInRet -> "The program does not end in a return statement."
  | Shadowing {name} -> Printf.sprintf "Shadowing error please update this error msg %s" name 