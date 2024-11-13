(* Errors module *)
module Sym = Symbol
module TAst = TypedAst
module TPretty = TypedPretty

exception ParseErr of string 

let location_to_string (location : Location.location) =
  let start_lnum = location.start_pos.pos_lnum in
  let start_cnum = location.start_pos.pos_cnum - location.start_pos.pos_bol in
  let end_lnum = location.end_pos.pos_lnum in
  let end_cnum = location.end_pos.pos_cnum - location.end_pos.pos_bol in
  let sentence =
    Printf.sprintf
      "start location (%d, %d) - end location (%d, %d)"
      start_lnum
      start_cnum
      end_lnum
      end_cnum
  in
  sentence;

type error =
| TypeMismatch of {expected : TAst.typ; actual : TAst.typ; loc: Location.location}
| NoReturnError
| UndeclaredVariable of { variablename : string; loc: Location.location }
| TooManyArguments of {expected: int ; actual: int; loc: Location.location}
| MissingArguments of {expected: int; actual: int; loc: Location.location}
| NotAFunction of {name : string; loc: Location.location}
| UnexpectedReturnType of { expected : TAst.typ; actual : TAst.typ; loc: Location.location}
| Shadowing of {name : string; loc: Location.location} 
| NotEndInRet 
| UnexpectedBreak of {loc: Location.location}
| UnexpectedContinue of {loc: Location.location}
| DuplicateName of {name: string; loc: Location.location}
| ParamsInMainFunc of {loc: Location.location}
| DuplicateParams of {loc: Location.location}
| MissingReturn of {loc: Location.location}
| VoidInFuncParam of {loc: Location.location}

(* other errors to be added as needed. *)

(* Useful for printing errors *)
let error_to_string err =
  match err with
  | TypeMismatch {expected; actual; loc} -> Printf.sprintf "Type mismatch: expected %s but found %s at %s." (TPretty.typ_to_string expected) (TPretty.typ_to_string actual) (location_to_string loc)
  | NoReturnError -> Printf.sprintf "program is missing a return statement"
  | UndeclaredVariable { variablename; loc } -> Printf.sprintf "Undeclared variable %s at %s" variablename (location_to_string loc)
  | TooManyArguments {expected; actual; loc} -> Printf.sprintf "Too many arguments: expected %d but found %d at %s." expected actual (location_to_string loc)
  | MissingArguments {expected; actual; loc} -> Printf.sprintf "Too few arguments: expected %d but found %d at %s." expected actual (location_to_string loc)
  | NotAFunction {name; loc} -> Printf.sprintf "Tried to call non-function %s at %s" name (location_to_string loc) 
  | UnexpectedReturnType {expected; actual; loc} -> Printf.sprintf "Unexpected return type: expected %s but found %s at %s." (TPretty.typ_to_string expected) (TPretty.typ_to_string actual) (location_to_string loc)
  | NotEndInRet -> "The program does not end in a return statement."
  | Shadowing {name; loc} -> Printf.sprintf "Shadowing error please update this error msg %s at %s" name (location_to_string loc) 
  | UnexpectedBreak {loc} -> Printf.sprintf "Break not in loop at %s" (location_to_string loc)
  | UnexpectedContinue {loc} -> Printf.sprintf "Continue not in loop at %s" (location_to_string loc)
  | DuplicateName {name; loc} -> Printf.sprintf "Duplicate function name %s. Location is: %s" (name) (location_to_string loc)
  | ParamsInMainFunc {loc} -> Printf.sprintf "Main function cannot have params at %s" (location_to_string loc)
  | DuplicateParams {loc} -> Printf.sprintf "Duplicate parameters at %s" (location_to_string loc)
  | MissingReturn {loc} -> Printf.sprintf "Missing return at %s" (location_to_string loc)
  | VoidInFuncParam {loc} -> Printf.sprintf "Cannot declare void type in function parameter %s" (location_to_string loc)