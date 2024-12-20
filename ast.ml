(* -- Use this in your solution without modifications *)
module Loc = Location

type ident = Ident of {name : string; loc : Loc.location}

type typ =
| Int of {loc : Loc.location}
| Bool of {loc : Loc.location}
| Void of {loc : Loc.location}
| Struct of {id: ident ; loc: Loc.location}
| Ptr of {typ: typ ; loc : Loc.location}
| Str of {loc : Loc.location} (* String type *)
| Byte of {loc : Loc.location} (* Optional *)
| ArrayType of {typ : typ; loc : Loc.location} (* Array type *)


type binop =
| Plus of {loc : Loc.location}
| Minus of {loc : Loc.location}
| Mul of {loc : Loc.location}
| Div of {loc : Loc.location}
| Rem of {loc : Loc.location}
| Lt of {loc : Loc.location}
| Le of {loc : Loc.location}
| Gt of {loc : Loc.location}
| Ge of {loc : Loc.location}
| Lor of {loc : Loc.location}
| Land of {loc : Loc.location}
| Eq of {loc : Loc.location}
| NEq of {loc : Loc.location}

type unop = 
| Neg of {loc : Loc.location}
| Lnot of {loc : Loc.location}

type expr =
| Nil of {loc: Loc.location}
| Integer of {int : int64; loc : Loc.location}
| Boolean of {bool : bool; loc : Loc.location}
| String of {str : string; loc : Loc.location}
| LengthOf of {expr : expr; loc : Loc.location}
| BinOp of {left : expr; op : binop; right : expr; loc : Loc.location}
| UnOp of {op : unop; operand : expr; loc : Loc.location}
| NewExpr of {typ : typ ; obj : new_init ; loc : Loc.location}
| Lval of lval
| Assignment of {lvl : lval; rhs : expr; loc : Loc.location}
| CommaExpr of {lhs : expr; rhs : expr; loc : Loc.location}
| Call of {fname : ident; args : expr list; loc : Loc.location}
and lval =
| Var of ident
| Idx of {arr : expr ; index : expr; loc : Loc.location}
| Fld of {rcrd : expr ; field : ident ; loc : Loc.location}

and field = {name: ident ; expr : expr; loc : Loc.location}
and record = { fields : field list}
and array = {size : expr}
and new_init = Record of record | Array of array

type single_declaration = Declaration of {name : ident; tp : typ option; body : expr; loc : Loc.location}

type declaration_block = DeclBlock of {declarations : single_declaration list; loc : Loc.location}

type for_init =
| FIExpr of expr
| FIDecl of declaration_block

type statement =
| VarDeclStm of declaration_block
| ExprStm of {expr : expr option; loc : Loc.location}
| IfThenElseStm of {cond : expr; thbr : statement; elbro : statement option; loc : Loc.location}
| WhileStm of {cond : expr; body : statement; loc : Loc.location}
| ForStm of {init : for_init option; cond : expr option; update : expr option; body : statement; loc : Loc.location}
| BreakStm of {loc : Loc.location}
| ContinueStm of {loc : Loc.location}
| CompoundStm of {stms : statement list; loc : Loc.location}
| ReturnStm of {ret : expr; loc : Loc.location} 


type param = Param of {name : ident; tp : typ; loc : Loc.location}

type func_decl =  {
  fname : ident;
  params : param list;
  ret_type : typ; 
  body : statement list;
  loc : Loc.location;
}

type record_decl = {name : ident; fields : param list; loc : Loc.location;}

type global_elements = Function of func_decl | Record of record_decl

type program = 
  | Program of global_elements list



