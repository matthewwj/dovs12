module TAst = TypedAst
module Sym = Symbol


let make_ident name = TAst.Ident {sym = Sym.symbol name}
let array_type : Ll.ty = Ll.Struct [Ll.I64; Ll.Array (0, Ll.I8)]
let array_type_name = Sym.symbol "array_type"
let array_type : Ll.ty = Ll.Namedt array_type_name

let dolphin_types name : TAst.record_decl =
  { name = TAst.Ident { sym = Sym.symbol ("" ^ name) }; fields = [] }
;;

let dolphin_rc_empty_string = dolphin_types

let library_functions =
  [
    (Symbol.symbol "read_integer", TAst.FunTyp {ret = TAst.Int; params = []});
    (Symbol.symbol "print_integer", TAst.FunTyp {
      ret = TAst.Void;
      params = [TAst.Param {paramname = make_ident "value"; typ = TAst.Int}]
    });
    (Symbol.symbol "compare_strings", TAst.FunTyp {
      ret = TAst.Int; (* Use TAst.Int for i64 *)
      params = [
        TAst.Param {paramname = make_ident "str1"; typ = TAst.Ptr TAst.Str};
        TAst.Param {paramname = make_ident "str2"; typ = TAst.Ptr TAst.Str}
      ]
    });
  ]
