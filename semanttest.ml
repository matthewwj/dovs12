module Semant = Semant

exception Unimplemented

(* Example Program AST:
   int x = 5;
   return x + 2;
*)

let simple_program = [
  Ast.VarDeclStm {
    name = Ast.Ident {name = "x"};
    tp = Some Ast.Int;
    body = Ast.Integer {int = 5L};
  };
  Ast.ReturnStm {
    ret = Ast.BinOp {
    left = Ast.Integer {int = 2L};
    op = Ast.Plus;
    right = Ast.Integer {int = 2L};
    };
  }
]

(* Example Program AST without a return:
   int x = 5;
*)

let program_no_return = [
  Ast.VarDeclStm {
    name = Ast.Ident {name = "x"};
    tp = Some Ast.Int;
    body = Ast.Integer {int = 5L};
  }
]

let test_var_decl_and_assignment = [
  Ast.CompoundStm {
    stms = [
      Ast.VarDeclStm {name = Ast.Ident {name = "x"}; tp = Some Ast.Int; body = Ast.Integer {int = 5L}};
      Ast.ExprStm {expr = Some (Ast.Assignment {lvl = Ast.Var (Ast.Ident {name = "x"}); rhs = Ast.Integer {int = 10L}})}
    ];
  };
  Ast.ReturnStm {
    ret = Ast.BinOp {
    left = Ast.Integer {int = 2L};
    op = Ast.Plus;
    right = Ast.Integer {int = 2L};
    };
  }
]


let test_typecheck program =
  try
    let _ = Semant.typecheck_prog program in
    print_endline "Program typechecked successfully."
  with
  | Invalid_argument msg -> print_endline ("Typecheck failed: " ^ msg)
  | Unimplemented -> print_endline "Unimplemented feature encountered."
  | _ -> print_endline "Unknown error during typecheck."

(* Test with the simple valid program *)
let () =

  print_endline "Testing simple program:";
  test_typecheck test_var_decl_and_assignment;