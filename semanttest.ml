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
  };
]

let test_addition_type_mismatch = [
  Ast.CompoundStm {
    stms = [
      Ast.ExprStm {expr = Some (Ast.BinOp {
        left = Ast.Integer {int = 5L};
        op = Ast.Minus;
        right = Ast.Boolean {bool = true}; (* Incorrect: trying to add a bool and an int *)
      })}
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
let test_shadowing = [
  (* Outer variable declaration *)
  Ast.VarDeclStm {
    name = Ast.Ident {name = "x"};  (* Declaring outer x *)
    tp = Some Ast.Int;
    body = Ast.Integer {int = 5L};  (* Outer x = 5 *)
  };
  (* Compound statement with inner variable declaration *)
  Ast.CompoundStm {
    stms = [
      Ast.VarDeclStm {
        name = Ast.Ident {name = "x"};  (* Declaring inner x, shadows outer x *)
        tp = Some Ast.Int;
        body = Ast.Integer {int = 10L}; (* Inner x = 10 *)
      };
      
      (* Expression using the inner variable *)
      Ast.ExprStm {
        expr = Some (Ast.BinOp {
          left = Ast.Lval (Ast.Var (Ast.Ident {name = "x"}));  (* Should refer to inner x (10) *)
          op = Ast.Plus;
          right = Ast.Integer {int = 2L};  (* 10 + 2 *)
        });
      };
    ]
  };
  
  (* Return statement should refer to the outer variable *)
  Ast.ReturnStm {
    ret = Ast.Lval (Ast.Var (Ast.Ident {name = "x"}));  (* Should refer to outer x (5) *)
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
  print_endline "Testing simple program: Positive test";
  test_typecheck simple_program;

  print_endline "Testing addition type mismatch: Negative test";
  test_typecheck test_addition_type_mismatch;

  print_endline "Testing no return prog: Negative test";
  test_typecheck program_no_return;
  
  print_endline "Testing shadowing: Positive test";
  test_typecheck test_shadowing;

  