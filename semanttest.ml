module Semant = Semant

exception Unimplemented

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

let program_no_return = [
  Ast.VarDeclStm {
    name = Ast.Ident {name = "x"};
    tp = Some Ast.Int;
    body = Ast.Integer {int = 5L};
  };
]

let program_returns_bool = [
  Ast.ReturnStm {
    ret = Ast.Boolean{bool = true}
  }
]

let test_addition_type_mismatch = [
  Ast.CompoundStm {
    stms = [
      Ast.ExprStm {expr = Some (Ast.BinOp {
        left = Ast.Integer {int = 5L};
        op = Ast.Plus;
        right = Ast.Boolean {bool = true}; 
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
  Ast.VarDeclStm {
    name = Ast.Ident {name = "x"}; 
    tp = Some Ast.Int;
    body = Ast.Integer {int = 5L};  
  };
  Ast.CompoundStm {
    stms = [
      Ast.VarDeclStm {
        name = Ast.Ident {name = "x"};  
        tp = Some Ast.Int;
        body = Ast.Integer {int = 10L}; 
      };
      
      Ast.ExprStm {
        expr = Some (Ast.BinOp {
          left = Ast.Lval (Ast.Var (Ast.Ident {name = "x"}));  
          op = Ast.Plus;
          right = Ast.Integer {int = 2L}; 
        });
      };
    ]
  };
  Ast.ReturnStm {
    ret = Ast.Lval (Ast.Var (Ast.Ident {name = "x"})); 
  }
]

let test_declared_var_in_inner_used_in_outer = [
  Ast.VarDeclStm {
    name = Ast.Ident {name = "y"};
    tp = Some Ast.Int;
    body = Ast.Integer {int = 5L};
  };
Ast.CompoundStm {
  stms = [
    Ast.VarDeclStm {
      name = Ast.Ident {name = "x"}; 
      tp = Some Ast.Int;
      body = Ast.Integer {int = 10L};
    };
    Ast.ExprStm {expr = Some (Ast.BinOp {
      left = Ast.Lval (Ast.Var (Ast.Ident {name = "x"})); 
      op = Ast.Plus;
      right = Ast.Integer {int = 2L};
    })}
  ];
};
Ast.ExprStm {expr = Some (Ast.BinOp {
      left = Ast.Lval (Ast.Var (Ast.Ident {name = "x"})); 
      op = Ast.Plus;
      right = Ast.Integer {int = 2L};
    })};
Ast.ReturnStm {
  ret = Ast.BinOp {
    left = Ast.Integer {int = 2L};
    op = Ast.Plus;
    right = Ast.Integer {int = 2L};
  };
}
]


let func_call_test = [
    Ast.VarDeclStm {
      name = Ast.Ident {name = "x"};
      tp = Some Ast.Int; 
      body = Ast.Integer {int = 10L}  
    };
    Ast.ExprStm {
      expr = Some (
        Ast.Call {
          fname = Ast.Ident {name = "print_intege"};
          args = [Ast.Lval (Ast.Var (Ast.Ident {name = "x"}))]
        }
      )
    };
    Ast.ReturnStm {
      ret = Ast.BinOp {
        left = Ast.Integer {int = 2L};
        op = Ast.Plus;
        right = Ast.Integer {int = 2L};
      };
    }  
]

let func_call_test2 = [
    Ast.VarDeclStm {
      name = Ast.Ident {name = "x"};
      tp = Some Ast.Int; 
      body = Ast.Integer {int = 10L}  
    };
    Ast.ExprStm {
      expr = Some (
        Ast.Call {
          fname = Ast.Ident {name = "print_integer"};
          args = [Ast.Lval (Ast.Var (Ast.Ident {name = "x"}))]
        }
      )
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

let () =
  print_endline "Testing simple program: Positive test";
  test_typecheck simple_program;

  print_endline "Testing addition type mismatch: Negative test";
  test_typecheck test_addition_type_mismatch;

  print_endline "Testing no return prog: Negative test";
  test_typecheck program_no_return;

  print_endline "Testing return bool: Negative test";
  test_typecheck program_returns_bool;

  print_endline "Testing function call: Negative test";
  test_typecheck func_call_test;
  
  print_endline "Testing function call: Positive test";
  test_typecheck func_call_test2;
  
  print_endline "Testing variable declared in inner block, tried using in outer: Negative test";
  test_typecheck test_declared_var_in_inner_used_in_outer;

  print_endline "Testing var decls and assignments: Positive test";
  test_typecheck test_var_decl_and_assignment;

  (* We need to look at these tests and the implementation *)
  print_endline "Testing shadowing: Positive test";
  test_typecheck test_shadowing;

  
  