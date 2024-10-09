module Codegen = Codegen
module Semant = Semant
exception Unimplemented (* your code should eventually compile without this exception *)

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
        tp = Some Ast.Bool;
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
          fname = Ast.Ident {name = "read_integer"};
          args = []
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

let if_then_else_test = 
  [
    Ast.CompoundStm {
      stms = [
    VarDeclStm { name = Ident {name = "a"}; tp = Some Int; body = Integer {int = 0L} };
    VarDeclStm { name = Ident {name = "b"}; tp = Some Int; body = Integer {int = 0L} };
    IfThenElseStm {
      cond = BinOp {left = Integer {int = 5L}; op = Lt; right = Integer {int = 10L}};
      thbr = ExprStm {expr = Some (Assignment {lvl = Var (Ident {name = "a"}); rhs = Integer {int = 5L}})};
      elbro = Some (ExprStm {expr = Some (Assignment {lvl = Var (Ident {name = "b"}); rhs = Integer {int = 10L}})
      })
    }; 
    ]};
    Ast.ReturnStm {
      ret = Ast.BinOp {
        left = Ast.Integer {int = 2L};
        op = Ast.Plus;
        right = Ast.Integer {int = 2L};
      };
    }  
  ]

let test_func_call_arg_mismatch = [
  Ast.ExprStm {
    expr = Some (
      Ast.Call {
        fname = Ast.Ident {name = "print_integer"};
        args = [Ast.Boolean {bool = true}] (* Incorrect argument type *)
      }
    )
  };
  Ast.ReturnStm {
    ret = Ast.BinOp {
      left = Ast.Integer {int = 2L};
      op = Ast.Plus;
      right = Ast.Integer {int = 2L};
    }
  }
]

let test_short_circuiting = [
  Ast.VarDeclStm {
    name = Ast.Ident {name = "x"};
    tp = Some Ast.Bool;
    body = Ast.Boolean {bool = false};
  };
  Ast.ExprStm {
    expr = Some (Ast.BinOp {
      left = Ast.Lval (Ast.Var (Ast.Ident {name = "x"})); (* false *)
      op = Ast.Land;
      right = Ast.Call {
        fname = Ast.Ident {name = "print_integer"}; (* this should not execute *)
        args = [Ast.Integer {int = 5L}]
      }
    })
  };
  Ast.ReturnStm {
    ret = Ast.Integer {int = 0L};
  }
]

let test_function_masking = [
  Ast.VarDeclStm {
    name = Ast.Ident {name = "print_integer"};
    tp = Some Ast.Int;
    body = Ast.Integer {int = 42L}; (* Masks the function print_integer *)
  };
  Ast.ExprStm {
    expr = Some (
      Ast.Call {
        fname = Ast.Ident {name = "print_integer"}; (* This should fail *)
        args = [Ast.Integer {int = 5L}]
      }
    )
  };
  Ast.ReturnStm {
    ret = Ast.Integer {int = 0L};
  }
]

let test_assignment_type_mismatch = [
  Ast.VarDeclStm {
    name = Ast.Ident {name = "x"};
    tp = Some Ast.Int;
    body = Ast.Integer {int = 5L};
  };
  Ast.ExprStm {
    expr = Some (Ast.Assignment {
      lvl = Ast.Var (Ast.Ident {name = "x"});
      rhs = Ast.Boolean {bool = true}; (* Type mismatch *)
    })
  };
  Ast.ReturnStm {
    ret = Ast.Integer {int = 0L};
  }
]


let test_codegen program output_filename=
  try
    let typedStmt, _ = Semant.typecheck_prog program in
    let llvm_prog = Codegen.codegen_prog typedStmt in
    let llvm_ir_string = Ll.string_of_prog llvm_prog in
    Codegen.write_to_file output_filename llvm_ir_string;
    print_endline "Program typechecked successfully."
  with
  | Invalid_argument msg -> print_endline ("Typecheck failed: " ^ msg)
  | Unimplemented -> print_endline "Unimplemented feature encountered."
  | _ -> print_endline "Unknown error during typecheck."


let compile_prog program =
  try
    let typedStmt, _ = Semant.typecheck_prog program in
    let llvm_prog = Codegen.codegen_prog typedStmt in
    let llvm_ir_string = Ll.string_of_prog llvm_prog in
    print_endline llvm_ir_string;
    exit 0
  with
  | Invalid_argument msg ->
    prerr_endline ("Typecheck failed: " ^ msg);
    exit 1
  | Unimplemented ->
    prerr_endline "Unimplemented feature encountered.";
    exit 1
  | _ ->
    prerr_endline "Unknown error during typecheck.";
    exit 1


let () =

  print_endline "Testing simple program: Positive test";
  test_codegen simple_program "test2.ll";

  print_endline "Testing addition type mismatch: Negative test";
  test_codegen test_addition_type_mismatch "test3.ll";

  print_endline "Testing no return prog: Negative test";
  test_codegen program_no_return "test4.ll";

  print_endline "Testing return bool: Negative test";
  test_codegen program_returns_bool "test5.ll";

  print_endline "Testing function call: Negative test";
  test_codegen func_call_test "test6.ll";
  
  print_endline "Testing function call: Positive test";
  test_codegen func_call_test2 "test7.ll";
  
  print_endline "Testing variable declared in inner block, tried using in outer: Negative test";
  test_codegen test_declared_var_in_inner_used_in_outer "test8.ll";

  print_endline "Testing var decls and assignments: Positive test";
  test_codegen test_var_decl_and_assignment "test9.ll";

  (* We need to look at these tests and the implementation *)
  print_endline "Testing shadowing: Positive test";
  test_codegen test_shadowing "test10.ll"; 

  print_endline "Testing ifthenelse: Positive test";
  test_codegen if_then_else_test "test11.ll";

  print_endline "Testing func call arg mismatch: Negative test";
  test_codegen test_func_call_arg_mismatch "test12.ll";

  print_endline "Testing short circuiting: Negative test";
  test_codegen test_short_circuiting "test13.ll";

  print_endline "Testing function masking: Negative test";
  test_codegen test_function_masking "test14.ll";

  print_endline "Testing assignment type mismatch: Negative test";
  test_codegen test_assignment_type_mismatch "test15.ll";

  compile_prog if_then_else_test;