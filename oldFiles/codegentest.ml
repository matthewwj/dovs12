(*module Codegen = Codegen
module Semant = Semant
exception Unimplemented (* your code should eventually compile without this exception *)


(* Positive test: Multiple variable declarations in one statement *)
let test_multi_var_decl_positive = [
  Ast.VarDeclStm (DeclBlock [
    Declaration {
      name = Ast.Ident {name = "a"};
      tp = Some Ast.Int;
      body = Ast.Integer {int = 1L};
    };
    Declaration {
      name = Ast.Ident {name = "b"};
      tp = Some Ast.Int;
      body = Ast.BinOp {
        left = Ast.Integer {int = 2L};
        op = Ast.Plus;
        right = Ast.Integer {int = 3L};
      };
    };
    Declaration {
      name = Ast.Ident {name = "c"};
      tp = Some Ast.Bool;
      body = Ast.Boolean {bool = true};
    };
  ]);
  Ast.ReturnStm {
    ret = Ast.BinOp {
      left = Ast.Lval (Ast.Var (Ast.Ident {name = "a"}));
      op = Ast.Plus;
      right = Ast.Lval (Ast.Var (Ast.Ident {name = "b"}));
    };
  }
]

(* Negative test: Multiple variable declarations with errors *)
let test_multi_var_decl_negative = [
  Ast.VarDeclStm (DeclBlock [
    Declaration {
      name = Ast.Ident {name = "a"};
      tp = Some Ast.Int;
      body = Ast.Integer {int = 1L};
    };
    Declaration {
      name = Ast.Ident {name = "a"};  (* Duplicate variable name *)
      tp = Some Ast.Int;
      body = Ast.Integer {int = 2L};
    };
  ]);
  Ast.ReturnStm {
    ret = Ast.Lval (Ast.Var (Ast.Ident {name = "a"}));
  }
]

(* Another Negative test: Type mismatch in variable initialization *)
let test_multi_var_decl_type_mismatch = [
  Ast.VarDeclStm (DeclBlock [
    Declaration {
      name = Ast.Ident {name = "x"};
      tp = Some Ast.Int;
      body = Ast.Integer {int = 5L};
    };
    Declaration {
      name = Ast.Ident {name = "y"};
      tp = Some Ast.Int;
      body = Ast.Boolean {bool = false};  (* Type mismatch *)
    };
  ]);
  Ast.ReturnStm {
    ret = Ast.BinOp {
      left = Ast.Lval (Ast.Var (Ast.Ident {name = "x"}));
      op = Ast.Plus;
      right = Ast.Lval (Ast.Var (Ast.Ident {name = "y"}));
    };
  }
]


let simple_program = [
  Ast.VarDeclStm (DeclBlock [
    Declaration {
      name = Ast.Ident {name = "x"};
      tp = Some Ast.Int;
      body = Ast.BinOp {
        left = Ast.Integer {int = 2L};
        op = Ast.Plus;
        right = Ast.Integer {int = 2L};
      };
    };
  ]);

  Ast.WhileStm {
    cond = Ast.BinOp {
      left = Ast.Lval (Ast.Var (Ast.Ident {name = "x"}));
      op = Ast.Gt;
      right = Ast.Integer {int = 0L};
    };
    body = Ast.WhileStm {
      cond = Ast.BinOp {
        left = Ast.Lval (Ast.Var (Ast.Ident {name = "x"}));
        op = Ast.Gt;
        right = Ast.Integer {int = 0L};
      };
      body = Ast.ExprStm {
        expr = Some (Ast.Assignment {
          lvl = Ast.Var (Ast.Ident {name = "x"});
          rhs = Ast.BinOp {
            left = Ast.Lval (Ast.Var (Ast.Ident {name = "x"}));
            op = Ast.Minus;
            right = Ast.Integer {int = 1L};
          };
        });
      };
    };
  };

  Ast.ForStm {
    init = None;
    cond = Some (Ast.BinOp {
      left = Ast.Lval (Ast.Var (Ast.Ident {name = "x"}));
      op = Ast.Gt;
      right = Ast.Integer {int = 0L};
    });
    update = Some (Ast.Assignment {
      lvl = Ast.Var (Ast.Ident {name = "x"});
      rhs = Ast.BinOp {
        left = Ast.Lval (Ast.Var (Ast.Ident {name = "x"}));
        op = Ast.Minus;
        right = Ast.Integer {int = 1L};
      };
    });
    body = Ast.ExprStm {
      expr = None;
    };
  };

  Ast.WhileStm {
    cond = Ast.BinOp {
      left = Ast.Lval (Ast.Var (Ast.Ident {name = "x"}));
      op = Ast.Gt;
      right = Ast.Integer {int = 0L};
    };
    body = Ast.BreakStm;
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
  Ast.VarDeclStm (DeclBlock [
    Declaration {
      name = Ast.Ident {name = "x"};
      tp = Some Ast.Int;
      body = Ast.Integer {int = 5L};
    };
  ]);
]


let program_returns_bool = [
  Ast.VarDeclStm (DeclBlock [
    Declaration {
      name = Ast.Ident {name = "x"};
      tp = Some Ast.Bool;
      body = Ast.Boolean {bool = true};
    };
  ]);
  Ast.ReturnStm {
    ret = Ast.Lval (Ast.Var (Ast.Ident {name = "x"})); 
  }
]

let read_integer_test = [
  Ast.VarDeclStm (DeclBlock [
    Declaration {
      name = Ast.Ident {name = "x"};
      tp = Some Ast.Int;
      body = Ast.Call {
          fname = Ast.Ident {name = "read_integer"};
          args = [];
        };
    };
  ]);
  Ast.ReturnStm {
    ret = Ast.Lval (Ast.Var (Ast.Ident {name = "x"})); 
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
      Ast.VarDeclStm (DeclBlock [
        Declaration {
          name = Ast.Ident {name = "x"};
          tp = Some Ast.Int;
          body = Ast.Integer {int = 5L};
        };
      ]);
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
  Ast.VarDeclStm (DeclBlock [
    Declaration {
      name = Ast.Ident {name = "x"}; 
      tp = Some Ast.Int;
      body = Ast.Integer {int = 5L};  
    };
  ]);
  Ast.CompoundStm {
    stms = [
      Ast.VarDeclStm (DeclBlock [
        Declaration {
          name = Ast.Ident {name = "x"};  
          tp = Some Ast.Bool;
          body = Ast.Integer {int = 10L}; 
        };
      ]);
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
  Ast.VarDeclStm (DeclBlock [
    Declaration {
      name = Ast.Ident {name = "x"};
      tp = Some Ast.Int;
      body = Ast.Integer {int = 5L};
    };
  ]);
  Ast.CompoundStm {
    stms = [
      Ast.VarDeclStm (DeclBlock [
        Declaration {
          name = Ast.Ident {name = "x"}; 
          tp = Some Ast.Int;
          body = Ast.Integer {int = 10L};
        };
      ]);
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
    Ast.VarDeclStm (DeclBlock [
      Declaration {
        name = Ast.Ident {name = "x"};
        tp = Some Ast.Int; 
        body = Ast.Integer {int = 10L};  
      };
    ]);
    Ast.ExprStm {
      expr = Some (
        Ast.Call {
          fname = Ast.Ident {name = "QQQ"};
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
    Ast.VarDeclStm (DeclBlock [
      Declaration {
        name = Ast.Ident {name = "x"};
        tp = Some Ast.Int; 
        body = Ast.Integer {int = 10L};  
      };
    ]);
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

let if_then_else_test = [
  Ast.CompoundStm {
    stms = [
      Ast.VarDeclStm (DeclBlock [
        Declaration { name = Ast.Ident {name = "a"}; tp = Some Ast.Int; body = Ast.Integer {int = 0L} };
      ]);
      Ast.VarDeclStm (DeclBlock [
        Declaration { name = Ast.Ident {name = "b"}; tp = Some Ast.Int; body = Ast.Integer {int = 0L} };
      ]);
      Ast.IfThenElseStm {
        cond = Ast.BinOp {left = Ast.Integer {int = 5L}; op = Ast.Lt; right = Ast.Integer {int = 10L}};
        thbr = Ast.ExprStm {expr = Some (Ast.Assignment {lvl = Ast.Var (Ast.Ident {name = "a"}); rhs = Ast.Integer {int = 5L}})};
        elbr = Some (Ast.ExprStm {expr = Some (Ast.Assignment {lvl = Ast.Var (Ast.Ident {name = "b"}); rhs = Ast.Integer {int = 10L}})})
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
  Ast.VarDeclStm (DeclBlock [
    Declaration {
      name = Ast.Ident {name = "x"};
      tp = Some Ast.Bool;
      body = Ast.Boolean {bool = false};
    };
  ]);
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
  Ast.VarDeclStm (DeclBlock [
    Declaration {
      name = Ast.Ident {name = "print_integer"};
      tp = Some Ast.Int;
      body = Ast.Integer {int = 42L}; (* Masks the function print_integer *)
    };
  ]);
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
  Ast.VarDeclStm (DeclBlock [
    Declaration {
      name = Ast.Ident {name = "x"};
      tp = Some Ast.Int;
      body = Ast.Integer {int = 5L};
    };
  ]);
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

let test_logic_short_circuit_pass = [
  Ast.VarDeclStm (DeclBlock [
    Declaration {
      name = Ast.Ident {name = "x"};
      tp = Some Ast.Bool;
      body = Ast.Boolean {bool = false}; 
    };
  ]);
  Ast.VarDeclStm (DeclBlock [
    Declaration {
      name = Ast.Ident {name = "y"};
      tp = Some Ast.Bool;
      body = Ast.Boolean {bool = true};  
    };
  ]);
  Ast.ExprStm {
    expr = Some (Ast.BinOp {
      left = Ast.Lval (Ast.Var (Ast.Ident {name = "x"}));  
      op = Ast.Land;  
      right = Ast.Lval (Ast.Var (Ast.Ident {name = "y"}));  
    })
  };
  Ast.ReturnStm {
    ret = Ast.Integer {int = 0L};
  }
]

let test_logic_short_circuit_fail = [
  Ast.VarDeclStm (DeclBlock [
    Declaration {
      name = Ast.Ident {name = "x"};
      tp = Some Ast.Bool;
      body = Ast.Boolean {bool = true};  
    };
  ]);
  Ast.VarDeclStm (DeclBlock [
    Declaration {
      name = Ast.Ident {name = "y"};
      tp = Some Ast.Int;
      body = Ast.Integer {int = 0L};  
    };
  ]);
  Ast.ExprStm {
    expr = Some (Ast.BinOp {
      left = Ast.Lval (Ast.Var (Ast.Ident {name = "x"}));  (* x = true *)
      op = Ast.Land;  
      right = Ast.Lval (Ast.Var (Ast.Ident {name = "y"}));  (* y = int *)
    })
  };
  Ast.ReturnStm {
    ret = Ast.Integer {int = 0L};
  }
]




let test_nested_while_loop_break = [
  Ast.VarDeclStm (DeclBlock [
    Declaration {
      name = Ast.Ident {name = "x"};
      tp = Some Ast.Int;
      body = Ast.Integer {int = 5L};
    };
  ]);
  
  Ast.WhileStm {
    cond = Ast.BinOp {
      left = Ast.Lval (Ast.Var (Ast.Ident {name = "x"}));
      op = Ast.Gt;
      right = Ast.Integer {int = 0L};
    };
    body = Ast.CompoundStm {
      stms = [
        Ast.WhileStm {
          cond = Ast.BinOp {
            left = Ast.Lval (Ast.Var (Ast.Ident {name = "x"}));
            op = Ast.Eq;
            right = Ast.Integer {int = 3L};
          };
          body = Ast.BreakStm;
        };
        Ast.ExprStm {
          expr = Some (Ast.Assignment {
            lvl = Ast.Var (Ast.Ident {name = "x"});
            rhs = Ast.BinOp {
              left = Ast.Lval (Ast.Var (Ast.Ident {name = "x"}));
              op = Ast.Minus;
              right = Ast.Integer {int = 1L};
            };
          });
        };
      ];
    };
  };
  Ast.ReturnStm {
    ret = Ast.Lval (Ast.Var (Ast.Ident {name = "x"}));
  }
]


let test_var_decls = [
  Ast.VarDeclStm (DeclBlock [
    Declaration {
      name = Ast.Ident {name = "x"};
      tp = Some Ast.Int;
      body = Ast.Integer {int = 10L};
    };
    Declaration {
      name = Ast.Ident {name = "y"};
      tp = Some Ast.Int;
      body = Ast.BinOp {
        left = Ast.Lval (Ast.Var (Ast.Ident {name = "x"}));
        op = Ast.Plus;
        right = Ast.Integer {int = 5L};
      };
    };
  ]);
  Ast.ReturnStm {
    ret = Ast.BinOp {
      left = Ast.Lval (Ast.Var (Ast.Ident {name = "x"}));
      op = Ast.Plus;
      right = Ast.Lval (Ast.Var (Ast.Ident {name = "y"}));
    };
  }
]

let advanced_for_program = [
  Ast.VarDeclStm (DeclBlock [
    Declaration {
      name = Ast.Ident {name = "i"};
      tp = Some Ast.Int;
      body = Ast.Integer {int = 1L};
    };
  ]);

  Ast.ForStm {
    init = Some (FIExpr (Ast.Assignment {
    lvl = Ast.Var (Ast.Ident {name = "i"});
    rhs = Ast.Integer {int = 0L};
    }));
    cond = Some (Ast.BinOp {
      left = Ast.Lval (Ast.Var (Ast.Ident {name = "i"}));
      op = Ast.Lt;
      right = Ast.Integer {int = 10L};
    });
    update = Some (Ast.Assignment {
      lvl = Ast.Var (Ast.Ident {name = "i"});
      rhs = Ast.BinOp {
        left = Ast.Lval (Ast.Var (Ast.Ident {name = "i"}));
        op = Ast.Plus;
        right = Ast.Integer {int = 1L};
      };
    });
    body = Ast.ExprStm {
      expr = Some (Ast.Call {
        fname = Ast.Ident {name = "print_integer"};
        args = [Ast.Lval (Ast.Var (Ast.Ident {name = "i"}))];
      });
    };
  };
  Ast.ExprStm {
      expr = Some (Ast.Call {
        fname = Ast.Ident {name = "print_integer"};
        args = [Ast.Lval (Ast.Var (Ast.Ident {name = "i"}))];
      });
    };
  Ast.ReturnStm {
    ret = Ast.Lval (Ast.Var (Ast.Ident {name = "i"}));
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
  | _ -> print_endline "Unknown error encountered (probably unimplemented)."


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
  test_codegen simple_program "simple program.ll";

  print_endline "Testing vardecl program: Positive test";
  test_codegen test_var_decls "vardeclprog.ll";

  print_endline "Testing nested while loop break program: Positive test";
  test_codegen test_nested_while_loop_break "nestedwhilebreakprog.ll";

  print_endline "Testing advanced for loop program: Positive test";
  test_codegen advanced_for_program "forloop.ll";

  print_endline "Testing read integer program: Positive test";
  test_codegen read_integer_test "testreadint.ll";

  print_endline "Testing multiple variable declarations: Positive test";
  test_codegen test_multi_var_decl_positive "multiplevardecl.ll";

  print_endline "Testing multiple variable declarations with duplicate names: Positive test";
  test_codegen test_multi_var_decl_negative "multiplevardeclwithdup.ll";

  print_endline "Testing multiple variable declarations with type mismatch: Negative test";
  test_codegen test_multi_var_decl_type_mismatch "vardeclwithtypemismatch.ll";

  print_endline "Testing addition type mismatch: Negative test";
  test_codegen test_addition_type_mismatch "additiontypemismatch.ll";

  print_endline "Testing no return prog: Negative test";
  test_codegen program_no_return "noreturn.ll";

  print_endline "Testing return bool: Negative test";
  test_codegen program_returns_bool "boolreturn.ll";

  print_endline "Testing function call: Negative test";
  test_codegen func_call_test "funccall1.ll";
  
  print_endline "Testing function call: Positive test";
  test_codegen func_call_test2 "funccall2.ll";
  
  print_endline "Testing variable declared in inner block, tried using in outer: Positive";
  test_codegen test_declared_var_in_inner_used_in_outer "scoping1.ll";

  print_endline "Testing var decls and assignments: Positive test";
  test_codegen test_var_decl_and_assignment "vardeclandassignment.ll";

  (* We need to look at these tests and the implementation *)
  print_endline "Testing shadowing: Negative test";
  test_codegen test_shadowing "shadowing1.ll"; 

  (*print_endline "Testing ifthenelse: Positive test";
  test_codegen if_then_else_test "ifthenelse.ll";*)

  print_endline "Testing func call arg mismatch: Negative test";
  test_codegen test_func_call_arg_mismatch "funccallargmismatch.ll";

  print_endline "Testing short circuiting: Negative test";
  test_codegen test_short_circuiting "shortcircuit1.ll";

  print_endline "Testing function masking: Negative test";
  test_codegen test_function_masking "funcmasking.ll";

  print_endline "Testing assignment type mismatch: Negative test";
  test_codegen test_assignment_type_mismatch "assignmenttypemismatch.ll";

  print_endline "Testing short circuiting: Positive Test";
  test_codegen test_logic_short_circuit_pass "shortcircuit2.ll";

  print_endline "Testing short circuiting: Negative Test";
  test_codegen test_logic_short_circuit_fail "shortcircuit3.ll";



*)