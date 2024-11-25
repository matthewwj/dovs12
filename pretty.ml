module PBox = PrintBox
open Ast

(* producing trees for pretty printing *)
let typ_style = PBox.Style.fg_color PBox.Style.Green
let ident_style = PBox.Style.fg_color PBox.Style.Yellow
let fieldname_style = ident_style
let keyword_style = PBox.Style.fg_color PBox.Style.Blue

let info_node_style = PBox.Style.fg_color PBox.Style.Cyan

let make_typ_line name = PBox.line_with_style typ_style name
let make_fieldname_line name = PBox.line_with_style fieldname_style name
let make_ident_line name = PBox.line_with_style ident_style name
let make_keyword_line name = PBox.line_with_style keyword_style name

let make_info_node_line info = PBox.line_with_style info_node_style info

let ident_to_tree (Ident {name; _}) = make_ident_line name


let rec ast_typ tp =
  match tp with
  | Bool _ -> "bool"
  | Int _ -> "int"
  | Struct { id = Ident { name; _ }; _ } -> name
  | Str _ -> "String"
  | ArrayType _ -> "Array"
  | Ptr a -> "Ptr " ^ ast_typ a.typ
;;

let typ_to_tree tp =
  match tp with
  | Bool _ -> make_typ_line "Bool"
  | Int _ -> make_typ_line "Int"
  | Struct {id = Ident {name; _}; _} -> make_typ_line name
  | Str _ -> make_typ_line  "String"
  | ArrayType _ -> make_typ_line "Array"
  | Ptr a -> make_typ_line ("Ptr" ^ ast_typ a.typ)

let ident_fix name =
  match name with
  | Ident { name; _ } -> name
;;

let binop_to_tree op =
    match op with
    | Plus _ -> make_keyword_line "PLUS"
    | Minus _ -> make_keyword_line "Minus"
    | Mul _ -> make_keyword_line "Mul"
    | Div _ -> make_keyword_line "Div"
    | Rem _ -> make_keyword_line "Rem"
    | Lt _ -> make_keyword_line "Lt"
    | Le _ -> make_keyword_line "Le"
    | Gt _ -> make_keyword_line "Gt"
    | Ge _ -> make_keyword_line "Ge"
    | Lor _ -> make_keyword_line "Lor"
    | Land _ -> make_keyword_line "Land"
    | Eq _ -> make_keyword_line "Eq"
    | NEq _ -> make_keyword_line "NEq"
  
let unop_to_tree op =
  match op with
  | Neg _ -> make_keyword_line "Neg"
  | Lnot _ -> make_keyword_line "Lor"
  
  let rec expr_to_tree e =
    match e with
    | Integer {int; _} -> PBox.hlist ~bars:false [make_info_node_line "IntLit("; PBox.line (Int64.to_string int); make_info_node_line ")"]
    | Boolean {bool; _} -> PBox.hlist ~bars:false [make_info_node_line "BooleanLit("; make_keyword_line (if bool then "true" else "false"); make_info_node_line ")"]
    | BinOp {left; op; right; _} -> PBox.tree (make_info_node_line "BinOp") [expr_to_tree left; binop_to_tree op; expr_to_tree right]
    | UnOp {op; operand; _} -> PBox.tree (make_info_node_line "UnOp") [unop_to_tree op; expr_to_tree operand]
    | Lval l -> PBox.tree (make_info_node_line "Lval") [lval_to_tree l]
    | Assignment {lvl; rhs; _} -> PBox.tree (make_info_node_line "Assignment") [lval_to_tree lvl; expr_to_tree rhs]
    | Call {fname; args; _} ->
      PBox.tree (make_info_node_line "Call")
        [PBox.hlist ~bars:false [make_info_node_line "FunName: "; ident_to_tree fname];
         PBox.tree (make_info_node_line "Args") (List.map (fun e -> expr_to_tree e) args)]
    | CommaExpr {lhs; rhs; _} -> PBox.tree (make_info_node_line "CommaExpr") [expr_to_tree lhs; expr_to_tree rhs]
    | NewExpr e ->
      PBox.hlist
      ~bars:false
      [make_info_node_line ("NewExpr " ^ ast_typ e.typ ^ " "); PBox.line (new_expr_str e.obj)]
    | String {str; _} ->
      PBox.hlist 
      ~bars:false
      [make_info_node_line "StringLit("; PBox.line str; make_info_node_line ")"]
    | LengthOf e -> PBox.tree (make_info_node_line "length_of") [ expr_to_tree e.expr ]
    | Nil _ -> PBox.tree (make_info_node_line "Nil") []
  and lval_to_tree l =
    match l with
    | Var ident -> PBox.hlist ~bars:false [make_info_node_line "Var("; ident_to_tree ident; make_info_node_line ")"]
    | Idx e ->
      PBox.tree
        (make_info_node_line "array lookup")
        [ expr_to_tree e.arr; expr_to_tree e.index ]
    | Fld _ -> PBox.hlist ~bars:false [ make_info_node_line "record_lookup(" ]
and new_expr_str (e : Ast.new_init) : string =
  match e with
  | Ast.Record e ->
    "{"
    ^ String.concat
        "; "
        (List.map
           (fun (e : Ast.field) ->
             ident_fix e.name ^ " = " ^ PrintBox_text.to_string (expr_to_tree e.expr))
           e.fields)
    ^ "}"
  | Ast.Array e -> "[" ^ PrintBox_text.to_string (expr_to_tree e.size) ^ "]"
;;

let single_declaration_to_tree (Declaration {name; tp; body; _}) =
  PBox.tree (make_keyword_line "Declaration") 
    [PBox.hlist ~bars:false [make_info_node_line "Ident: "; ident_to_tree name]; 
    PBox.hlist ~bars:false [make_info_node_line "Type: "; Option.fold ~none:PBox.empty ~some:typ_to_tree tp];
    PBox.hlist ~bars:false [make_info_node_line "Body: "; expr_to_tree body]]

let declaration_block_to_tree (DeclBlock {declarations; _}) =
PBox.tree (make_keyword_line "VarDecl")  (List.map single_declaration_to_tree declarations)

let for_init_to_tree = function
| FIDecl db -> PBox.hlist ~bars:false [PBox.line "ForInitDecl: "; declaration_block_to_tree db]
| FIExpr e -> PBox.hlist ~bars:false [PBox.line "ForInitExpr: "; expr_to_tree e]

let rec statement_to_tree c =
  match c with
  | VarDeclStm db -> PBox.hlist ~bars:false [PBox.line "DeclStm: "; declaration_block_to_tree db]
  | ExprStm {expr; _} -> PBox.hlist ~bars:false [make_info_node_line "ExprStm: "; Option.fold ~none:PBox.empty ~some:expr_to_tree expr]
  | IfThenElseStm {cond; thbr; elbro; _} ->
    PBox.tree (make_keyword_line "IfStm")
      ([PBox.hlist ~bars:false [make_info_node_line "Cond: "; expr_to_tree cond]; PBox.hlist ~bars:false [make_info_node_line "Then-Branch: "; statement_to_tree thbr]] @
       match elbro with None -> [] | Some elbr -> [PBox.hlist ~bars:false [make_info_node_line "Else-Branch: "; statement_to_tree elbr]])
  | WhileStm {cond; body; _} ->
    PBox.tree (make_keyword_line "WhileStm") 
      [PBox.hlist ~bars:false [make_info_node_line "Cond: "; expr_to_tree cond];
        PBox.hlist ~bars:false [make_info_node_line "Body: "; statement_to_tree body]]
  | ForStm {init; cond; update; body; _} ->
    PBox.tree (make_keyword_line "ForStm") 
      [PBox.hlist ~bars:false [make_info_node_line "Init: "; Option.fold ~none:PBox.empty ~some:for_init_to_tree init];
        PBox.hlist ~bars:false [make_info_node_line "Cond: "; Option.fold ~none:PBox.empty ~some:expr_to_tree cond];
        PBox.hlist ~bars:false [make_info_node_line "Update: "; Option.fold ~none:PBox.empty ~some:expr_to_tree update];
        PBox.hlist ~bars:false [make_info_node_line "Body: "; statement_to_tree body]]
  | BreakStm _ -> make_keyword_line "BreakStm"
  | ContinueStm _ -> make_keyword_line "ContinueStm"
  | CompoundStm {stms; _} -> PBox.tree (make_info_node_line "CompoundStm") (statement_seq_to_forest stms)
  | ReturnStm {ret; _} -> PBox.hlist ~bars:false [make_keyword_line "ReturnValStm: "; expr_to_tree ret]
and statement_seq_to_forest stms = List.map statement_to_tree stms

let program_to_tree prog =
  PBox.tree (make_info_node_line "Program") (statement_seq_to_forest prog)
;;

let function_arg_to_tree (Param {name; tp; _}) =
  PBox.tree (make_keyword_line "FunctionArg") 
    [PBox.hlist ~bars:false [make_info_node_line "ArgName: "; ident_to_tree name]; 
    PBox.hlist ~bars:false [make_info_node_line "ArgType: "; typ_to_tree tp]]

let function_decl_to_tree ({ret_type; fname; params; body; _}) =
  PBox.tree (make_keyword_line "FunctionDecl")
    [PBox.hlist ~bars:false [make_info_node_line "ReturnType: "; typ_to_tree ret_type];
     PBox.hlist ~bars:false [make_info_node_line "FunctionName: "; ident_to_tree fname];
     PBox.tree (make_info_node_line "FunctionArgs") (List.map function_arg_to_tree params);
     PBox.hlist ~bars:false [make_info_node_line "FunctionBody: "; PBox.tree (make_keyword_line "FunctionBody") (statement_seq_to_forest body)]]

  let program_to_tree prog =
    PBox.tree (make_info_node_line "Program") (statement_seq_to_forest prog)
  ;;

  
let funcs_to_tree (funcs : func_decl list) =
  List.map
    (fun { ret_type; fname; params; body; _ } ->
      PBox.tree
        (make_info_node_line
           (ast_typ ret_type
            ^ " "
            ^ ident_fix fname
            ^ " ("
            ^ String.concat
                ", "
                (List.map (fun (Param {name; tp; _}) -> ident_fix name ^ ":" ^ ast_typ tp) params)
            ^ ")"))
        (statement_seq_to_forest body))
    funcs
;;

  let structs_to_tree (records : Ast.record_decl list) =
    List.map
      (fun { name; fields; _ } ->
        PBox.tree
          (make_info_node_line ("struct " ^ ident_fix name))
          (List.map function_arg_to_tree fields))
      records
  ;;
  
  let real_prog_print prog =
    let func_decls, record_decls =
      List.partition_map
        (fun e ->
          match e with
          | Ast.Function e -> Left e
          | Ast.Record r -> Right r)
        prog
    in
    PBox.tree
      (make_info_node_line "Program")
      (structs_to_tree record_decls @ funcs_to_tree func_decls)
  ;;