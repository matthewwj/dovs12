module Errors = Errors
module Codegen = Codegen
module Semant = Semant
(*
let _ = 
  let file_in = open_in "/workspaces/dovs/phase1/dovs12/input.txt"
 in 
  let lex_buf = Lexing.from_channel file_in in 
  let parse_res = Parser.main Lexer.read lex_buf in
  PrintBox_text.output stdout (Pretty.program_to_tree parse_res)

(*let parse_from_file ?(with_positions = true) file_name =
  let file_in = open_in file_name in
  let lex_buf = Lexing.from_channel ~with_positions file_in in
  try Parser.main Lexer.read lex_buf with
  | Parser.Error ->
    let c = lex_buf.lex_curr_p.pos_cnum - lex_buf.lex_curr_p.pos_bol in
    let err =
      "Syntax Error, Line "
      ^ string_of_int lex_buf.lex_curr_p.pos_lnum
      ^ ", coloum "
      ^ string_of_int c
      ^ ", error in: \""
      ^ Bytes.sub_string lex_buf.lex_buffer lex_buf.lex_curr_p.pos_bol c
      ^ "\""
    in
    raise @@ Errors.ParseErr err
;;*)
*)
let _ = 
  let file_in = open_in "/Users/matthewwestergaard/Desktop/dovs/compiler/input1.txt" in 
  let lex_buf = Lexing.from_channel file_in in 
  let parse_res = Parser.main Lexer.read lex_buf in
  let (typedStmt, env) = Semant.typecheck_prog parse_res in
  PrintBox_text.output stdout (Pretty.program_to_tree parse_res);
  let llvm_prog = Codegen.codegen_prog typedStmt in
  let llvm_ir_string = Ll.string_of_prog llvm_prog in
  let output_filename = "output.ll" in
  Codegen.write_to_file output_filename llvm_ir_string;
  print_endline ("Writing LLVM IR to " ^ output_filename);
  exit 0
