module Errors = Errors
module Codegen = Codegen
module Semant = Semant

let write_to_file (path: string) (contents: string) = 
  (* Create the full path with the 'testfiles' directory *)
  let dir = "llvm_outputs" in
  let full_path = Filename.concat dir path in

  (* Ensure the 'testfiles' directory exists, create it if it doesn't *)
  if not (Sys.file_exists dir) then Sys.mkdir dir 0o755;

  (* Write the file to the 'llvm_outputs' directory *)
  let oc = open_out full_path in
  Printf.fprintf oc "%s" contents;
  close_out oc


let compile_prog pathtofile = 
  let file_in = open_in pathtofile in 
  let lex_buf = Lexing.from_channel file_in in 
  let parse_res = Parser.main Lexer.read lex_buf in
  let (typedStmt, _) = Semant.typecheck_prog parse_res in
  (*PrintBox_text.output stdout (Pretty.program_to_tree parse_res);*)  (* For printing parsetree*)
  let llvm_prog = Codegen.codegen_prog typedStmt in
  let llvm_ir_string = Ll.string_of_prog llvm_prog in
  print_endline llvm_ir_string;
  exit 0

let _ =
  compile_prog "tests/oldTests/OT1.dlp";