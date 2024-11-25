module Errors = Errors
module Codegen = Codegen
module Semant = Semant

let write_to_file (path: string) (contents: string) = 
  (* Create the full path with the 'testfiles' directory *)
  let dir = "tests/phase5test" in
  let full_path = Filename.concat dir path in

  (* Ensure the 'testfiles' directory exists, create it if it doesn't *)
  if not (Sys.file_exists dir) then Sys.mkdir dir 0o755;

  (* Write the file to the 'llvm_outputs' directory *)
  let oc = open_out full_path in
  Printf.fprintf oc "%s" contents;
  close_out oc
  

let normalise_line_endings (text: string) : string =
  String.split_on_char '\r' text |> String.concat ""

let compile_prog pathtofile = 
  let file_in = open_in pathtofile in 
  let lex_str = really_input_string file_in (in_channel_length file_in) in
  let normalized_str = normalise_line_endings lex_str in
  let lex_buf = Lexing.from_string normalized_str in
  let parse_res = Parser.main Lexer.read lex_buf in
  (* PrintBox_text.output stdout (Pretty.program_to_tree parse_res);  For printing parsetree*)
  (*let (typedStmt, _) = Semant.typecheck_prog parse_res in
  let llvm_prog = Codegen.codegen_prog typedStmt in
  let llvm_ir_string = Ll.string_of_prog llvm_prog in
  print_endline llvm_ir_string;
  write_to_file "test.ll" llvm_ir_string;*)
  exit 0

let _ =
  compile_prog "tests/phase5test/test1.dlp";
  