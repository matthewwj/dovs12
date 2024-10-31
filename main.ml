


let _ = 
  let file_in = open_in "/Users/matthewwestergaard/Desktop/dovs/compiler/input.txt"
 in 
  let lex_buf = Lexing.from_channel file_in in 
  let parse_res = Parser.main Lexer.read lex_buf in
  PrintBox_text.output stdout (Pretty.program_to_tree parse_res)


 (*PrintBox_text.output stdout (Dolphin.Pretty.program_to_tree parse_res) ---> print parse tree of the program*)