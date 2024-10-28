{

exception Err of string

open Parser

}

rule token = parse
| eof      {EOF}
(*| ('0' | (['1'-'9']['0'-'9']* as i  {INT } *)
| "int" {INT}
| '+'   {PLUS}
| '-'   {MINUS}
| '*'   {MUL}
| '/'   {DIV}
| '('   {LPAREN}
| ')'   {RPAREN}
| [' ' '\t']   { token lexbuf}
| '\n'         {Lexing.new_line lexbuf; token lexbuf}
(*| "//"         {comment lexbuf }*)
| _     { raise (Err "unexpected character!\n") }

(*rule comment = parse
| _     { raise (Err "unexpected character!\n") }
*)