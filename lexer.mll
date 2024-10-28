{

exception Err of string

open Parser

}

rule token = parse
| eof      {EOF}
| ('0' | (['1'-'9']['0'-'9']*)) as i  {INT (int_of_string i)}
| '+'   {PLUS}
| '-'   {MINUS}
| '*'   {TIMES}
| '/'   {DIVIDE}
| '('   {LPAREN}
| ')'   {RPAREN}
| [' ' '\t']   { token lexbuf}
| '\n'         {Lexing.new_line lexbuf; token lexbuf}
| "//"         {comment lexbuf }
| _     { raise (Err "unexpected character!\n") }

rule comment = parse
|