{

exception Err of string

open Parser

}

let digit = ['0'-'9']
let digits = digit+
let letter = ['a'-'z'] | '_'
let ident = letter (letter | digit)*


rule read = parse
| eof      {EOF}
| "true" {TRUE}
| "false" {FALSE}
| "<=" {LE}
| ">=" {GE}
| "==" {EQ}
| "!=" {NEQ}
| "nil" {NIL}
| "var" {VAR}
| "let" {LET}
| "if" {IF}
| "else" {ELSE}
| "while" {WHILE}
| "for" {FOR}
| "break" {BREAK}
| "continue" {CONTINUE}
| "return" {RETURN}
| "new" {NEW}
| "int" {INT}
| "bool" {BOOL}
| "string" {STRING}
| "byte" {BYTE}
| "void" {VOID}
| "record" {RECORD}
| "length_of" {LENGTHOF}
| "||" {LOR}
| "&&" {LAND}
| "\n" {Lexing.new_line lexbuf ; read lexbuf}
| "/*" {comment 1 false lexbuf}
| "//" {comment 0 true lexbuf}
| '+' { PLUS }
| '-' { MINUS }
| '*' { MUL }
| '/' { DIV }
| '%' { REM }
| '!' { LNOT }
| '<' { LT }
| '>' { GT }
| '=' { ASSIGN }
| '(' { LPAREN }
| ')' { RPAREN }
| '{' { LBRACE }
| '}' { RBRACE }
| '[' { LBRACKET }
| ']' { RBRACKET }
| '.' { DOT }
| '?' { QUESTIONMARK } 
| ':' { COLON }
| ';' { SEMICOLON }
| ',' { COMMA }
  | ident as e { IDENT e }
  | digits as e { INT_LIT (Int64.of_string e) }
  (* | "(-" digits as e ')' { INT_LIT (Int64.of_string e) } *)
  | ' ' { read lexbuf }
  | _ as e {failwith ("no match for: " ^ (String.make 1 e)) }

and comment nestingLevel line_com = parse 
| "/*" { comment (nestingLevel + 1) line_com lexbuf }
| "*/" { 
	if nestingLevel = 1 then 
	  read lexbuf 
	else 
	  comment (nestingLevel - 1) line_com lexbuf 
       }
| "//" {comment nestingLevel true lexbuf}
| "\n" { if nestingLevel = 0 then read lexbuf else comment nestingLevel false lexbuf}
| eof  { failwith "Broken comment" }
| _   { comment nestingLevel line_com lexbuf } 