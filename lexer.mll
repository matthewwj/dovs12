{

exception Err of string

open Parser

}

let digit = ['0'-'9']
let digits = digit+
let letter = ['a'-'z'] | '_'
let ident = letter (letter | digit)*
let non_escaped_char = [^ '\\' '"'] 
let escape_sequence = 
  '\\' [ '"' 'n' 't' 'r' 'b' 'f' ]
let string_char = non_escaped_char | escape_sequence

let string_lit = '"' (string_char)* '"'


rule read = parse
  | eof      {EOF}
  | string_lit as string { STRING_LIT (Scanf.unescaped (String.sub string 1 (String.length string - 2))) }
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
  | "(" {LPAREN}
  | ")" {RPAREN}
  | "," {COMMA}
  | ident as x { IDENT x }
  | digits as e { INT_LIT (Int64.of_string e) }
  | "/*" {comment 1 false lexbuf}
  | "//" {comment 0 true lexbuf}
  | "+" {PLUS}
  | "-" {MINUS}
  | "*" {MUL}
  | "/" {DIV}
  | "%" {REM}
  | "!" {LNOT}
  | "<" {LT}
  | ">" {GT}
  | "=" {ASSIGN}
  | "{" {LBRACE}
  | "}" {RBRACE}
  | "[" {LBRACKET}
  | "]" {RBRACKET}
  | "." {DOT}
  | "?" {QUESTIONMARK} 
  | ":" {COLON}
  | ";" {SEMICOLON}
  | " " {read lexbuf}
  | "\n" {Lexing.new_line lexbuf; read lexbuf}
  | _ as e {failwith ("no match for: " ^ (String.make 1 e))}

and comment nestingLevel line_com = parse
  | "/*" { comment (nestingLevel + 1) line_com lexbuf }
  | "*/" { 
      if nestingLevel = 1 then 
        read lexbuf 
      else 
        comment (nestingLevel - 1) line_com lexbuf 
    }
  | "//" {comment nestingLevel true lexbuf}
  | "\n" { 
      if nestingLevel = 0 then read lexbuf else comment nestingLevel false lexbuf
    }
  | "\r" { comment nestingLevel line_com lexbuf }  
  | eof  { failwith "Broken comment" }
  | _   { comment nestingLevel line_com lexbuf }

and strings sBuf = parse
| '"' { STRING_LIT (Buffer.contents sBuf) }
| "\\" (ascii_digits as e) {Buffer.add_char sBuf ((Char.chr (int_of_string e))); strings sBuf lexbuf} 
| "\\n" {Buffer.add_char sBuf '\n'; strings sBuf lexbuf}                                              
| "\\t" {Buffer.add_char sBuf '\t'; strings sBuf lexbuf}       
| "\\b" {Buffer.add_char sBuf '\b'; strings sBuf lexbuf}        
| "\\r" {Buffer.add_char sBuf '\r'; strings sBuf lexbuf}                                   
| "\\" ('"' as e) {Buffer.add_char sBuf e; strings sBuf lexbuf}
| "\\" ("\\" as e) {Buffer.add_char sBuf e; strings sBuf lexbuf}
| "\\" ("'" as e) {Buffer.add_char sBuf e; strings sBuf lexbuf}
| eof { failwith "String not closed" }
| _ as e {Buffer.add_char sBuf e; strings sBuf lexbuf }

