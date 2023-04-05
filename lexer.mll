{
  open Lexing
  open Parser

  exception Error of char
  exception StrEndError of string
}

let alpha = ['a'-'z' 'A'-'Z']
let num = ['0'-'9']
let identifier = alpha (alpha | num | '-' | '_')*

rule token = parse
| eof             { Lend }
| [ ' ' '\t' ]    { token lexbuf }
| '\n'            { Lexing.new_line lexbuf; token lexbuf }
| "//"            { comment lexbuf }
| "/*"            { long_comment lexbuf }
| '"'             { Lstring (String.concat "" (string_r lexbuf))}
| ";"             { Lsc }

| "true"          { Lbool true }
| "false"         { Lbool false }

| "return"        { Lreturn }
| "="             { Lassign }

| "+"             { Ladd }
| "-"             { Lsub }
| "*"             { Lmul }
| "/"             { Ldiv }
| "%"             { Lmod }

| ">"             { Lgt }
| ">="            { Lgte }
| "<"             { Llt }
| "<="            { Llte }

| "=="            { Leq }
| "!="            { Lneq }

| "&&"            { Land }
| "||"            { Lor }



| "if"            { Lif }
| "else"          { Lelse }
| "while"         { Lwhile }

| "{"             { Locbra }
| "}"             { Lccbra }
| "("             { Lopar }
| ")"             { Lcpar }

| "int"           { Ltype (Int_t) }
| "str"           { Ltype (Str_t) }
| "bool"          { Ltype (Bool_t) }
| "void"          { Ltype (Nil_t) }

| "print_str"     { Lprint_str}
| "print_int"     { Lprint_int}
| "print_bool"    { Lprint_bool}
| "print_nl"      { Lprint_nl}

| identifier as i { Lvar i}
| num+ as n       { Lint (int_of_string n)}
| _ as c          { raise (Error c) }
and string_r = parse
| eof             { raise (StrEndError "Missing '") }
| '"'             { [] }
| "\\n"           { "\n" :: (string_r lexbuf) }
| "\\t"           { "\t" :: (string_r lexbuf) }
| "\\\""          { "\"" :: (string_r lexbuf) }
| "\\\\"          { "\\" :: (string_r lexbuf) }
| _ as c          { (String.make 1 c) :: (string_r lexbuf) }
and comment = parse
| eof  { Lend }
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| _    { comment lexbuf }
and long_comment = parse
| eof  { Lend }
| "*/" { Lexing.new_line lexbuf; token lexbuf }
| _    { long_comment lexbuf }