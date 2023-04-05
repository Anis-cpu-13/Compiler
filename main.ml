(* ocamlbuild -use-menhir main.byte *)

open Lexing
open Ast

let err msg pos =
  Printf.eprintf "Error on line %d col %d: %s.\n"
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol) msg ;
  exit 1

let () =
  if (Array.length Sys.argv) != 2 then begin
      Printf.eprintf "Usage: %s <file>\n" Sys.argv.(0) ;
      exit 1
    end;
  let f = open_in Sys.argv.(1) in
  let buf = Lexing.from_channel f in
  try
    let parsed = Parser.start Lexer.token buf in
    close_in f ;
    let ast, r = Semantics.analyze parsed in
    let simplified = Simplifier.simplify ast  in 
    let asm = Compiler.compile simplified r in 
    let oc =  open_out "result.s" in
    Mips.emit oc asm
  with
  | Lexer.Error c ->
     err (Printf.sprintf "unrecognized char '%c'" c) (Lexing.lexeme_start_p buf)
  | Parser.Error ->
     err "syntax error" (Lexing.lexeme_start_p buf)
  | Semantics.Error (msg, pos) ->
     err msg pos
