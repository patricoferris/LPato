(*** FRONT FOR LPATO ***)
open Core
open Lexer
open Lexing

(* Using Lexing to Lex the input *)

let print_pos outx lexbuf = 
  let pos = lexbuf.lex_curr_p in 
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf = 
  try Parser.prog Lexer.token lexbuf with 
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_pos lexbuf msg;
    exit(-1)
  | Parser.Error -> 
    fprintf stderr "%a: syntax error" print_pos lexbuf;
    exit(-1)

let parse_and_print lexbuf =
  let e = parse_with_error lexbuf in
  let _ = print_string "🦆: \n" in
    Types.print_expr e

let front_end filename = 
  let inx = In_channel.create filename in 
  let lexbuf = Lexing.from_channel inx in 
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_with_error lexbuf
  
  (*parse_and_print lexbuf;
  In_channel.close inx*)

(* let () =
  Command.basic_spec ~summary:"Parsing the Flang!"
    Command.Spec.(empty +> anon ("filename" %: file))
    loop
  |> Command.run *)
