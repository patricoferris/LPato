{
  open Parser
  open Lexing 
  exception SyntaxError of string 
(* next_line copied from  Ch. 16 of "Real World Ocaml" *) 
let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

}

let newline = ('\r' | '\n' | "\r\n" | "\010" | "\013")
let ident_reg_exp = ['A'-'Z' 'a'-'z']+ ['0'-'9' 'A'-'Z' 'a'-'z' '_' '\'']* 
let int_reg_exp = ['0'-'9']+
let bool_reg_exp = ("true" | "false")

rule token = parse
  | [' ' '\t']+ { token lexbuf }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LCURLY }
  | '}' { RCURLY }
  | ',' { COMMA }
  | "+" { ADD }
  | "-" { SUB }
  | "*" { MULT }
  | "/" { DIV }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "==" { EQUALITY }
  | '<'  { LESS }
  | "&&" { AND }
  | "||" { OR }
  | ';'  { SEMI }
  | ";;" { DOUBLESEMI }
  | "let" { LET }
  | '='  { ASSIGN }
  | "=>" { ARROW }
  | "lambda" { LAMBDA }
  | "func" { FUNC } 
  | "end" { END }
  | int_reg_exp { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | bool_reg_exp { BOOLEAN (bool_of_string (Lexing.lexeme lexbuf)) }
  | ident_reg_exp { ID (Lexing.lexeme lexbuf) } 
  | newline { Lexing.new_line lexbuf; token lexbuf }
  | eof { EOF }
  | _ { raise (SyntaxError ("Unexpected Char: " ^ Lexing.lexeme lexbuf)) }
