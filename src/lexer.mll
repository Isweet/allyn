{
  open Core

  open Lexing
  open Parser

  exception SyntaxError of Position.t * String.t
}

let whitespace = [' ' '\t']
let newline    = ('\n' | '\r' '\n')

let letter     = ['a'-'z' 'A'-'Z']
let digit      = ['0'-'9']

(* TODO(ins): Should be official OCaml float regex,
              so that `Float.to_string` is guaranteed to succeed. *)
let number     = (digit | '.')+
let var        = letter (letter | digit | [''' '_'])*

(**********************************
 ***** Allyn Lexical Analysis *****
 **********************************)

rule token = parse
  (** Whitespace *)
  | whitespace { token lexbuf }

  (** Newline *)
  | newline    { new_line lexbuf; token lexbuf }

  (** Def *)
  | "def"      { DEF }

  (** Extern *)
  | "extern"   { EXTERN }

  (** Number *)
  | number     { NUM (Float.of_string (lexeme lexbuf)) }

  (** Variable *)
  | var        { VAR (lexeme lexbuf) }

  (** PARENS *)
  | "("        { LPAREN }
  | ")"        { RPAREN }

  (** COMMA *)
  | ","        { COMMA }

  (** Binary Arithmetic Operators *)
  | "+"        { PLUS }

  (** Binary Arithmetic Relations *)
  | "<"        { LT }

  (** Comment *)
  | "#"        { comment (lexeme_start_p lexbuf) lexbuf; token lexbuf }

  (** EOF *)
  | eof        { EOF }

  (** Failure *)
  | _          { raise (SyntaxError (lexeme_start_p lexbuf, "Unexpected token.")) }

and comment pos_inner = parse
  | newline    { () }
  | _          { comment pos_inner lexbuf }