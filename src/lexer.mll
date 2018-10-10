{
  open Core
  open Stdio

  open Lexing
  open Parser

  open Var

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

let ext        = [ '+' ]

(**********************************
 ***** Allyn Lexical Analysis *****
 **********************************)

rule token = parse
  (** Whitespace *)
  | whitespace { token lexbuf }

  (** Newline *)
  | newline    { new_line lexbuf; token lexbuf }

  (** Def *)
  | "def"      { TDEF }

  (** Extern *)
  | "extern"   { TEXTERN }

  (** Number *)
  | number     { TNUM (Float.of_string (lexeme lexbuf)) }

  (** Variable *)
  | var        { TVAR (Var (lexeme lexbuf)) }

  (** Ext *)
  | ext        { TEXT (lexeme lexbuf) }

  (** Comment *)
  | "#"        { comment (lexeme_start_p lexbuf); token lexbuf }

  (** EOF *)
  | eof        { TEOF }

  (** Failure *)
  | _          { raise (SyntaxError (lexeme_start_p lexbuf, "Unexpected token.")) }

and comment pos_inner = parse
  | newline    { () }
  | _          { comment pos_inner lexbuf }