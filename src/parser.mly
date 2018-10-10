%{
  open Core

  open Parsing

  open Ast

  (* exception SyntaxError of Position.t * String.t *)

  let annotate e =
    { loc  = Some ({ l_pos = symbol_start_pos ()
                   ; r_pos = symbol_end_pos ()
                   })
    ; node = e
    }
%}

/**************************
 **** TOKEN DEFINITIONS ****
 ***************************/

/** Def */
%token DEF

/** Extern */
%token EXTERN

/** Number */
%token <Float.t> NUM

/** Variable */
%token <String.t> VAR

/** Parens */
%token LPAREN
%token RPAREN

/** Binary Arithmetic Operators */
%token PLUS

/** Binary Arithmetic Relations */
%token LT

/** EOF */
%token EOF

/*********************************
 **** PARSER ENTRY PRODUCTION ****
 *********************************/

%start expr
%type <Ast.t> expr

/***********************
 **** ALLYN PARSING ****
 ***********************/

%%
expr :
  | NUM { annotate (Number { value = $1 }) }
  | LPAREN expr RPAREN { $2 }
  | VAR { annotate (Variable { value = $1 }) }