%{
  open Core

  open Syntax

  exception SyntaxError of Position.t * String.t

  let annotate b left right =
    { loc  = Some ({ l_pos = left
                   ; r_pos = right
                   })
    ; base = b
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

/** Comma */
%token COMMA

/** Binary Arithmetic Operators */
%token PLUS

/** Binary Arithmetic Relations */
%token LT

/** EOF */
%token EOF

/*********************************
 **** PARSER ENTRY PRODUCTION ****
 *********************************/

%start start
%type <Syntax.prog> start

%nonassoc IDENT
%nonassoc LPAREN

%left LT
%left PLUS

/***********************
 **** ALLYN PARSING ****
 ***********************/

%%
start :
  | prog EOF { $1 }
;

prog :
  |                 { [ ] }
  | expr       prog { (Expression { value = $1 }) :: $2 }
  | extern     prog { (Extern     { value = $1 }) :: $2 }
  | definition prog { (Definition { value = $1 }) :: $2 }
;

expr :
  | VAR %prec IDENT        { annotate (Variable { value = $1 }) $symbolstartpos $endpos }

  | VAR LPAREN args RPAREN { annotate (Call     { value = $1
                                                ; args  = Array.of_list $3
                                                }) $symbolstartpos $endpos }

  | NUM                    { annotate (Number   { value = $1 }) $symbolstartpos $endpos }

  | expr PLUS expr         { annotate (ABinOp   { op    = Arith.Bin.Op.Add
                                                ; lhs   = $1
                                                ; rhs   = $3
                                                }) $symbolstartpos $endpos }

  | expr LT expr           { annotate (ABinRel  { op    = Arith.Bin.Rel.LessThan
                                                ; lhs   = $1
                                                ; rhs   = $3
                                                }) $symbolstartpos $endpos }
  | LPAREN expr RPAREN     { $2 }

  | error                  { raise (SyntaxError ($symbolstartpos, "Expected an expression.")) }
;

args :
  |                 { [ ] }
  | expr            { [ $1 ] }
  | expr COMMA args { $1 :: $3 }
;

extern :
  | EXTERN prototype { $2 }
;

prototype :
  | VAR LPAREN params RPAREN { annotate { name   = $1
                                        ; params = Array.of_list $3
                                        } $symbolstartpos $endpos }
;

params :
  |                  { [ ] }
  | VAR              { [ $1 ] }
  | VAR COMMA params { $1 :: $3 }
;

definition :
  | DEF func { $2 }
;

func :
  | prototype expr { annotate { header = $1
                              ; body   = $2
                              } $symbolstartpos $endpos }