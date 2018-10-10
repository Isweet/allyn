open Core

type 'a annotated =
  { loc  : Section.t Option.t
  ; base : 'a
  }

type expr = expr' annotated

and expr' =
  | Number   of { value : Float.t }
  | Variable of { value : String.t }
  | ABinOp   of { op : Arith.Bin.Op.t
                ; lhs : expr
                ; rhs : expr
                }
  | ABinRel  of { op : Arith.Bin.Rel.t
                ; lhs : expr
                ; rhs : expr
                }
  | Call     of { value : string
                ; args : expr Array.t
                }

type proto' =
  { name   : String.t
  ; params : String.t Array.t
  }

type proto = proto' annotated

type func' =
  { header : proto
  ; body   : expr
  }

type func = func' annotated

type top =
  | Expression of { value : expr }
  | Extern     of { value : proto }
  | Definition of { value : func }

type prog = top List.t
