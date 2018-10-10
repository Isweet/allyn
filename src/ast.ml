open Core

type t =
  { loc  : Section.t Option.t
  ; node : t'
  }

and t' =
  | Number   of { value : Float.t }
  | Variable of { value : String.t }
  | ABinOp   of { op : Arith.Bin.Op.t
                ; lhs : t
                ; rhs : t
                }
  | ABinRel  of { op : Arith.Bin.Rel.t
                ; lhs : t
                ; rhs : t
                }
  | Call     of { value : string
                ; args : t Array.t
                }

type proto =
  { name   : String.t
  ; params : String.t Array.t
  }

type func =
  { header : proto
  ; body   : t
  }
