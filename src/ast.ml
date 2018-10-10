open Core

type t =
  | Number   of { value : Float.t }
  | Variable of { value : String.t }
  | Binary   of { op : Boolean.Binary.Op.t
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
