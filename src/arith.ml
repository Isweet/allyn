
module Un = struct
  module Op = struct
    type t = unit
  end
  module Rel = struct
    type t = unit
  end
end

module Bin = struct
  module Op = struct
    type t =
      | Add
      | Subtract
      | Mult
  end
  module Rel = struct
    type t =
      | Equal
      | LessThan
  end
end
