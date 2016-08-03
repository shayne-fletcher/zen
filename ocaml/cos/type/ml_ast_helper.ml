open Ml_types

let default_loc = ref none

module Pat = struct
  let mk ?(loc = !default_loc) d = {ppat_desc = d; ppat_loc = loc;}    

end

module Exp = struct
  let mk ?(loc = !default_loc) d = { pexp_desc = d; pexp_loc = loc; }

end

module Vb = struct
  let mk ?(loc = !default_loc) pat expr =
    {
     pvb_pat = pat;
     pvb_expr = expr;
     pvb_loc = loc;
    }

end
