open Ml_astttypes
open Ml_types

type partial = | Partial | Total

(*Core language*)

type pattern = {
  pat_desc : pattern_desc;
  pat_loc : Ml_location.t;
  pat_extra : (pat_extra * Location.t) list;
  pat_type : type_expr;
  mutable pat_env : Env.t
}

  
