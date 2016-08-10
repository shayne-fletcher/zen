type rec_flag = Nonrecursive | Recursive

type unop = Unop_fst | Unop_snd
type binop = Binop_add | Binop_sub | Binop_mul | Binop_eq | Binop_less

type 'a loc = 'a Ml_location.loc = {
  txt : 'a;
  loc : Ml_location.t;
}

