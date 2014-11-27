module Int_set : Set.S = Set.Make (
  struct
    type t = int
    let compare = Pervasives.compare
  end)

type int_set = Int_set.t

type regexpr =
  | Epsilon
  | Symbol of char * int
  | Star of regexpr * pos
  | Or of regexpr * regexpr * pos
  | Seq of regexpr * regexpr * pos
  | Accept of int
   and pos =
     {
       null : bool;
       first : Int_set.t;
       last : Int_set.t;
     }
