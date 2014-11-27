module Int_set : Set.S = Set.Make (
  struct
    type t = int
    let compare = Pervasives.compare
  end)

type regular_expression =
  | Epsilon
  | Character of char * int
  | Sequence of regexpr * regexpr * pos
  | Alternative of regexpr * regexpr * pos
  | Repetition of regexpr * pos
  | Accept of int
   and pos =
     {
       null : Int_set.t;
       first : Int_set.t;
       last : Int_set.t;
     }
