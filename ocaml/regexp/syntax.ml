module Int_set : Set.S = Set.Make (
  struct
    type t = int
    let compare = Pervasives.compare
  end)

type regular_expression =
  | Epsilon
  | Character of char * int
  | Sequence of regular_expression * regular_expression * pos
  | Alternative of regular_expression * regular_expression * pos
  | Repetition of regular_expression * pos
  | Accept of int
   and pos =
     {
       null : Int_set.t;
       first : Int_set.t;
       last : Int_set.t;
     }
