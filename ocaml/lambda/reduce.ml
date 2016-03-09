(*Evaluate a term using a call-by-value strategy resembling OCaml. Its
  result need not be in normal form*)
let rec eval : Lambda.t -> Lambda.t = function
  | Lambda.Apply (t1, t2) ->
    begin
      match eval t1 with
      | Lambda.Abs (a, u) -> eval (Lambda.subst 0 (eval t2) u)
      | _ as u1 -> Lambda.Apply (u1, eval t2)
    end
  | _ as t -> t
