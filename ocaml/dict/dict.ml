(*The type of a dictionary with keys of type ['a] and values of type
  ['b]*)
type ('a, 'b) dict = 'a -> 'b option

(*The empty dictionary maps every key to [None]*)
let empty (k : 'a) : 'b option = None

(*[add d k v] is the dictionary [d] together with a binding of [k] to
  [v]*)
let add (d : ('a, 'b) dict) (k : 'a) (v : 'b) : ('a, 'b) dict = 
  fun k' -> 
    if k' = k then Some v else d k'

(*[find d k] retrieves the value bound to [k]*)
let find (d : ('a, 'b) dict) (k : 'a) : 'b option = d k

(*e.g.

  Name                            | Age
  ================================+====
  "Felonius Gru"                  |  53
  "Dave the Minion"               | 4.54e9
  "Dr. Joseph Albert Nefario"     |  80

*)
let ages = 
  add 
    (add 
       (add 
          empty "Felonius Gru" 53
       ) 
       "Dave the Minion" (int_of_float 4.54e9)
    )
    "Dr. Nefario" 80 

let _ = 
  find ages "Dave the Minion" |> 
      function | Some x -> x | _ -> failwith "Not found"
