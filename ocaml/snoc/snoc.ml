type 'a snoc_list = 
   | Empty | Snoc of 'a snoc_list * 'a

let hd : 'a snoc_list -> 'a = function 
  | Empty -> raise (Failure "hd") | Snoc (_, s) -> s

let tl : 'a snoc_list -> 'a snoc_list = function
  | Empty -> raise (Failure "tl") | Snoc (l, _) -> l

let rec list_of_snoc_list : 'a snoc_list -> 'a list = function
  | Empty -> [] | Snoc (t, h) -> h :: list_of_snoc_list t

let rec snoc_list_of_list : 'a list -> 'a snoc_list = function
  | [] -> Empty | (h :: t) -> Snoc (snoc_list_of_list t, h)
