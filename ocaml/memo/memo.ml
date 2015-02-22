let memo f =
  let m = ref [] in
    fun x ->
      try
        List.assoc x !m
      with
      | Not_found ->
        let y = f x in
        m := (x, y) :: !m;
        y

let memo_rec f =
  let m = ref [] in
  let rec g x =
    try
      List.assoc x !m
    with
    | Not_found ->
      let y = f g x in
      m := (x, y) :: !m;
      y
  in g

let rec fib self = function
  | 0 -> 1
  | 1 -> 1
  | n -> self (n - 1) + self (n - 2)

let fib_memoized = memo_rec fib
