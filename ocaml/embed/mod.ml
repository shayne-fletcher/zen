let rec fib : int -> int = 
  fun n ->
    if n < 2 then 1 
    else fib (n-1) + fib (n-2)

let format_result : int -> string =
  fun n ->
    Printf.sprintf "Result is %d\n" n

(* Register named closures for C clients. *)

let _ = Callback.register "fib" fib
and _ = Callback.register "format_result" format_result 
