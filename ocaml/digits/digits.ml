module type S=sig
    (**Count the number of base 10 digits of an integer*)
    val digits : int -> int 
    (**  Prepend with zeroes as neccessary to get the 'right' number of
      colums e.g. *)
    val digits_10 : int -> int -> string

end

module A:S = struct 
  let digits n =
    let rec loop acc n =
      let k = n / 10 in
      if k = 0 then acc else loop (acc + 1) k
    in loop 1 n

  let digits_10 i n =
    let l = digits n in
    let s = string_of_int (abs n) in
    let maybe_sign = if n < 0 then "-" else "" in
    if l >= i then (maybe_sign^s) 
    else
      let lz=string_of_int 0 in
      let rec aux acc j = 
        if j = 0 then acc 
        else 
          aux (lz^acc) (j - 1) in
      maybe_sign ^ aux s (i - l)
end

module Test_io = struct

  let test_0 () = 
    let print_line i = 
      Printf.printf "There are %d digits in %d\n" (A.digits i) i
    in
    let rec loop i n =
      if i = n then () 
      else
        let x  =
          int_of_float (10. ** (float_of_int i)) in
        print_line x; loop (i + 1) n
    in (*Range over 0 to to 10^19 *) loop 0 19

  let test_1 () =
    let print_line i = 
      Printf.printf "%s\n" i
    in
    let rec loop i n =
      if i = n then () 
      else
        let x = A.digits_10 3 i in
        print_line x; loop (i + 1) n
    in (*Range over 0 to 100, 3 columns*)loop 0 100

  let run () = test_0 (); test_1 ()

end

let () = Test_io.run ()
