let fortune f g =
  let bc = open_in_bin g in
  try
    ignore (input_binary_int bc); (*version*)
    let numstrs = input_binary_int bc in
    ignore (input_binary_int bc); (*longest*)
    ignore (input_binary_int bc); (*shortest*)
    ignore (input_binary_int bc); (*flags*)
    Random.self_init ();
    seek_in bc (((5 + Random.int numstrs)) * 4);
    let spos = input_binary_int bc in
    let epos = input_binary_int bc in
    let numchars = epos - spos - 2 in
    close_in bc;
    let ic = open_in f in
    try
      seek_in ic spos;
      let s = String.create numchars in
      really_input ic s 0 numchars;
      close_in ic;
      Printf.printf "%s" s
    with e ->
      close_in_noerr ic;
      raise e
  with
  |  _ -> close_in_noerr bc

let () = fortune "fortunes" "fortunes.dat"
