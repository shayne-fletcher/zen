let test () =
  let s = input_line stdin in
  let xpr = Regexp.compile s in
  print_endline @@ "Pattern : \""^s^"\"";
  while true do
    let buf = input_line stdin in
    match Regexp.re_match xpr buf with
    | true -> print_endline @@ "\""^buf^"\" : success"
    | false -> print_endline @@ "\""^buf^"\" : fail"
  done
    
let _ =  
  try
    test ()
  with
  | End_of_file -> ()
  | Failure msg -> print_endline msg

(*

let test xpr s = 
  match Regexp.re_match xpr s with
  | true -> Printf.printf "\"%s\" : success\n" s
  | false -> Printf.printf "\"%s\" : fail\n" s

let _ = 
  try 
    let xpr = Regexp.compile "(a|b)*abb" in
    Printf.printf "Pattern: \"%s\"\n" "(a|b)*abb" ;
    test xpr "abb" ;
    test xpr "aabb" ;
    test xpr "baabb" ;
    test xpr "bbbbbbbbbbbbbaabb" ;
    test xpr "aaaaaaabbbaabbbaabbabaabb" ;
    test xpr "baab" ;
    test xpr "aa" ;
    test xpr "ab" ;
    test xpr "bb" ;
    test xpr "" ;
    test xpr "ccabb" ;
  with 
  | Failure msg -> print_endline msg
*)
