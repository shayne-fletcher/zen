open Regexp

let test xpr s = 
  let result = xpr s in
  match result with
  | Lexical_analysis.Recognizer.Remains [] -> Printf.printf "\"%s\" : success\n" s
  | _ -> Printf.printf "\"%s\" : fail\n" s

let _ = 
  try 
    let xpr = compile "(a|b)*abb" in
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
