(*expr2_repl.ml

  First attempt at a read-eval-print loop
  (http://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop).

  The program makes use of the Buffer module for string buffers that
  automatically expand as necessary. It's an iterative program that
  exploits mutability.
*)

let prompt (continuing:bool)=
  (print_string (if (not continuing) 
    then "? " else "... ");(flush stdout))  
let read (continuing:bool)=prompt continuing; input_line stdin

let string_of_expr2 = fun e -> Expr2.string_of_expr2 e
let expr2_of_string = fun s ->
  let lexbuf = Lexing.from_string s
  in Expr2_parser.main Expr2_lexer.token lexbuf

(*Proxy for real evaluation*)
let eval_print buf env = 
  let e=expr2_of_string (Buffer.contents buf)
  in let s = string_of_expr2 e in
     print_string (s^"\n")

(*The  top-level.*)
let toplevel process =
  let initial_capacity = 4*1024 in 
  let buf = Buffer.create initial_capacity in
  let env(*:((string*value) list) ref*) = [] (*The top level environment is 
                                               necessarily mutable.*)
  in try
       let _ = Printf.printf "Hi! You're in the loop yo! To exit, type ^Z.\n" in
       while true do
	 try
	   (*Read*)
	   let l = read ((Buffer.length buf)!=0) in
	   let len = String.length l in
	   if len > 0 then
	     if (l.[len - 1] == '\\') then
	       (*Line continuation; append and keep reading*)
	       Buffer.add_string buf ((String.sub l 0 (len-1))^" ")
	     else 
	       (*Discard partial statements with ^G*)
	       if l.[len - 1] = (char_of_int 7) then Buffer.clear buf
	       else
		 (*Maybe the user doesn't know how to terminate?*)
		 if l = "exit" or l = "quit" or l.[0] = (char_of_int 3(*^C*)) then 
		   print_string "Type ^Z to finish\n"
		 else
		   (*We think we got a phrase.*)
		   let _ = Buffer.add_string buf l in 
		   (process buf env; (Buffer.clear buf))
	 with
	 | Parsing.Parse_error -> print_string "Syntax error\n"; (Buffer.clear buf)
       done
    with 
    | End_of_file -> print_string "K, thx bye!"

let main : unit = toplevel eval_print
