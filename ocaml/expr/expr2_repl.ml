(*expr2_repl.ml

  First attempt at a read-eval-print loop
  (http://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop).

  The program makes use of the Buffer module for string buffers that
  automatically expand as necessary. It's an iterative program relying
  heavily on side-effects (mentioning that so's that you might keep it
  in mind as you read).

*)

let prompt (continuing:bool)=
  (print_string (if (not continuing) 
    then ">>> " else "... ");(flush stdout))  
let read (continuing:bool)=prompt continuing; input_line stdin
(*These next two functions modify the provided buffer in-place.*)
let buf_reset (buf:Buffer.t)=flush stdout;Buffer.clear buf
let buf_append (buf:Buffer.t) (s:string) = Buffer.add_string buf s

(*Proxy for real evaluation - for now, just echo the phrase.*)
let eval_print buf env = print_string ((Buffer.contents buf)^"\n")
(*Note that env is ignored for now. It's type will be ((string*value)
  list) ref *)

(*The  top-level.*)
let main =
  let _ = Printf.printf "Hi! You're in the loop! To exit, type ^Z.\n" in
  let initial_capacity = 4*1024 in 
  let buf = Buffer.create initial_capacity in
  let env(*:((string*value) list) ref*) = [] (*The top level environment is 
                                               necessarily mutable.*)
  in try
       while true do
	 let l = read ((Buffer.length buf)!=0) in
	 let len = String.length l in
	 if len > 0 then
	   if (l.[len - 1] == '\\') then
	     (buf_append buf ((String.sub l 0 (len-1))^" "))
	   else 
             let _ = buf_append buf l in (eval_print buf env; (buf_reset buf))
       done
    with 
    | End_of_file -> print_string "K, thx bye!" (*We're out of here.*)

(* That's all... We're done.

  Here's an example session illustrating what this program does to
  date:

  C:\expr2>.\expr2_repl
  Hi! You're in the loop! To exit type ^Z.
  >>> (max(L - K, 0.0)*T
  (max(L - K, 0.0)*T
  >>> (max(L - K,\
  ... 0.0)*T)
  (max(L - K, 0.0)*T)
  >>> 
  K, thx bye!

*)
