(*expr2_repl.ml

  First attempt at a read-eval-print-loop.
*)

let prompt (continuing:bool)=
  (print_string (if (not continuing) then ">>> " else "... ");(flush stdout))  
let read (continuing:bool)=prompt continuing; input_line stdin
(*These next two functions modify the provided buffer in-place*)
let buf_reset (buf:Buffer.t)=flush stdout;Buffer.clear buf
let buf_append (buf:Buffer.t) (s:string) = Buffer.add_string buf s

(*Proxy for real evaluation - for now, just echo the phrase.*)
let eval_print buf env = (*env ignored for now, type will be ((string*value) list) ref *)
  ((print_string ((Buffer.contents buffer)^"\n"))

(*The  top-level.*)
let main =
  let _ = Printf.printf "Hi! You're in the loop! To exit type ^z.\n" in
  let initial_capacity = 4*1024 in 
  let buf = Buffer.create initial_capacity in
  let env = []
  in try
       while true do
	 let l = read ((Buffer.length buffer)!=0) in
	 let len = String.length l in
	 if len > 0 then
	   if (l.[len - 1] == '\\') then
	     (buf_append buf ((String.sub l 0 (len-1))^"\n"))
	   else 
             let _ = buf_append buf l in (eval_print buf env; (buf_reset buffer))
       done
    with 
    | End_of_file -> print_string "K, thx bye!"

