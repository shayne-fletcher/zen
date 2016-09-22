let parse 
    (pe : 'a -> Lexing.lexbuf -> 'b) 
    (le : 'a)
    (lexbuf : Lexing.lexbuf)  : 'b =
  try
    pe le lexbuf
  with
  | Parsing.Parse_error ->
    let loc = Ml_location.curr lexbuf in
    raise (Ml_syntaxerr.Error (Ml_syntaxerr.Other loc))
  | x -> raise x

let from_bytes ?(file : string = "<string>") (str : string) 
    (pe : 'a -> Lexing.lexbuf -> 'b) 
    (le : 'a)
     =
  let set_filename lexbuf name =
    let open Lexing in
    lexbuf.lex_curr_p <-  {
      lexbuf.lex_curr_p with pos_fname = name
    } in
  let lexbuf = Lexing.from_string str in
  set_filename lexbuf file ;
  parse pe le lexbuf

let prompt (continuing:bool) =
  (print_string (if (not continuing)
    then "? " else "... ");(flush stdout))
let read (continuing:bool)=prompt continuing; input_line stdin

let handle_interpreter_error ?(finally=(fun () -> ())) ex =
  finally ();
  Ml_location.report_exception (Format.std_formatter) ex

let safe_proc ?finally f =
  try f ()
  with exn -> handle_interpreter_error ?finally exn

let reduce 
    (eval : 'b -> 'c)
    (pe : 'd -> Lexing.lexbuf -> 'b)
    (le : 'd)
    (to_bytes : 'c -> 'e) 
    (buf : Buffer.t) : 'e =
  let t = 
    eval (from_bytes (Buffer.contents buf) pe le) in
  to_bytes t

let repl
    (eval : 'b -> 'c)
    (pe : 'd -> Lexing.lexbuf -> 'b)
    (tok : 'd)
    (to_bytes : 'c -> 'e) : unit = 
  let initial_capacity = 4*1024 in
  let buf = Buffer.create initial_capacity in
  try 
    while true do
      let f () =
        let l = read ((Buffer.length buf)!=0) in
        let len = String.length l in
        if len > 0 then
          if l.[0] = '%' then ()
          else
            if l.[len - 1] = '\\' then
              (Buffer.add_string buf ((String.sub l 0 (len-1))^"\n"))
            else
              if l.[len-1] = (char_of_int 7) then Buffer.clear buf
              else
                let _ = Buffer.add_string buf l in
                let s = reduce eval pe tok to_bytes buf in
                Buffer.clear buf; print_endline s
      in (safe_proc ~finally:(fun () -> Buffer.clear buf) f)
    done
  with
  | End_of_file -> print_string "\n"
