let to_bytes : Types.Lambda.t -> string = 
  Types.Lambda.string_of_lambda

let parse (lexbuf : Lexing.lexbuf) : Types.Lambda.t =
  try 
    Parser.main Lexer.token lexbuf
  with 
  | Parsing.Parse_error ->
    begin
      let curr = lexbuf.Lexing.lex_curr_p in
      let line = curr.Lexing.pos_lnum in
      let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
      let tok = Lexing.lexeme lexbuf in
      raise 
        (Failure
           (Printf.sprintf 
              "file \"\", line %d, character %d\nError : Syntax error \"%s\"" line cnum tok))
    end

let from_bytes ?(file : string = "<string>") (str : string) : Types.Lambda.t =
  let set_filename lexbuf name =
    let open Lexing in
    lexbuf.lex_curr_p <-  {
      lexbuf.lex_curr_p with pos_fname = name
    } in
  let lexbuf = Lexing.from_string str in
  set_filename lexbuf file ;
  parse lexbuf

let prompt (continuing:bool) =
  (print_string (if (not continuing)
    then "? " else "... ");(flush stdout))
let read (continuing:bool)=prompt continuing; input_line stdin

let handle_interpreter_error ?(finally=(fun () -> ())) ex =
  match ex with
  | Failure s -> finally () ; (Printf.printf "%s\n" s)
  | Stack_overflow -> finally () ; Printf.printf "Stack overflow\n"
  | Division_by_zero -> finally () ; Printf.printf "Division by zero\n"
  | End_of_file -> finally (); raise ex
  | _  as e -> finally (); Printf.printf "Unknown exception : %s\n" (Printexc.to_string e) ; raise e

let safe_proc ?finally f =
  try f ()
  with exn -> handle_interpreter_error ?finally exn

let eval_show buf =
  to_bytes (Types.Lambda.eval1 [] (from_bytes (Buffer.contents buf)))

let main =
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
                let s = eval_show buf in 
                Buffer.clear buf; print_endline s
      in (safe_proc ~finally:(fun () -> Buffer.clear buf) f)
    done
  with
  | End_of_file -> print_string "\n"


