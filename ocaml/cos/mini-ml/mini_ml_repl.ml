(*
  read-eval-print loop
  (http://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop).

  The program makes use of the Buffer module for string buffers that
  automatically expand as necessary. This is an iterative program relying
  on mutability.
*)

open Mini_ml

let version       = ref false
let debug_parse   = ref false (*Print parse trees. No evaluation.*)
let evaluate_flag = ref false
let nolabel       = ref false (*Don't display startup message*)
let filename      = ref ""

let read_args () =
  let specification =
    [("-v", Arg.Set version, "Print the version number");
     ("-p", Arg.Set debug_parse, "Print parse trees (no evaluation - this flag has no effect in batch mode" );
     ("-l", Arg.Set nolabel, "Don't display a startup banner");
     ("-e", Arg.String (fun s -> filename := s; evaluate_flag := true),
      "Evaluates the script in the specified filename")
    ]
  in Arg.parse specification
  (fun s ->
    Printf.printf "Warning : Ignoring unrecognized argument \"%s\"\n" s)
  "Usage : mini-ml [opt]"

let prompt (continuing:bool) =
  (print_string (if (not continuing)
    then "? " else "... ");(flush stdout))
let read (continuing:bool)=prompt continuing; input_line stdin

let banner () =
  Printf.printf "mini-ml version 0.0.1\n" ;
  Printf.printf "Copyright (C) Bloomberg L.P. 2012,2013,2014 - All rights reserved.\n";
  Printf.printf "To exit, type the EOF character (^Z on Windows).\n" 

let handle_interpreter_error ?(finally=(fun () -> ())) exn=
  match exn with
  (*
  | Blan.Exception.Eval_error (msg, ps) -> finally () ; Printf.printf "%s\n" (Blan.Exception_util.build_eval_error msg ps)
  *)
  | Division_by_zero -> finally () ; Printf.printf "Division by zero\n" ;
  | End_of_file -> finally (); raise exn 
  | Failure s -> finally () ; (Printf.printf "%s\n" s)
  | _  as e -> finally (); Printf.printf "Unknown exception : %s\n" (Printexc.to_string e) ; raise e

let safe_proc ?finally f =
  try f ()
  with exn -> handle_interpreter_error ?finally exn

let eval_show debug_parse buf env =
  if debug_parse then
    (string_of_expression (expression_of_string (Buffer.contents buf)))^"\n"
  else
    (string_of_value (value_of_string env (Buffer.contents buf)))^"\n"

let eval_show_file debug_parse file env =
  if debug_parse then
    (string_of_expression_list (expression_list_of_file file))^"\n"
  else
    (string_of_value (value_of_file env file))^"\n"

let main =
  read_args ();
  if !version then print_string "0.9.0\n"
  else 
    (*The top level environment is necessarily mutable (and there is
      this one and only one for an entire interactive session).*)
    let env = ref [] in
    let _ = (if not !nolabel then banner ()) in
    if !evaluate_flag then
      (*File*)
      let f () =
        let s = eval_show_file !debug_parse !filename env in
        print_string s
      in  safe_proc f
    else 
      (*Interactive*)
      let initial_capacity = 4*1024 in
      let buf = Buffer.create initial_capacity in
      try 
        while true do
          let f () =
            (*Read*)
            let l = read ((Buffer.length buf)!=0) in
            let len = String.length l in
            if len > 0 then
              if l.[0] = '%' then (*Comment line. Discard.*) ()
              else
                if l.[len - 1] = '\\' then
                  (*Line continuation; append and keep reading*)
                  (Buffer.add_string buf ((String.sub l 0 (len-1))^"\n"))
                else
                  (*Discard partial statements with ^G*)
                  if l.[len-1] = (char_of_int 7) then Buffer.clear buf
                  else
                    (*We think we got a phrase. Evaluate.*)
                    let _ = Buffer.add_string buf l in
                    let s = eval_show !debug_parse  buf env in 
                    Buffer.clear buf; print_string s
          in (safe_proc ~finally:(fun () -> Buffer.clear buf) f)
        done
      with
      | End_of_file -> print_string "\n" (*We're out of here.*)
