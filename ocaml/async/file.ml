open! Core
open! Async

(* [pipe_of_file f] returns the reader end of a pipe that will
   continually be filled with chunks of data from an underlying file
   reader. *)
let pipe_of_file file =
  Reader.open_file file >>| fun r -> Reader.pipe r

(* [consume_pipe p] drains the pipe writing its data to stdout as it
   does so. *)
let rec consume_pipe p =
  Pipe.read p >>= function
  | `Ok s -> printf "%s" s; consume_pipe p
  | `Eof -> return ()

let test src = pipe_of_file src >>= fun p -> consume_pipe p

(* This code takes control of starting and stopping the scheduler. *)
(*
let () =
  don't_wait_for (
    pipe_of_file "/Users/fletch/fmt_parser.mly"
    >>= fun p -> consume_pipe p >>| fun () -> shutdown 0)
(* Start the scheduler. *)
let () = never_returns (Scheduler.go ())
*)

(* It's more idiomatic to let the [Command] module do that. *)
let command : Command.t =
  let open Command.Let_syntax in
  Command.async'
    ~summary:"pipe example"
    [%map_open
      let src = anon ("SRC" %: string) in
      fun () ->  test src
    ]
let () =
  try Command.run ~version:"0.0" command
  with e -> eprintf "%s\n" (Exn.to_string e)
