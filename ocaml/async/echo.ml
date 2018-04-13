open Core
open Async

let rec copy_blocks buf r w =
  Reader.read r buf >>= function
  | `Eof -> return ()
  | `Ok bytes_read ->
    Writer.write w buf ~len:bytes_read;
    Writer.flushed w >>= fun () ->
    copy_blocks buf r w

let run () =
  let host_and_port =
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.on_port 8765)
      (fun _addr r w ->
         let buf = String.create (16 * 1024) in
         copy_blocks buf r w
      ) in
  ignore (host_and_port)

let () =
  run ();
  never_returns (Scheduler.go ())