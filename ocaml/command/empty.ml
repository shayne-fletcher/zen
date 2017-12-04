open Import

let mk_cmd cmd =
  Cfg.cfg >>| fun {cli_exe} ->
  sprintf "%s 2>&1 %s" cli_exe cmd

let init () =
  mk_cmd {|del "S" > /dev/null|} >>=
  Sys.command

let test path =
  let cmd=
    sprintf "add \"%s\" -fmt json \"[\\\"Structure\\\"]\"" path in
  mk_cmd cmd >>=
  Sys.command_exn

let tests () =
  init () >>= fun _ ->
  let n = 20 in
  let rec loop path k =
    if k = n then
      return ()
    else
      test path >>= fun () ->
      printf "Adding \"%s\"\n" path;
      loop (path ^ (sprintf "/S%s" (string_of_int k))) (k + 1) in
  loop "S" 0 >>= fun () ->
  mk_cmd "tree" >>= fun cmd ->
  printf "cmd : \"%s\"\n" cmd; Sys.command_exn cmd

let command : Command.t =
  Command.async' ~summary:"blah" (Command.Param.return tests)
