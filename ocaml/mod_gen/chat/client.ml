(*This is a 'nasty' client :) It's enough to prove the application
  though.*)

(*A mutex for writing to the screen*)
let console_access : Mutex.t = Mutex.create ()

(*[write s] acquires a lock on the console, writes [s] and releases
  the lock*)
let write (s : string) : unit  =
  Mutex.lock console_access;
  print_string s;
  flush stdout;
  Mutex.unlock console_access

(*This client's state*)
let tok : string option ref = ref None (*Server provided cookie*)
let rooms : string list ref = ref []  (*Names of channels subscribed to*)

(*[proccess_response in_ch] performs updates given server responses*)
let rec process_response (in_ch : in_channel) : unit =
  let try_parse_token resp = 
    let regexp = Str.regexp "[0-9]+" in
    if Str.string_match regexp resp 0 then
      (tok := Some (Str.matched_group 0 resp); true)
    else false 
  and try_parse_room resp = 
    let args = Str.split (Str.regexp_string ":") resp in
    if List.length args > 1 && not (List.mem (List.hd args) !rooms) then
      rooms := (List.hd args) :: !rooms in
  try
    let resp = input_line in_ch in
    if try_parse_token resp then () 
    else
      (try_parse_room resp;
       if resp = "" || resp = "OK" then () else
         write (resp ^ "\n"); process_response in_ch)
  with
  | End_of_file -> ()

(*[process_cmd server cmd] opens a connection to the server, sends it
  [cmd] invokes [process_response] in whatever the server comes back
  with*)
let process_cmd 
    (server : Unix.sockaddr) 
    (cmd : string) : unit =
  let in_ch, out_ch = Unix.open_connection server in
  (match !tok with
  | Some token -> 
    output_string out_ch ("[" ^ token ^ "]" ^ cmd ^ "\n")
  | None -> output_string out_ch (cmd ^ "\n"));
  flush out_ch;
  process_response in_ch;
  close_out out_ch

(*[update server] polls the server for any messages not yet seen in
  this clients subscribed channels. It runs on its own thread*)
let update (server : Unix.sockaddr) () : unit =
  let call_for_backlog chan =
    process_cmd server ("/poll " ^ chan) in
  while true do
    Thread.delay 1.;
    match !tok with
    | None -> ()
    | Some token -> List.iter call_for_backlog !rooms
  done

(*Main entry point*)
let _ =
  let server =
    begin
      try
       let addr = Unix.inet_addr_of_string (Sys.argv.(1))
       and port = int_of_string (Sys.argv.(2)) in
       Unix.ADDR_INET (addr, port)
     with
     | _ -> 
       let addr = 
         (Unix.gethostbyname (Unix.gethostname ())).Unix.h_addr_list.(0)
       and port = 8086 in
       Unix.ADDR_INET (addr, port) 
    end in
  ignore (Thread.create (update server) ());
  while true do
    ignore (input_line stdin);
    Mutex.lock console_access;
    print_string "$ ";
    flush stdout;
    let cmd = input_line stdin in
    Mutex.unlock console_access;
    process_cmd server cmd
  done
