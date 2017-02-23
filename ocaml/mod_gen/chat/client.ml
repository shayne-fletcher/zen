let screen_lock : Mutex.t = Mutex.create ()

let write (s : string) : unit  =
  Mutex.lock screen_lock;
  print_string s;
  flush stdout;
  Mutex.unlock screen_lock

let rooms : string list ref = ref []
let tok : string option ref = ref None

let rec process_response (input : in_channel) : unit =

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
    (*Read characters from the input channel until a newline is
      encountered*)
    let resp = input_line input in
    (* Printf.printf "Response : %s\n" resp; flush stdout; *)
    if try_parse_token resp then () else
      (try_parse_room resp;
       if resp = "" || resp = "OK" then () else
         write (resp ^ "\n"); process_response input)
  with
  | End_of_file -> ()

let process_command 
    (server_address : Unix.sockaddr) 
    (command : string) : unit =
  let in_ch, out_ch = Unix.open_connection server_address in
  (match !tok with
  | Some token -> 
    (* Printf.printf "Sending : %s\n" ("[" ^ token ^ "]" ^ command ^ "\n"); *)
    output_string out_ch ("[" ^ token ^ "]" ^ command ^ "\n")
  | None -> output_string out_ch (command ^ "\n"));
  flush out_ch;
  process_response in_ch;
  close_out out_ch

let update server_address () =
  let call_for_backlog chan =
    process_command server_address ("/poll " ^ chan) in
  while true do
    Thread.delay 1.;
    match !tok with
    | None -> ()
    | Some token -> List.iter call_for_backlog !rooms
  done

let _ =
  let server_address =
    (try
       let addr = Unix.inet_addr_of_string (Sys.argv.(1))
       and port = int_of_string (Sys.argv.(2)) in
       Unix.ADDR_INET (addr, port)
     with
     | _ -> 
       let addr = 
         (Unix.gethostbyname (Unix.gethostname ())).Unix.h_addr_list.(0)
       and port = 8086 in
       Unix.ADDR_INET (addr, port)) in
    ignore (Thread.create (update server_address) ());
    while true do
      ignore (input_line stdin);
      Mutex.lock screen_lock;
      print_string "> ";
      flush stdout;
      let command = (input_line stdin) in
      Mutex.unlock screen_lock;
      process_command server_address command
    done
