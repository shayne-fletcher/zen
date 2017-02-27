open Server

(*Here is where we bring all the above together to implement the
  chat-server*)
let () = 
  let module Port = struct let port_number = 8086 end in
  let module Max_pending_connections = struct let max_pending_connections = 256 end in
  let module Socket_address = Internet_address (Port) in
  let module Socket = TCP_socket (Socket_address) (Max_pending_connections) in
  let module Task = struct type t = in_channel * out_channel end in
  let module Task_queue = Thread_safe_queue (Task) in
  let module Server = String_socket_server (Socket) (Task_queue) in
  Server.run (Request_handler.process_message)
