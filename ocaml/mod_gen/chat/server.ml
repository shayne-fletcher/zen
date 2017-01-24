(*Signature for modules representing work*)
module type TASK = sig
  type t (*The type of a task*)
end

(*Signature for a producer/consumer (FIFO) queue*)
module type PRODUCER_CONSUMER_QUEUE = sig
  type task (*The type of a job*)
  type t  (*The type of the queue*)

  (*Add a task to the queue*)
  val enqueue_task : task -> unit

  (*Pop a task from the queue*)
  val dequeue_task : unit -> task
end

(*Signature of a functor that generates producer/consumer queues given
  a module for tasks*)
module type MAKE_PRODUCER_CONSUMER_QUEUE =
  functor (Task : TASK) -> 
    PRODUCER_CONSUMER_QUEUE with type task = Task.t

(*A particular implementation of the signature [TASK] where the job is
  represented by a pair of input/output channels (client connection)*)
module Make_query : 
  TASK with type t = in_channel * out_channel = 
struct
  (*The type of the task*)
  type t = in_channel * out_channel
end

(*A functor implementing [MAKE_PRODUCER_CONSUMER_QUEUE]*)
module Thread_safe_queue : MAKE_PRODUCER_CONSUMER_QUEUE =
  functor (Task : TASK) -> struct
    type task = Task.t
    type t = task Queue.t

    let tasks : task Queue.t = Queue.create ()
    let task_available : Condition.t = Condition.create ()
    let tasks_access : Mutex.t = Mutex.create ()

    let enqueue_task (job : task) : unit =
      Mutex.lock tasks_access;
      Queue.add job tasks;
      Condition.signal task_available;
      Mutex.unlock tasks_access

    let dequeue_task () : task =
      Mutex.lock tasks_access;
      let job = ref None in
      while !job = None do
        try
          job := Some (Queue.take tasks)
        with Queue.Empty -> Condition.wait task_available tasks_access
      done;
      Mutex.unlock tasks_access;
      match !job with
      | Some j -> j
      | _ -> assert false (*This can't happen*)

  end

(*Type of module implementing a producer/consumer queue where the
  tasks are represented by pairs of input/output channels*)
module type IO_QUEUE =
  PRODUCER_CONSUMER_QUEUE 
  with type task = in_channel * out_channel

(*Signature of a port module*)
module type PORT = sig
  val port_number : int
end

(*Signature of a module implementing addresses*)
module type ADDRESS = sig
  type t
  val get : unit -> t
end

(*Specialized signature for socket addresses*)
module type SOCKET_ADDRESS = sig
  include ADDRESS with type t = Unix.sockaddr
  module Port : PORT
end    

(*Functor for a socket address configured by its port*)
module type MAKE_SOCKET_ADDRESS = 
  functor (P : PORT) -> SOCKET_ADDRESS with module Port = P

(*An implementation of [MAKE_SOCKET_ADDRESS] that computes a module
  satisfying [SOCKET_ADDRESS] for a socket in the internet doman *)
module Internet_address : MAKE_SOCKET_ADDRESS =
  functor (P : PORT) -> struct
    module Port = P
    type t = Unix.sockaddr
    let get () =
      Unix.ADDR_INET
        ((Unix.gethostbyname(
              Unix.gethostname())
         ).Unix.h_addr_list.(0), P.port_number)
  end

(*The signature of a socket module*)
module type SOCKET = sig
  module Address : SOCKET_ADDRESS

  val get : unit -> Unix.file_descr
  val close : unit -> unit
end

(*Signature of a module with a [max_connections] constant*)
module type MAX_PENDING_CONNECTIONS = sig
  val max_pending_connections : int
end

(*The type of a binary functor that produces a socket module from a socket
  address and a maximal number of pending connections*)
module type MAKE_SOCKET = 
  functor (A : SOCKET_ADDRESS) -> 
    functor (M : MAX_PENDING_CONNECTIONS) -> 
  SOCKET with module Address = A

(*An implementation of functor [MAKE_SOCKET] that computes a TCP
  socket functor*)
module TCP_socket : MAKE_SOCKET =
  functor (A : SOCKET_ADDRESS) -> 
    functor (M : MAX_PENDING_CONNECTIONS) ->
struct
  module Address = A

  (*[home_socket] type is a possibly null reference to a file
    descriptor*)
  type fd_type = 
    Unix.file_descr option ref

  let home_socket : fd_type = ref None

  let rec init () =
    try
      print_endline ("Initializing");
      let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Unix.bind s (A.get ());
      Unix.listen s (M.max_pending_connections);
      print_endline ("Ready on port" ^ (string_of_int (A.Port.port_number)));
      home_socket := Some s
    with _ -> init ()

  let rec get () =
    match !home_socket with
    | None -> init (); get ()
    | Some s -> s

  let close () =
    match !home_socket with
    | Some s -> Unix.close s
    | None -> ()
      
end

(*The type of a server module*)
module type SERVER = sig
  type input
  type output
  val run : (input -> output) -> unit
end

module type STRING_SERVER = SERVER 
  with type input = string 
  with type output = string

(*The type of a functor for socket servers that require a user
  provided handler function to accept string requests and produce
  string responses*)
module type MAKE_STRING_SOCKET_SERVER = 
  functor (Socket : SOCKET) -> 
    functor (Task_queue : IO_QUEUE) -> STRING_SERVER

(*An implementation of the functor where the provided task queue is
  serviced by a thread pool*)
module String_socket_server : MAKE_STRING_SOCKET_SERVER = 
  functor (Socket : SOCKET) -> 
    functor (Task_queue : IO_QUEUE) ->
  struct
    type input = string
    type output = string

  (*[accept_connection ()] retrieves the home socket then loops
    forever accepting connections on it and enqueuing jobs*)
  let accept_connections () : unit =
    let s = Socket.get () in
    while true do
    (*Accept a connection on [s]. The returned descriptor is a socket
      connected to the client; the returned address is the address of
      the connecting client*)
      let (client, _) = Unix.accept s in
      let job = (Unix.in_channel_of_descr client,
                 Unix.out_channel_of_descr client) in
      Task_queue.enqueue_task job
  done

  (*[process_job] runs on the worker threads servicing the thread
    pool*)
  let process_job (handler : string -> string) : unit =
    while true do
      let (ic, outc) = Task_queue.dequeue_task () in
      let cmd = input_line ic in
      let response = handler cmd in
      output_string outc (response ^ "\n"); flush outc;
      (*Suspend execution of this thread for 1/10(s), allow other
        threads to run during this time*)
      Thread.delay 0.1;
      (*We are done with this socket's output channel - close it*)
      close_out outc;
    done

  (*[run] executes on the main thread where it spawnse 3 worker and
    then accepts connections*)
  let run handler =
    for i = 0 to 3 do
      ignore (Thread.create process_job handler)
    done;
    accept_connections ()
  end

(* -- *)

(* Example : "echo" server *)
let () = 
  let module Port = struct let port_number = 8086 end in
  let module Max_pending_connections = struct let max_pending_connections = 256 end in
  let module Socket_address = Internet_address (Port) in
  let module Socket = TCP_socket (Socket_address) (Max_pending_connections) in
  let module Task = struct type t = in_channel * out_channel end in
  let module Task_queue = Thread_safe_queue (Task) in
  let module Server = String_socket_server (Socket) (Task_queue) in
  Server.run (fun s -> s)





