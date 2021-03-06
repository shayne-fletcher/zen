(*Trivial module record*)
module type TYPE = sig
  type t
end

(*Signature for a module implementing a mutable singly-linked list*)
module type SLIST = sig
  type item
  type t = {
    data : item;
    mutable next : t option
  }
  type cursor = t ref

  (*Create an isolated [t]*)
  val singleton : item -> t

  (*Return a pointer to the provided node*)
  val mk_cursor : t -> cursor
end

(*The type of a functor producing modules of singly-linked list
  datatype*)
module type MAKE_SLIST =
  functor (T : TYPE) -> SLIST with type item = T.t

(*An implementation of [MAKE_SLIST]*)
module Make_slist : MAKE_SLIST =
  functor (T : TYPE) -> struct
    type item = T.t
    type t = { data : item;  mutable next : t option }
    type cursor = t ref
    let mk_cursor p = ref p
    let singleton d = { data = d; next = None }
  end

(*--*)

(*Signature for a module implementing chat-history*)
module type CHAT_HISTORY = SLIST with type item = string

(*--*)

(*The type of a module that implements a chat-room*)
module type ROOM = sig

  type t (*The type of a chat-room*)
  type username (*The type of a user's handle*)
  type roomname (*The type of a room's handle'*)
  type extern_t (*Type for external representation*)

  (*Fix external reps of user and room names to [string]*)
  val username_of_string : string -> username
  val string_of_username : username -> string
  val roomname_of_string : string -> roomname
  val string_of_roomname : roomname -> string

  (*[show r] is a general accessor producing an external
    representation*)
  val show : t -> extern_t

  (*[make user tag] causes the creation of a new room called [tag] and
    an initial message in its history as well subscribing [user] to
    it*)
  val make : username -> roomname -> t

  (*[catchup room user] returns the messages that have been
    added to [room] not yet seen by [user] *)
  val catchup : t -> username -> string list

  (*[subscribe room user] adds [user] to [room] and causes the
    addition of a new message in its history detailing the name of the
    [user] who's joined*)
  val subscribe : t -> username -> unit

  (*[unsubscribe] removes [user] from [room] and causes the
    addition of a new message in the room's history detailing the name
    of the [user] who left*)
  val unsubscribe : t -> username -> unit

  (*[accept_message user msg] inserts a new message in the room
    history detailing the name of the [user] and the contents of the
    [msg]*)
  val accept_message : t -> username -> string -> unit
end

(*The type of a functor that generates a module satisfying (a
  particular specialization) of [ROOM] given a module satisfying
  [CHAT_HISTORY]*)
module type MAKE_ROOM = 
  functor (Chat_history : CHAT_HISTORY) -> ROOM  
  with type extern_t = string * (Chat_history.t) * (string * Chat_history.cursor) list

(*An implementation of the [MAKE_ROOM] functor*)
module Make_room : MAKE_ROOM =
  functor (Chat_history : CHAT_HISTORY) -> struct

    type roomname = string
    type username = string

    (*Trivial*)
    let username_of_string s = s
    let string_of_username s = s
    let roomname_of_string s = s
    let string_of_roomname s = s

    (*As per the below; 3-components : this format can be visualized
      in the top-level. Remember there are pointers involved so don't
      go writing through the result*)
    type extern_t = 
      roomname * (Chat_history.t) 
      * (username * Chat_history.cursor) list

    (*The room is modeled with 3 components:
     {ol {- [name] : the name of the room}
     {- [last] : the last node of the chat-history}
     {- [logs] : a map of users to cursors in the chat-history}}
     where [logs] keeps track of the last message in the chat-room history
     each subscribed user has seen.
    *)
    type t = {
      name : roomname; (*The room's name*)
      mutable last : Chat_history.t; (*Last message in history*)
      mutable logs : (username, Chat_history.cursor) Hashtbl.t (*As above*)
    }

    (*[show room] produces an external representation of [room]*)
    let show room = (room.name , room.last
      , Hashtbl.fold (fun k v acc -> (k, v) :: acc) room.logs [])

    module Detail = struct 
      (*[publish room msg] appends a new node to the room's chat
      history detailing the room's name and the contents of [msg]*)
      let publish room msg =
        let item = Chat_history.singleton ("#" ^ room.name ^ ": " ^ msg) in
        room.last.Chat_history.next <- Some item;
        room.last <- item
    end

    (*[subscribe room user] appends an publishment to the
      chat-history and creates an entry for the user in [room.logs]*)
    let subscribe room user =
      if (not (Hashtbl.mem room.logs user)) then
        begin
          Detail.publish room (user ^ " joined the room");
          Hashtbl.add room.logs user (Chat_history.mk_cursor room.last)
        end

    (*[on_unsubscript room user] removes any binding of [user] in
      [room.logs] and appends an publishment to the chat-history*)
    let unsubscribe room user =
      if Hashtbl.mem room.logs user then 
        (Hashtbl.remove room.logs user;
         Detail.publish room (user ^ " left the room"))

    (*[catchup room user] searches for [user] in [room.logs]
      and if found, computes the messages not seen and updates the
      users's cursor to [room.last]*)
    let catchup room user = 
      let rec read_log node acc =
        match node.Chat_history.next with
        | None -> List.rev acc
        | Some next -> read_log next (next.Chat_history.data :: acc) in
      try
        let log = !(Hashtbl.find room.logs user) in
        let backlog = read_log log [] in
        Hashtbl.replace room.logs user (Chat_history.mk_cursor room.last);
        backlog
      with
        Not_found -> []

    (*[make user tag] produces a new room where the
      singleton chat-history containing the annoucment of the room's
      creation and who's logs contain an entry for the creating
      [user] who is incidentally subscribed*)
    let make user tag =
      let room = {
        name = tag;
        last = Chat_history.singleton ("User " ^ user ^ " started " ^ tag);
        logs = Hashtbl.create 16 } in
      subscribe room user;
      room

    (*[accept_message room user] appends a new message to the
      chat-history*)
    let accept_message room user msg =
      if Hashtbl.mem room.logs user then
        Detail.publish room ("[" ^ user ^ "]" ^ " " ^ msg)

  end

(*Instantiation of a chat-room based on a chat-history of an linked
  list of strings*)

module Chat_history : CHAT_HISTORY = 
  Make_slist (struct type t = string end)

module Room : ROOM with type extern_t = 
  string * (Chat_history.t) * (string* Chat_history.cursor) list = 
Make_room (Chat_history)

(*--*)

(*Module type of the server request protocol*)
module type REQUEST_HANDLER = sig

  (*Sub-module satisfying [ROOM]*)
  module R : ROOM

  (*Expressions of this type are produced by the module
    [Request_parser]*)
  type ast = private
  | Ast_connect of string  (*Connect a new user*)
  | Ast_nick of string * string (*Change a user's 'handle'*)
  | Ast_join of string * string (*Subscribe a user to a channel*)
  | Ast_privmsg of (string * string) * string (*Send user's message to a channel*)
  | Ast_poll of (string * string) (*Get updates for the given channel not yet seen by the user*)

  (*It's convenient to have these functions for testing/debugging*)
  val string_of_ast : ast -> string
  val ast_of_string : string -> ast
  val list_users : unit -> (string * string) list
  val list_rooms : unit -> (string * R.extern_t) list

  (*[process_message req] satisfies the command represented by [req]
    and returns the response*)
  val process_message : string -> string
end

(*Type of a functor producing request handlers given a module
  satisfying [ROOM]*)
module type MAKE_REQUEST_HANDLER = 
  functor (R : ROOM) -> REQUEST_HANDLER with module R = R

(*An instance of [MAKE_REQUEST_HANDLER]*)
module Make_request_handler : MAKE_REQUEST_HANDLER =
  functor (R : ROOM) -> struct

    (*The implementation is already provided via 'request_ast.mli'
      (which is referenced from the module [Request_parser])*)
    include Request_ast

    (*The module parameter binds as the sub-module*)
    module R = R

    type token_t = string (*The type of tokens uniquely identifying users*)
    type username = R.username (*The type of user 'handles'*)
    type roomname = R.roomname (*The type of room names*)

    exception Username_exists (*Raised in the event we get a token clash*)

    (*Aliases for brevity*)
    module Lex = Request_lexer
    module Parse = Request_parser

    (*Module state variables:
     {ol {- [users] : a map of tokens to nicknames}
     {- [rooms] : a table of rooms}}
    *)
    let rooms : (roomname, R.t) Hashtbl.t = Hashtbl.create 16
    let users : (token_t, username) Hashtbl.t = Hashtbl.create 16

    (*Convience functions for visualizing the module state*)
    let list_users () =
      Hashtbl.fold 
        (fun x y acc -> (x, (R.string_of_username y)) :: acc) users []
    let list_rooms () =
      Hashtbl.fold 
        (fun x y acc -> (R.string_of_roomname x, R.show y) :: acc) rooms []

    (*Convenience function for visualizing an AST*)
    let string_of_ast = function
      | Ast_connect usr -> "/nick " ^ usr
      | Ast_nick (tok, usr) -> "[" ^ tok ^ "]" ^ "/nick " ^ usr
      | Ast_join (tok, chan) -> "[" ^ tok ^ "]" ^ "/join " ^ "#" ^ chan
      | Ast_privmsg ((tok, chan), msg) -> "[" ^ tok ^ "]" ^ "/privmsg " ^ "#" ^ chan ^ ": " ^ msg
      | Ast_poll (tok, chan) -> "[" ^ tok ^ "]" ^ "/poll " ^ "#" ^ chan

    (*[ast_of_string s] uses [Request_lexer] and [Request_parser] to
      parse an AST from a string*)
    let ast_of_string s = 
      Parse.parse_message Lex.token (Lexing.from_string s)

    (*Implementation details*)
    module Detail = struct

      (*[connect handle] adds a new user.  If [handle] is in use, the
        operation is unsuccessful and and error message
        results. Otherwise, the token generated for the new user is
        returned*)
      let rec connect (handle : string) : string =
        let user = R.username_of_string handle in
        try
          if Hashtbl.fold 
            (fun _ name acc -> acc || name = user) users false then
            raise Username_exists
          else
            let tok = 
              let hi = Int64.to_string (Random.int64 Int64.max_int) 
              and lo = Int64.to_string (Random.int64 Int64.max_int) in
              hi ^ lo in
            if Hashtbl.mem users tok then
              connect handle (*Token taken; generate another and try again*)
            else  (Hashtbl.add users tok user; tok)
        with
        | Username_exists -> "Username taken"

      (*[join tok chan] attempts to subscribe the user indicated by
        [tok] to the channel known by [chan]. If the given channel is
        found not to exist, it is created*)
      let join (tok : token_t) (chan : string) : string =
        try  begin
          let user = Hashtbl.find users tok in
          try
            let room = Hashtbl.find rooms (R.roomname_of_string chan) in
            R.subscribe room user;
            ("#" ^ chan ^ ": " ^ (R.string_of_username user) ^ " joined the room")
          with
          | Not_found -> 
            let roomname = R.roomname_of_string chan in
            Hashtbl.add rooms roomname (R.make user roomname);
            ("#" ^ chan ^ ": " ^ (R.string_of_username user) ^ " started the room")
          end
        with
        | Not_found -> "User not recognized"

      (*[privmsg tok chan msg] publishes the user (indicated by [tok])
        [msg] to the channel [chan]. If that works out, the empty string
        ["OK"] is returned otherwise, a string that indicates why the
        operation failed*)
      let privmsg (tok : token_t) (chan : string) (msg : string) : string =
        try  begin
          let user = Hashtbl.find users tok in
          try
            let room = Hashtbl.find rooms (R.roomname_of_string chan) in
            R.accept_message room user msg;
            "OK"
          with
          | Not_found -> "Unrecognized channel"
        end
        with
        | Not_found -> "Unrecognized user"

      (*[poll tok chan] gets any messages for the user [tok] on the
        channel [chan] that they haven't already seen*)
      let poll (tok : token_t) (chan : string) : string =
        try begin
          let usr = Hashtbl.find users tok in
          try
            let room = Hashtbl.find rooms (R.roomname_of_string chan) in
            let msgs = R.catchup room usr in
            String.concat "\n" msgs
          with
          | Not_found -> "Unrecognized channel"
        end
        with
        | Not_found -> "Unrecognized user"

    end (*Detail*)

    (*[process_message s] computes a response from the incoming
      request [s]*)
    let process_message s =
      let process_message_aux =  function
        | Ast_connect usr -> Detail.connect usr
        | Ast_join (tok, chan) -> Detail.join tok chan
        | Ast_privmsg ((tok, chan), msg) -> Detail.privmsg tok chan msg
        | Ast_poll (tok, chan) -> Detail.poll tok chan
        | Ast_nick (_, _) -> "Not implemented" in
      try
        let exp = ast_of_string s in
        let resp = process_message_aux exp in
        (* print_endline (" => " ^ resp); *)
        resp
      with
      | _ -> "Unrecognized command"

  end

(*Instantation of a request handler from the room module with a
  chat-history using a linked list of strings*)
module Request_handler = Make_request_handler (Room)

(*--*)

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
      flush stdout;
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

