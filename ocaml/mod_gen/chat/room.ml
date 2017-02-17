module type TYPE = sig
  type t
end

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

module type MAKE_SLIST =
  functor (T : TYPE) -> SLIST with type item = T.t

module Make_slist : MAKE_SLIST =
  functor (T : TYPE) -> struct
    type item = T.t
    type t = { data : item;  mutable next : t option }
    type cursor = t ref
    let mk_cursor p = ref p
    let singleton d = { data = d; next = None }
  end

(*Signature for a module implementing chat-history*)
module type CHAT_HISTORY = SLIST with type item = string

(*The type of a module that implements a chat-room*)
module type ROOM = sig

  type t (*The type of a chat-room*)
  type extern_t (*Type for external representation*)

  type roomname = string (*The type of a room's handle'*)
  type username = string (*The type of a user's handle*)

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

(*The type of a functor that generates a module satisfying [ROOM]
  (with a specific external representation) given a module satisfying
  [CHAT_HISTORY]*)
module type MAKE_ROOM = 
  functor (Chat_history : CHAT_HISTORY) -> 
     ROOM  with type extern_t = 
       string * (Chat_history.t) * (string * Chat_history.cursor) list

(*An implementation of the [MAKE_ROOM] functor*)
module Make_room : MAKE_ROOM =
  functor (Chat_history : CHAT_HISTORY) -> struct

    type roomname = string
    type username = string

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

    (*As per the above; 3-components : this format can be visualized
      in the top-level. Remember there are pointers involved so don't
      go writing through the result*)
    type extern_t = 
      roomname * (Chat_history.t) 
      * (username * Chat_history.cursor) list

    (*[show room] produces an external representation of [room]*)
    let show room = (room.name , room.last
      , Hashtbl.fold (fun k v acc -> (k, v) :: acc) room.logs [])

    module Detail = struct 
      (*[publish room msg] appends a new node to the room's chat
      history detailing the room's name and the contents of [msg]*)
      let publish room msg =
        let item = Chat_history.singleton (room.name ^ ": " ^ msg) in
        room.last.Chat_history.next <- Some item;
        room.last <- item
    end

    (*[subscribe room user] appends an publishment to the
      chat-history and creates an entry for the user in [room.logs]*)
    let subscribe room user =
      if (not (Hashtbl.mem room.logs user)) then
        begin
          Detail.publish room ("Say hi to " ^ user ^ "!");
          Hashtbl.add room.logs user (Chat_history.mk_cursor room.last)
        end

    (*[on_unsubscript room user] removes any binding of [user] in
      [room.logs] and appends an publishment to the chat-history*)
    let unsubscribe room user =
      if Hashtbl.mem room.logs user then 
        (Hashtbl.remove room.logs user;
         Detail.publish room ("Say goodbye to " ^ user))

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
        Detail.publish room (user ^ ": " ^ msg)

  end

(*Instantiation of a chat-room based on a chat-history of an linked
  list of strings*)

module Chat_history : CHAT_HISTORY = 
  Make_slist (struct type t = string end)

module Room : ROOM with type extern_t = 
  string * (Chat_history.t) * (string* Chat_history.cursor) list = 
  Make_room (Chat_history)

(*For convience sake, now lift the above module's definitions into
  this scope*)
open Room
