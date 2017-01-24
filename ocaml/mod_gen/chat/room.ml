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

  type username = string (*The type of a user's handle*)

  (*[on_create_room user tag] causes the creation of a new room called
    [tag] and an initial message in the room's history detailing the
    name of the user who created it*)
  val on_create_room : username -> string -> t

  (*[on_catchup_user room user] returns the messages that have been
    added to [room] not yet seen by [user] *)
  val on_catchup_user : t -> username -> string list

  (*[on_subscribe_user room user] adds [user] to [room] and causes the
    addition of a new message in its history detailing the name of the
    [user] who's joined*)
  val on_subscribe_user : t -> username -> unit

  (*[on_unsubscribe_user] removes [user] from [room] and causes the
    addition of a new message in the room's history detailing the name
    of the [user] who left*)
  val on_unsubscribe_user : t -> username -> unit

  (*[on_incoming_message user msg] inserts a new message in the room
    history detailing the name of the [user] and the contents of the
    [msg]*)
  val on_incoming_message : t -> username -> string -> unit
end

(*The type of a functor that generates a module satisfying [ROOM]
  given a module satisfying [CHAT_HISTORY]*)
module type MAKE_ROOM = 
  functor (Chat_history : CHAT_HISTORY) -> ROOM

(*An implementation of the [MAKE_ROOM] functor*)
module Make_room : MAKE_ROOM =
  functor (Chat_history : CHAT_HISTORY) -> struct

    type username = string

    (*The room is modeled with 3 components:
     {ol {- [name] : the name of the room}
     {- [last] : the last node of the chat-history}
     {- [logs] : a map of users to cursors in the chat-history}}
     where [logs] keeps track of the last message in the chat-room history
     each subscribed user has seen.
    *)
    type t = {
      name : string; (*The room's name*)
      mutable last : Chat_history.t; (*Last message in history*)
      mutable logs : (username, Chat_history.cursor) Hashtbl.t (*As above*)
    }

    (*[publish room msg] appends a new node to the room's chat
      history detailing the room's name and the contents of [msg]*)
    let publish room msg =
      let item = Chat_history.singleton (room.name ^ ": " ^ msg) in
      room.last.Chat_history.next <- Some item;
      room.last <- item

    (*[on_subscribe_user room user] appends an publishment to the
      chat-history and creates an entry for the user in [room.logs]*)
    let on_subscribe_user room user =
      if (not (Hashtbl.mem room.logs user))then
        (publish room ("Say hi to " ^ user ^ "!");
         Hashtbl.add room.logs user (Chat_history.mk_cursor room.last))

    (*[on_unsubscript room user] removes any binding of [user] in
      [room.logs] and appends an publishment to the chat-history*)
    let on_unsubscribe_user room user =
      if Hashtbl.mem room.logs user then 
        (Hashtbl.remove room.logs user;
         publish room ("Say goodbye to " ^ user))

    (*[on_catchup_user room user] searches for [user] in [room.logs]
      and if found, computes the messages not seen and updates the
      users's cursor to [room.last]*)
    let on_catchup_user room user = 
      let rec read_log node acc =
        match node.Chat_history.next with
        | None -> List.rev acc
        | Some next -> read_log next (next.Chat_history.data :: acc) in
      try
        let backlog = read_log !(Hashtbl.find room.logs user) [] in
        Hashtbl.replace room.logs user (Chat_history.mk_cursor room.last);
        backlog
      with
        Not_found -> []

    (*[on_create_room user tag] produces a new room where the
      singleton chat-history containing the annoucment of the room's
      creation and who's logs contain an entry for the creating
      [user]*)
    let on_create_room user tag =
      let room = {
        name = tag;
        last = Chat_history.singleton ("User " ^ user ^ " started " ^ tag);
        logs = Hashtbl.create 16 } in
      on_subscribe_user room user;
      room

    (*[on_incoming_message room user] appends a new message to the
      chat-history*)
    let on_incoming_message room user msg =
      if Hashtbl.mem room.logs user then
        publish room (user ^ ": " ^ msg)

  end

(*Instantiation of a chat-room based on a chat-history of an linked
  list of strings*)
module Room : ROOM = 
  Make_room (Make_slist (struct type t = string end))
  
