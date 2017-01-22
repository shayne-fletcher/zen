module type TYPE = sig
    type t
end

module type WEAKLY_LINKED_LIST = sig
  type item
  type t = {
    data : item;
    mutable next : t option
  }
  type cursor = t ref  
end

module type MAKE_WEAKLY_LINKED_LIST =
  functor (T : TYPE) -> WEAKLY_LINKED_LIST with type item = T.t

module type CHAT_HISTORY =
  WEAKLY_LINKED_LIST with type item = string

module type ROOM = sig
  type t
  type username = string

  val create : username -> string -> t
  val backlog : t -> username -> string list
  val add_user : t -> username -> unit
  val user_message : t -> string -> string -> unit
end

module type MAKE_ROOM = 
  functor (History : CHAT_HISTORY) -> ROOM

module Make_room : MAKE_ROOM =
  functor (History : CHAT_HISTORY) -> struct
    type username = string

    type t = {
      name : string;
      mutable last : History.t;
      mutable logs : (username, History.cursor) Hashtbl.t
    }

    let announce (room : t) (message : string) : unit =
      let announcment = {
        History.data = room.name ^ ": " ^ message;
        History.next = None } in
      room.last.History.next <- Some announcment;
      room.last <- announcment

    let backlog room user = 
      let rec read_log node acc =
        match node.History.next with
        | None -> List.rev acc
        | Some next ->
           read_log next (next.History.data :: acc) in
      let backlog = read_log !(Hashtbl.find room.logs user) [] in
      Hashtbl.replace room.logs user (ref room.last);
      backlog

    let add_user room user =
      announce room (user ^ " has joined the room.");
      Hashtbl.add room.logs user (ref room.last)

    let remove_user room user =
      if Hashtbl.mem room.logs user then 
        (Hashtbl.remove room.logs user;
         announce room (user ^ " has left the room."))

    let user_message room user message =
      if Hashtbl.mem room.logs user then
        announce room (user ^ ": " ^ message)

    let create user name =
      let room = {
        name = name;
        last = {
          History.data = ("User " ^ user ^ " started " ^ name);
          History.next = None };
        logs = Hashtbl.create 16 } in
      add_user room user;
      room
  end

module Make_weakly_linked_list : MAKE_WEAKLY_LINKED_LIST =
  functor (T : TYPE) -> struct
    type item = T.t
    type t = { data : item;  mutable next : t option }
    type cursor = t ref
  end

module Room : ROOM = 
  Make_room (Make_weakly_linked_list (struct type t = string end))
  

(**)

