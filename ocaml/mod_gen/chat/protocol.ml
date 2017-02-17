include Ast

open Room

let rooms : (string, Room.t) Hashtbl.t = Hashtbl.create 16

let string_of_ast = function
  | Ast_connect u -> "/nick " ^ u
  | Ast_nick (t, u) -> "[" ^ t ^ "]" ^ "/nick " ^ u
  | Ast_join (t, chan) -> "[" ^ t ^ "]" ^ "/join " ^ "#" ^ chan
  | Ast_privmsg ((t, chan), txt) -> "[" ^ t ^ "]" ^ "/privmsg " ^ "#" ^ chan ^ ": " ^ txt

let ast_of_string s = 
  Parser.parse_message Lexer.token (Lexing.from_string s)

let process_message s =
  let process_message_aux =  function
    | Ast_connect u ->
      let tok = 
        let hi = Int64.to_string (Random.int64 Int64.max_int) 
        and lo = Int64.to_string (Random.int64 Int64.max_int) in
        hi ^ lo in
      "Your token is : " ^ tok
    (*...*)
    | Ast_join (t, chan) -> failwith "Not implemented"
      (*
      let room, created =
        try
          (Hashtbl.find rooms chan, false)
        with 
        | Not_found -> 
          Hashtbl.add rooms chan (Room.on_create_room usr chan); 
          (Hashtbl.find rooms chan, true)
      in
      chan ^ " : " ^ usr ^
        if created then
          " has started the room"
        else
          begin
            Room.on_subscribe_user room usr; 
            " has joined the room"
          end
          *)
    | Ast_nick (_, _) -> failwith "Not implemented"
    | Ast_privmsg ((_, _), txt) -> failwith "Not implemented" in

  let exp = ast_of_string s in
  let resp = process_message_aux exp in
  print_endline (" => " ^ resp);
  resp
