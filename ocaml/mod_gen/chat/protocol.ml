include Ast

open Room

let rooms : (string, Room.t) Hashtbl.t = Hashtbl.create 16

let string_of_ast = function
  | Ast_nick u -> 
    "/nick " ^ u
  | Ast_join (usr, chan) -> 
    "/join " ^ usr ^ " #" ^ chan
  | Ast_privmsg ((usr, chan), txt) -> 
    "/privmsg " ^ usr ^ " #" ^ chan ^ ": " ^ txt

let ast_of_string s = 
  Parser.parse_message Lexer.token (Lexing.from_string s)

let process_message s =
  let process_message_aux =  function
    | Ast_join (usr, chan) -> 
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
    | Ast_nick name -> failwith "Not implemented"
    | Ast_privmsg ((usr, chan), txt) -> failwith "Not implemented" in

  let exp = ast_of_string s in
  let resp = process_message_aux exp in
  print_endline (" => " ^ resp);
  resp
