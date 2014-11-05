{
open Lexing
open Parser

module Location = struct

  type t = {
    loc_start: Lexing.position;
    loc_end: Lexing.position;
    loc_ghost: bool;
  }

  let in_file name =
    let loc = {
      pos_fname = name;
      pos_lnum = 1;
      pos_bol = 0;
      pos_cnum = -1;
    } in
    { loc_start = loc; loc_end = loc; loc_ghost = true }

  let none = in_file "_none_";;
  (** An arbitrary value of type [t]; describes an empty ghost range. *)

  let curr lexbuf = {
    loc_start = lexbuf.lex_start_p;
    loc_end = lexbuf.lex_curr_p;
    loc_ghost = false
  }
end

type error =
  | Illegal_escape of string
  | Eol_in_string of string
  | Unterminated_string
;;

exception Error of error * Location.t;;

(* To buffer string literals *)

let initial_string_buffer = String.create 256
let string_buff = ref initial_string_buffer
let string_index = ref 0

let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0

let store_string_char c =
  if !string_index >= String.length (!string_buff) then 
    begin
      let new_buff = String.create (String.length (!string_buff) * 2) in
      String.blit (!string_buff) 0 new_buff 0 (String.length (!string_buff));
      string_buff := new_buff
  end;
  String.unsafe_set (!string_buff) (!string_index) c;
  incr string_index

let get_stored_string () =
  let s = String.sub (!string_buff) 0 (!string_index) in
  string_buff := initial_string_buffer;
  s

(* To store the position of the beginning of a string *)
let string_start_loc = ref Location.none;;
let is_in_string = ref false
let in_string () = !is_in_string

(* To translate escape sequences *)

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let char_for_decimal_code lexbuf i =
  let c = 100 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
           10 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
                (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48) in
  if (c < 0 || c > 255) then
   raise (Error(Illegal_escape (Lexing.lexeme lexbuf),
                      Location.curr lexbuf))
  else Char.chr c

let char_for_hexadecimal_code lexbuf i =
  let d1 = Char.code (Lexing.lexeme_char lexbuf i) in
  let val1 = if d1 >= 97 then d1 - 87
             else if d1 >= 65 then d1 - 55
             else d1 - 48
  in
  let d2 = Char.code (Lexing.lexeme_char lexbuf (i+1)) in
  let val2 = if d2 >= 97 then d2 - 87
             else if d2 >= 65 then d2 - 55
             else d2 - 48
  in
  Char.chr (val1 * 16 + val2)

(* Update the current location with file name and line number. *)

let update_loc lexbuf file line absolute chars =
  let pos = lexbuf.lex_curr_p in
  let new_file = 
    match file with
    | None -> pos.pos_fname
    | Some s -> s
  in
  lexbuf.lex_curr_p <- { pos with
    pos_fname = new_file;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  }
}

let newline = ('\010' | "\013\010" )
let blank = [' ' '\009' '\012']

rule token = parse
  | newline
      { update_loc lexbuf None 1 false 0;
        token lexbuf
      }
  | blank +
      { token lexbuf }
  | "\""
      { reset_string_buffer();
        is_in_string := true;
        let string_start = lexbuf.lex_start_p in
        string_start_loc := Location.curr lexbuf;
        string lexbuf;
        is_in_string := false;
        lexbuf.lex_start_p <- string_start;
        STRING (get_stored_string()) }

and string = parse
    '"'
      { () }
  | '\\' newline ([' ' '\t'] * as space)
      { update_loc lexbuf None 1 false (String.length space);
        string lexbuf
      }
  | '\\' ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']
      { store_string_char(char_for_backslash (Lexing.lexeme_char lexbuf 1));
        string lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      { store_string_char (char_for_decimal_code lexbuf 1);
         string lexbuf }
  | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']
      { store_string_char (char_for_hexadecimal_code lexbuf 2);
         string lexbuf }
  | '\\' _
      { raise (Error (Illegal_escape (Lexing.lexeme lexbuf),
                      Location.curr lexbuf)) }
  | newline
      { raise (Error (Eol_in_string (Lexing.lexeme lexbuf),
                      Location.curr lexbuf)) }
  | eof
      { is_in_string := false;
        raise (Error (Unterminated_string, !string_start_loc)) }
  | _
      { store_string_char(Lexing.lexeme_char lexbuf 0);
        string lexbuf }
