{
open Lexing
open Ml_parser

type error =
| Illegal_character of char
| Unterminated_comment of Ml_location.t

exception Error of error * Ml_location.t

(*Update the current location with file name and line number*)

let update_loc lexbuf file line absolute chars =
  let pos = lexbuf.lex_curr_p in
  let new_file = match file with
                 | None -> pos.pos_fname
                 | Some s -> s
  in
  lexbuf.lex_curr_p <- { pos with
    pos_fname = new_file;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  }

let create_hashtable size init =
  let tbl = Hashtbl.create size in
  List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
  tbl

let keyword_table =
  create_hashtable 149 [
    "else", T_else;
    "false", T_false;
    "fun", T_fun;
    "if", T_if;
    "in", T_in;
    "let", T_let;
    "rec", T_rec;
    "then", T_then;
    "true", T_true;
  ]

(*To buffer strings and comments*)

let initial_string_buffer = Bytes.create 256
let string_buff = ref initial_string_buffer
let string_index = ref 0
let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0

let store_string_char c =
  if !string_index >= Bytes.length !string_buff then begin
    let new_buff = Bytes.create (Bytes.length (!string_buff) * 2) in
    Bytes.blit !string_buff 0 new_buff 0 (Bytes.length !string_buff);
    string_buff := new_buff
  end;
  Bytes.unsafe_set !string_buff !string_index c;
  incr string_index

let store_string s =
  for i = 0 to String.length s - 1 do
    store_string_char s.[i];
  done

let store_lexeme lexbuf =
  store_string (Lexing.lexeme lexbuf)

let get_stored_string () =
  let s = Bytes.sub_string !string_buff 0 !string_index in
  string_buff := initial_string_buffer;
  s

(*To store the position of the beginning of a string and comment *)

let string_start_loc = ref Ml_location.none
let is_in_string = ref false
let in_string () = !is_in_string

let comment_start_loc = ref []
let in_comment () = !comment_start_loc <> []

(*Comments*)

let comment_list = ref []

let add_comment (com : string * Ml_location.t) : unit =
  comment_list := com :: !comment_list

let comments () = List.rev !comment_list

let with_comment_buffer comment lexbuf =
  let start_loc = Ml_location.curr lexbuf  in
  comment_start_loc := [start_loc];
  reset_string_buffer ();
  let end_loc = comment lexbuf in
  let s = get_stored_string () in
  reset_string_buffer ();
  let loc = { start_loc 
              with Ml_location.loc_end = end_loc.Ml_location.loc_end } in
  s, loc

(*Error reporting*)

open Format

let report_error ppf = function
  | Illegal_character c -> 
    fprintf ppf "Illegal character (%s)" (Char.escaped c)
  | Unterminated_comment _ -> 
    fprintf ppf "Comment not terminated"

let () =
  Ml_location.register_error_of_exn
    (function
      | Error (err, loc) ->
          Some (Ml_location.error_of_printer loc report_error err)
      | _ ->  None
    )

}

let blank = [' ' '\009' '\012']
let newline = ('\013'* '\010')
let decimal_literal = ['0'-'9'] ['0'-'9' '_']*
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']

let lowercase = ['a'-'z' '_']
let uppercase = ['A'-'Z']

rule token = parse
  | newline             { update_loc lexbuf None 1 false 0; T_eol }
  | blank+                                         { token lexbuf }
  | "-"                                            { T_underscore }
  | "->"                                                { T_arrow }
  | ','                                                 { T_comma }
  | '+'                                                  { T_plus }
  | '-'                                                 { T_minus }
  | '*'                                                  { T_star }
  | '('                                                { T_lparen }
  | ')'                                                { T_rparen }
  | "="                                                    { T_eq }
  | '<'                                                    { T_lt }
  | "fun"                                                 { T_fun }
  | "let"                                                 { T_let }
  | "rec"                                                 { T_rec }
  | "in"                                                   { T_in }
  | "then"                                               { T_then }
  | "else"                                               { T_else }
  | "if"                                                   { T_if }
  | "true"                                               { T_true }
  | "false"                                             { T_false }
  | "fst"                                                 { T_fst }
  | "snd"                                                 { T_snd }
  | decimal_literal as i                                { T_int i }
  | lowercase identchar *
      { let s = Lexing.lexeme lexbuf in
        try 
          (*If its a keyword, look it up and return the associated
            token*)
          Hashtbl.find keyword_table s
        with Not_found -> T_ident s  (*Else, treat as identifier*)
      }
  | "(*"    { let s, loc = with_comment_buffer comment lexbuf in
              T_comment (s, loc) }
  | eof                                                  { T_eof  }
  | _ 
      {raise (Error (Illegal_character (Lexing.lexeme_char lexbuf 0)
                       , Ml_location.curr lexbuf)) }
and comment = parse
  | "(*"
      {
        comment_start_loc := 
        (Ml_location.curr lexbuf) :: !comment_start_loc;
        store_lexeme lexbuf;
        comment lexbuf
      }
  | "*)"
      {
        match !comment_start_loc with
        | [] -> assert false
        | [_] -> comment_start_loc := []; Ml_location.curr lexbuf
        | _ :: l -> 
          comment_start_loc := l;
          store_lexeme lexbuf;
          comment lexbuf
      }
  | eof
      {
        match !comment_start_loc with
        | [] -> assert false
        | loc :: _ ->
          let start = List.hd (List.rev !comment_start_loc) in
          comment_start_loc := [];
          raise (Error (Unterminated_comment start, loc))
      }
  | newline
      {
        update_loc lexbuf None 1 false 0;
        store_lexeme lexbuf;
        comment lexbuf
      }
  | _
      { store_lexeme lexbuf; comment lexbuf }

{
  let token lexbuf =
    let rec loop lexbuf = 
      match token lexbuf with
      | T_comment (s, loc) -> add_comment (s, loc); loop lexbuf
      | T_eol -> loop lexbuf
      | tok -> tok
    in loop lexbuf
}
