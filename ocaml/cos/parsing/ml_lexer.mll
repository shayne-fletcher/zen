{
(**The lexical analyzer*)

open Lexing
open Ml_parser

(**{2 Utilities}*)

(**Update the current location with file name and line
  number. [absolute] if [false] means add [line] to the current line
  number, if [true], replace it with [line] entirely*)
let update_loc 
    (lexbuf : lexbuf) 
    (file : string option)
    (line : int)
    (absolute : bool) 
    (chars :int) : unit =
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

(**[create_hashtable size init] creates a hashtable with [size] buckets
  with initial contents [init]*)
let create_hashtable 
    (size : int) 
    (init : ('a * 'b) list ): ('a, 'b) Hashtbl.t =
  let tbl = Hashtbl.create size in
  List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
  tbl

(**[keyword_table] associates keywords with their tokens*)
let keyword_table =
  create_hashtable 149 [
    "and", T_and;
    "else", T_else;
    "false", T_false;
    "fun", T_fun;
    "if", T_if;
    "in", T_in;
    "let", T_let;
    "match", T_match;
    "rec", T_rec;
    "then", T_then;
    "true", T_true;
    "when", T_when;
    "with", T_with;
  ]

(**An allocation of 256 bytes*)
let initial_string_buffer : bytes = Bytes.create 256
(**A string buffer reference*)
let string_buf : bytes ref = ref initial_string_buffer
(**The next unused index in the buffer*)
let string_index : int ref = ref 0

(**Reset the string buffer contents to the original allocation*)
let reset_string_buffer () =
  string_buf := initial_string_buffer;
  string_index := 0

(**Write a char [c] into the string buffer at the position indicated by
  the current [string_index], creating more space as
  neccessary. Increment [string_index]*)
let store_string_char (c : char) : unit =
  if !string_index >= Bytes.length !string_buf then begin
    let new_buf = Bytes.create (Bytes.length (!string_buf) * 2) in
    Bytes.blit !string_buf 0 new_buf 0 (Bytes.length !string_buf);
    string_buf := new_buf
  end;
  Bytes.unsafe_set !string_buf !string_index c;
  incr string_index

(**[store_string s] writes [s] into [string_buf] by way of
  [store_string_char]*)
let store_string (s : string) : unit =
  for i = 0 to String.length s - 1 do
    store_string_char s.[i];
  done

(**Called from the semantic actions of lexer definitions,
  [Lexing.lexeme lexubf] returns the string corresponding to the the
  matched regular expression. [store_lexeme lexbuf] stores this string
  in [string_buf] by way of [store_string]*)
let store_lexeme lexbuf =
  store_string (Lexing.lexeme lexbuf)

(**Gets the contents of [string_buff], from 0 to the current
  [string_index], resets the [string_buff] back to it's original
  allocation; does not modify [string_index]*)
let get_stored_string () =
  let s = Bytes.sub_string !string_buf 0 !string_index in
  string_buf := initial_string_buffer;
  s

(*Comments*)

(**A mutuable list of start locations of comments*)
let comment_start_loc = ref []

(**[in_comment ()] evaluates to [true] between the time a comment has
   been started and not ended, [false] at all other times*)
let in_comment () = !comment_start_loc <> []

(**A list of all comments accumulated during lexing*)
let comment_list : (string * Ml_location.t) list ref = ref []

(**Add a comment to the list*)
let add_comment (com : string * Ml_location.t) : unit =
  comment_list := com :: !comment_list

(**Retrieve the list of comments*)
let comments () : (string * Ml_location.t) list = List.rev !comment_list

(**[with_comment_buffer comment lexbuf] uses the string buffer
  [comment_start_loc] and the [comment] rule*)
let with_comment_buffer 
    (comment : lexbuf -> Ml_location.t)
    (lexbuf : lexbuf) : string * Ml_location.t =
  let start_loc = Ml_location.curr lexbuf  in
  comment_start_loc := [start_loc];
  reset_string_buffer ();
  let end_loc : Ml_location.t = comment lexbuf in
  let s = get_stored_string () in
  reset_string_buffer ();
  let loc = { start_loc 
              with Ml_location.loc_end = end_loc.Ml_location.loc_end } in
  s, loc

(**{2 Error reporting}*)

(**The type of errors that can come up in this module*)
type error =
| Illegal_character of char (**Unrecognizable token*)
| Unterminated_comment of Ml_location.t (**An unterimated comment*)

(**The type of exceptions that contain those errors*)
exception Error of error * Ml_location.t

(**This function takes a formatter and an instance of type [error] and
  writes a message to the formatter explaining the meaning. This is a
  "printer"*)
let report_error (ppf : Format.formatter) : error -> unit = function
  | Illegal_character c -> 
    Format.fprintf ppf "Illegal character (%s)" (Char.escaped c)
  | Unterminated_comment _ -> 
    Format.fprintf ppf "Comment not terminated"
(**Register an exception handler*)
let () =
  Ml_location.register_error_of_exn
    (function
      | Error (err, loc) ->
          Some (Ml_location.error_of_printer loc report_error err)
      | _ ->  None
    )

(**{2 Tables & rules}*)
}

let blank = [' ' '\009' '\012']
let newline = ('\013'* '\010')
let decimal_literal = ['0'-'9'] ['0'-'9' '_']*
let lowercase = ['a'-'z' '_']
let uppercase = ['A'-'Z']
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']
let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']

rule token = parse
  | newline             { update_loc lexbuf None 1 false 0; T_eol }
  | blank+                                         { token lexbuf }
  | "_"                                            { T_underscore }
  | "->"                                                { T_arrow }
  | ','                                                 { T_comma }
  | '+'                                                  { T_plus }
  | '-'                                                 { T_minus }
  | '*'                                                  { T_star }
  | '('                                                { T_lparen }
  | ')'                                                { T_rparen }
  | "!"                                                  { T_bang }
  | "::"                                           { T_coloncolon }
  | ";"                                                  { T_semi }
  | "|"                                                   { T_bar }
  | "||"                                               { T_barbar }
  | "&&"                                           { T_amperamper }
  | "="                                                    { T_eq }
  | "["                                              { T_lbracket }
  | "]"                                              { T_rbracket }
  | '<'                                                    { T_lt }
  | '>'                                                    { T_gt }
  | "!" symbolchar+          { T_prefix_op (Lexing.lexeme lexbuf) }
  | ['~' '?'] symbolchar+    { T_prefix_op (Lexing.lexeme lexbuf) }
  | ['=' '<' '>' '|' '&' '$'] symbolchar* { T_infixop0 (Lexing.lexeme lexbuf) }
  | ['@' '^'] symbolchar*     { T_infixop1 (Lexing.lexeme lexbuf) }
  | ['+' '-'] symbolchar*     { T_infixop2 (Lexing.lexeme lexbuf) }
  | ['*' '/' '%'] symbolchar* { T_infixop3 (Lexing.lexeme lexbuf) }
  | "**" symbolchar*          { T_infixop4 (Lexing.lexeme lexbuf) }
  | decimal_literal as i                                { T_int i }
  | lowercase identchar*
      { let s = Lexing.lexeme lexbuf in
        try 
          (*If its a keyword, look it up and return the associated
            token*)
          Hashtbl.find keyword_table s
        with Not_found -> T_ident s  (*Else, treat as identifier*)
      }
  | uppercase identchar* { 
                       let s = Lexing.lexeme lexbuf in T_uident s }
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
  (**{2 Postscript}*)

  (**A wrapper around the token rule that collects comment strings
    encountered during lexing and discards comment and end of line
    tokens*)
  let token (lexbuf : lexbuf) : token =
    let rec loop lexbuf = 
      match token lexbuf with
      | T_comment (s, loc) -> add_comment (s, loc); loop lexbuf
      | T_eol -> loop lexbuf
      | tok -> tok
    in loop lexbuf
}
