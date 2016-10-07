# 1 "ml_lexer.mll"
 
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
    "function", T_function;
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

# 169 "ml_lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\222\255\223\255\084\000\192\000\159\000\026\001\061\001\
    \096\001\131\001\166\001\201\001\237\255\238\255\236\001\015\002\
    \050\002\243\255\034\000\085\002\246\255\004\000\120\002\155\002\
    \251\255\190\002\220\002\002\000\255\255\005\000\006\000\054\003\
    \089\003\124\003\224\255\159\003\244\255\194\003\229\003\008\004\
    \135\000\251\255\252\255\007\000\253\255\055\000\083\000\255\255\
    \254\255\011\000";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\030\000\029\000\028\000\026\000\024\000\
    \023\000\033\000\020\000\019\000\255\255\255\255\016\000\023\000\
    \013\000\255\255\033\000\010\000\255\255\008\000\007\000\005\000\
    \255\255\006\000\002\000\001\000\255\255\033\000\255\255\025\000\
    \003\000\026\000\255\255\021\000\255\255\014\000\015\000\022\000\
    \255\255\255\255\255\255\004\000\255\255\004\000\004\000\255\255\
    \255\255\255\255";
  Lexing.lex_default = 
   "\001\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\000\000\255\255\255\255\
    \255\255\000\000\255\255\255\255\000\000\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\000\000\255\255\000\000\255\255\255\255\255\255\
    \041\000\000\000\000\000\255\255\000\000\255\255\255\255\000\000\
    \000\000\255\255";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\027\000\028\000\027\000\027\000\029\000\027\000\028\000\
    \028\000\042\000\030\000\030\000\049\000\042\000\000\000\000\000\
    \049\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \027\000\019\000\027\000\000\000\008\000\006\000\015\000\000\000\
    \021\000\020\000\022\000\023\000\024\000\025\000\034\000\006\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\018\000\017\000\011\000\014\000\010\000\009\000\
    \007\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\013\000\036\000\012\000\007\000\026\000\
    \048\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\003\000\016\000\047\000\009\000\000\000\
    \000\000\000\000\000\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\042\000\000\000\000\000\043\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\046\000\
    \000\000\045\000\000\000\003\000\000\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\004\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\000\000\000\000\000\000\000\000\005\000\000\000\
    \002\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\000\000\000\000\000\000\000\000\004\000\
    \000\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\006\000\000\000\000\000\006\000\006\000\
    \006\000\000\000\000\000\000\000\006\000\006\000\000\000\006\000\
    \006\000\006\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\006\000\000\000\006\000\006\000\
    \006\000\006\000\006\000\000\000\000\000\000\000\007\000\000\000\
    \000\000\007\000\007\000\007\000\000\000\000\000\000\000\007\000\
    \007\000\000\000\007\000\007\000\007\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\007\000\
    \006\000\007\000\007\000\007\000\007\000\007\000\000\000\000\000\
    \000\000\008\000\000\000\000\000\008\000\008\000\008\000\044\000\
    \000\000\000\000\008\000\008\000\000\000\008\000\008\000\008\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\006\000\000\000\
    \006\000\000\000\008\000\007\000\008\000\008\000\008\000\008\000\
    \008\000\000\000\000\000\000\000\039\000\000\000\000\000\039\000\
    \039\000\039\000\000\000\000\000\000\000\039\000\039\000\000\000\
    \039\000\039\000\039\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\007\000\000\000\007\000\000\000\039\000\008\000\039\000\
    \039\000\039\000\039\000\039\000\000\000\000\000\000\000\008\000\
    \000\000\000\000\008\000\008\000\008\000\000\000\000\000\000\000\
    \008\000\008\000\000\000\008\000\008\000\008\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\008\000\000\000\008\000\000\000\
    \008\000\039\000\008\000\008\000\008\000\008\000\008\000\000\000\
    \000\000\000\000\008\000\000\000\000\000\008\000\008\000\008\000\
    \000\000\000\000\000\000\008\000\008\000\000\000\008\000\008\000\
    \008\000\000\000\000\000\000\000\000\000\000\000\000\000\039\000\
    \000\000\039\000\000\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\000\000\000\000\000\000\008\000\000\000\000\000\
    \008\000\008\000\008\000\000\000\000\000\000\000\008\000\008\000\
    \000\000\008\000\008\000\008\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\008\000\000\000\008\000\000\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\000\000\000\000\000\000\
    \008\000\000\000\000\000\008\000\008\000\038\000\000\000\000\000\
    \000\000\008\000\008\000\000\000\008\000\008\000\008\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\008\000\000\000\008\000\
    \000\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \000\000\000\000\000\000\008\000\000\000\000\000\008\000\008\000\
    \008\000\000\000\000\000\000\000\008\000\008\000\000\000\008\000\
    \008\000\008\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \008\000\000\000\008\000\000\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\000\000\000\000\000\000\035\000\000\000\
    \000\000\035\000\035\000\035\000\000\000\000\000\000\000\035\000\
    \035\000\000\000\035\000\035\000\035\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\008\000\000\000\008\000\000\000\035\000\
    \008\000\035\000\035\000\035\000\035\000\035\000\000\000\000\000\
    \000\000\006\000\000\000\000\000\006\000\006\000\006\000\000\000\
    \000\000\000\000\033\000\006\000\000\000\006\000\006\000\006\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\037\000\000\000\
    \008\000\000\000\006\000\035\000\006\000\006\000\006\000\006\000\
    \006\000\000\000\000\000\000\000\031\000\000\000\000\000\031\000\
    \031\000\031\000\000\000\000\000\000\000\031\000\031\000\000\000\
    \031\000\031\000\031\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\035\000\000\000\035\000\000\000\031\000\006\000\031\000\
    \031\000\031\000\031\000\031\000\000\000\000\000\000\000\031\000\
    \000\000\000\000\031\000\031\000\031\000\000\000\000\000\000\000\
    \031\000\031\000\000\000\031\000\031\000\031\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\006\000\000\000\006\000\000\000\
    \031\000\031\000\031\000\031\000\032\000\031\000\031\000\000\000\
    \000\000\000\000\000\000\004\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\000\000\031\000\
    \000\000\031\000\000\000\000\000\031\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\000\000\
    \000\000\000\000\031\000\004\000\031\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\031\000\
    \000\000\000\000\031\000\031\000\031\000\000\000\000\000\000\000\
    \031\000\031\000\000\000\031\000\031\000\031\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \031\000\000\000\031\000\031\000\031\000\031\000\031\000\000\000\
    \000\000\000\000\031\000\000\000\000\000\031\000\031\000\031\000\
    \000\000\000\000\000\000\031\000\031\000\000\000\031\000\031\000\
    \031\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\000\000\000\000\000\000\033\000\000\000\000\000\
    \033\000\033\000\033\000\000\000\000\000\000\000\033\000\033\000\
    \000\000\033\000\033\000\033\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\031\000\000\000\031\000\000\000\033\000\031\000\
    \033\000\033\000\033\000\033\000\033\000\000\000\000\000\000\000\
    \035\000\000\000\000\000\035\000\035\000\035\000\000\000\000\000\
    \000\000\035\000\035\000\000\000\035\000\035\000\035\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\031\000\000\000\031\000\
    \000\000\035\000\033\000\035\000\035\000\035\000\035\000\035\000\
    \000\000\000\000\000\000\008\000\000\000\000\000\008\000\008\000\
    \008\000\000\000\000\000\000\000\008\000\008\000\000\000\008\000\
    \008\000\008\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \033\000\000\000\033\000\000\000\008\000\035\000\008\000\008\000\
    \008\000\008\000\008\000\000\000\000\000\000\000\008\000\000\000\
    \000\000\008\000\008\000\008\000\000\000\000\000\000\000\008\000\
    \008\000\000\000\008\000\008\000\008\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\035\000\000\000\035\000\000\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\000\000\000\000\
    \000\000\039\000\000\000\000\000\039\000\039\000\039\000\000\000\
    \000\000\000\000\039\000\039\000\000\000\039\000\039\000\039\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\008\000\000\000\
    \008\000\000\000\039\000\008\000\039\000\039\000\039\000\039\000\
    \039\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\008\000\000\000\008\000\000\000\000\000\039\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\039\000\000\000\039\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\027\000\000\000\000\000\027\000\029\000\
    \030\000\043\000\029\000\030\000\043\000\049\000\255\255\255\255\
    \049\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\027\000\255\255\000\000\000\000\000\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\021\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\018\000\000\000\000\000\000\000\
    \045\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\003\000\000\000\046\000\000\000\255\255\
    \255\255\255\255\255\255\255\255\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\255\255\255\255\
    \255\255\040\000\255\255\255\255\040\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\040\000\
    \255\255\040\000\255\255\003\000\255\255\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\004\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\255\255\255\255\255\255\255\255\005\000\255\255\
    \000\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\255\255\255\255\255\255\255\255\004\000\
    \255\255\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\006\000\255\255\255\255\006\000\006\000\
    \006\000\255\255\255\255\255\255\006\000\006\000\255\255\006\000\
    \006\000\006\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\006\000\255\255\006\000\006\000\
    \006\000\006\000\006\000\255\255\255\255\255\255\007\000\255\255\
    \255\255\007\000\007\000\007\000\255\255\255\255\255\255\007\000\
    \007\000\255\255\007\000\007\000\007\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\007\000\
    \006\000\007\000\007\000\007\000\007\000\007\000\255\255\255\255\
    \255\255\008\000\255\255\255\255\008\000\008\000\008\000\040\000\
    \255\255\255\255\008\000\008\000\255\255\008\000\008\000\008\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\006\000\255\255\
    \006\000\255\255\008\000\007\000\008\000\008\000\008\000\008\000\
    \008\000\255\255\255\255\255\255\009\000\255\255\255\255\009\000\
    \009\000\009\000\255\255\255\255\255\255\009\000\009\000\255\255\
    \009\000\009\000\009\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\007\000\255\255\007\000\255\255\009\000\008\000\009\000\
    \009\000\009\000\009\000\009\000\255\255\255\255\255\255\010\000\
    \255\255\255\255\010\000\010\000\010\000\255\255\255\255\255\255\
    \010\000\010\000\255\255\010\000\010\000\010\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\008\000\255\255\008\000\255\255\
    \010\000\009\000\010\000\010\000\010\000\010\000\010\000\255\255\
    \255\255\255\255\011\000\255\255\255\255\011\000\011\000\011\000\
    \255\255\255\255\255\255\011\000\011\000\255\255\011\000\011\000\
    \011\000\255\255\255\255\255\255\255\255\255\255\255\255\009\000\
    \255\255\009\000\255\255\011\000\010\000\011\000\011\000\011\000\
    \011\000\011\000\255\255\255\255\255\255\014\000\255\255\255\255\
    \014\000\014\000\014\000\255\255\255\255\255\255\014\000\014\000\
    \255\255\014\000\014\000\014\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\010\000\255\255\010\000\255\255\014\000\011\000\
    \014\000\014\000\014\000\014\000\014\000\255\255\255\255\255\255\
    \015\000\255\255\255\255\015\000\015\000\015\000\255\255\255\255\
    \255\255\015\000\015\000\255\255\015\000\015\000\015\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\011\000\255\255\011\000\
    \255\255\015\000\014\000\015\000\015\000\015\000\015\000\015\000\
    \255\255\255\255\255\255\016\000\255\255\255\255\016\000\016\000\
    \016\000\255\255\255\255\255\255\016\000\016\000\255\255\016\000\
    \016\000\016\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \014\000\255\255\014\000\255\255\016\000\015\000\016\000\016\000\
    \016\000\016\000\016\000\255\255\255\255\255\255\019\000\255\255\
    \255\255\019\000\019\000\019\000\255\255\255\255\255\255\019\000\
    \019\000\255\255\019\000\019\000\019\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\015\000\255\255\015\000\255\255\019\000\
    \016\000\019\000\019\000\019\000\019\000\019\000\255\255\255\255\
    \255\255\022\000\255\255\255\255\022\000\022\000\022\000\255\255\
    \255\255\255\255\022\000\022\000\255\255\022\000\022\000\022\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\016\000\255\255\
    \016\000\255\255\022\000\019\000\022\000\022\000\022\000\022\000\
    \022\000\255\255\255\255\255\255\023\000\255\255\255\255\023\000\
    \023\000\023\000\255\255\255\255\255\255\023\000\023\000\255\255\
    \023\000\023\000\023\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\019\000\255\255\019\000\255\255\023\000\022\000\023\000\
    \023\000\023\000\023\000\023\000\255\255\255\255\255\255\025\000\
    \255\255\255\255\025\000\025\000\025\000\255\255\255\255\255\255\
    \025\000\025\000\255\255\025\000\025\000\025\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\022\000\255\255\022\000\255\255\
    \025\000\023\000\025\000\025\000\025\000\025\000\025\000\255\255\
    \255\255\255\255\255\255\026\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\255\255\023\000\
    \255\255\023\000\255\255\255\255\025\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\255\255\
    \255\255\255\255\025\000\026\000\025\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\031\000\
    \255\255\255\255\031\000\031\000\031\000\255\255\255\255\255\255\
    \031\000\031\000\255\255\031\000\031\000\031\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \031\000\255\255\031\000\031\000\031\000\031\000\031\000\255\255\
    \255\255\255\255\032\000\255\255\255\255\032\000\032\000\032\000\
    \255\255\255\255\255\255\032\000\032\000\255\255\032\000\032\000\
    \032\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\032\000\031\000\032\000\032\000\032\000\
    \032\000\032\000\255\255\255\255\255\255\033\000\255\255\255\255\
    \033\000\033\000\033\000\255\255\255\255\255\255\033\000\033\000\
    \255\255\033\000\033\000\033\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\031\000\255\255\031\000\255\255\033\000\032\000\
    \033\000\033\000\033\000\033\000\033\000\255\255\255\255\255\255\
    \035\000\255\255\255\255\035\000\035\000\035\000\255\255\255\255\
    \255\255\035\000\035\000\255\255\035\000\035\000\035\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\032\000\255\255\032\000\
    \255\255\035\000\033\000\035\000\035\000\035\000\035\000\035\000\
    \255\255\255\255\255\255\037\000\255\255\255\255\037\000\037\000\
    \037\000\255\255\255\255\255\255\037\000\037\000\255\255\037\000\
    \037\000\037\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \033\000\255\255\033\000\255\255\037\000\035\000\037\000\037\000\
    \037\000\037\000\037\000\255\255\255\255\255\255\038\000\255\255\
    \255\255\038\000\038\000\038\000\255\255\255\255\255\255\038\000\
    \038\000\255\255\038\000\038\000\038\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\035\000\255\255\035\000\255\255\038\000\
    \037\000\038\000\038\000\038\000\038\000\038\000\255\255\255\255\
    \255\255\039\000\255\255\255\255\039\000\039\000\039\000\255\255\
    \255\255\255\255\039\000\039\000\255\255\039\000\039\000\039\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\037\000\255\255\
    \037\000\255\255\039\000\038\000\039\000\039\000\039\000\039\000\
    \039\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\038\000\255\255\038\000\255\255\255\255\039\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\039\000\255\255\039\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec token lexbuf =
    __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 178 "ml_lexer.mll"
                        ( update_loc lexbuf None 1 false 0; T_eol )
# 542 "ml_lexer.ml"

  | 1 ->
# 179 "ml_lexer.mll"
                                                   ( token lexbuf )
# 547 "ml_lexer.ml"

  | 2 ->
# 180 "ml_lexer.mll"
                                                   ( T_underscore )
# 552 "ml_lexer.ml"

  | 3 ->
# 181 "ml_lexer.mll"
                                                        ( T_arrow )
# 557 "ml_lexer.ml"

  | 4 ->
# 182 "ml_lexer.mll"
                                                        ( T_comma )
# 562 "ml_lexer.ml"

  | 5 ->
# 183 "ml_lexer.mll"
                                                         ( T_plus )
# 567 "ml_lexer.ml"

  | 6 ->
# 184 "ml_lexer.mll"
                                                        ( T_minus )
# 572 "ml_lexer.ml"

  | 7 ->
# 185 "ml_lexer.mll"
                                                         ( T_star )
# 577 "ml_lexer.ml"

  | 8 ->
# 186 "ml_lexer.mll"
                                                       ( T_lparen )
# 582 "ml_lexer.ml"

  | 9 ->
# 187 "ml_lexer.mll"
                                                       ( T_rparen )
# 587 "ml_lexer.ml"

  | 10 ->
# 188 "ml_lexer.mll"
                                                         ( T_bang )
# 592 "ml_lexer.ml"

  | 11 ->
# 189 "ml_lexer.mll"
                                                   ( T_coloncolon )
# 597 "ml_lexer.ml"

  | 12 ->
# 190 "ml_lexer.mll"
                                                         ( T_semi )
# 602 "ml_lexer.ml"

  | 13 ->
# 191 "ml_lexer.mll"
                                                          ( T_bar )
# 607 "ml_lexer.ml"

  | 14 ->
# 192 "ml_lexer.mll"
                                                       ( T_barbar )
# 612 "ml_lexer.ml"

  | 15 ->
# 193 "ml_lexer.mll"
                                                   ( T_amperamper )
# 617 "ml_lexer.ml"

  | 16 ->
# 194 "ml_lexer.mll"
                                                           ( T_eq )
# 622 "ml_lexer.ml"

  | 17 ->
# 195 "ml_lexer.mll"
                                                     ( T_lbracket )
# 627 "ml_lexer.ml"

  | 18 ->
# 196 "ml_lexer.mll"
                                                     ( T_rbracket )
# 632 "ml_lexer.ml"

  | 19 ->
# 197 "ml_lexer.mll"
                                                           ( T_lt )
# 637 "ml_lexer.ml"

  | 20 ->
# 198 "ml_lexer.mll"
                                                           ( T_gt )
# 642 "ml_lexer.ml"

  | 21 ->
# 199 "ml_lexer.mll"
                             ( T_prefix_op (Lexing.lexeme lexbuf) )
# 647 "ml_lexer.ml"

  | 22 ->
# 200 "ml_lexer.mll"
                             ( T_prefix_op (Lexing.lexeme lexbuf) )
# 652 "ml_lexer.ml"

  | 23 ->
# 201 "ml_lexer.mll"
                                          ( T_infixop0 (Lexing.lexeme lexbuf) )
# 657 "ml_lexer.ml"

  | 24 ->
# 202 "ml_lexer.mll"
                              ( T_infixop1 (Lexing.lexeme lexbuf) )
# 662 "ml_lexer.ml"

  | 25 ->
# 203 "ml_lexer.mll"
                              ( T_infixop2 (Lexing.lexeme lexbuf) )
# 667 "ml_lexer.ml"

  | 26 ->
# 204 "ml_lexer.mll"
                              ( T_infixop3 (Lexing.lexeme lexbuf) )
# 672 "ml_lexer.ml"

  | 27 ->
# 205 "ml_lexer.mll"
                              ( T_infixop4 (Lexing.lexeme lexbuf) )
# 677 "ml_lexer.ml"

  | 28 ->
let
# 206 "ml_lexer.mll"
                       i
# 683 "ml_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 206 "ml_lexer.mll"
                                                        ( T_int i )
# 687 "ml_lexer.ml"

  | 29 ->
# 208 "ml_lexer.mll"
      ( let s = Lexing.lexeme lexbuf in
        try 
          (*If its a keyword, look it up and return the associated
            token*)
          Hashtbl.find keyword_table s
        with Not_found -> T_ident s  (*Else, treat as identifier*)
      )
# 698 "ml_lexer.ml"

  | 30 ->
# 215 "ml_lexer.mll"
                         ( 
                       let s = Lexing.lexeme lexbuf in T_uident s )
# 704 "ml_lexer.ml"

  | 31 ->
# 217 "ml_lexer.mll"
            ( let s, loc = with_comment_buffer comment lexbuf in
              T_comment (s, loc) )
# 710 "ml_lexer.ml"

  | 32 ->
# 219 "ml_lexer.mll"
                                                         ( T_eof  )
# 715 "ml_lexer.ml"

  | 33 ->
# 221 "ml_lexer.mll"
      (raise (Error (Illegal_character (Lexing.lexeme_char lexbuf 0)
                       , Ml_location.curr lexbuf)) )
# 721 "ml_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

and comment lexbuf =
    __ocaml_lex_comment_rec lexbuf 40
and __ocaml_lex_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 225 "ml_lexer.mll"
      (
        comment_start_loc := 
        (Ml_location.curr lexbuf) :: !comment_start_loc;
        store_lexeme lexbuf;
        comment lexbuf
      )
# 738 "ml_lexer.ml"

  | 1 ->
# 232 "ml_lexer.mll"
      (
        match !comment_start_loc with
        | [] -> assert false
        | [_] -> comment_start_loc := []; Ml_location.curr lexbuf
        | _ :: l -> 
          comment_start_loc := l;
          store_lexeme lexbuf;
          comment lexbuf
      )
# 751 "ml_lexer.ml"

  | 2 ->
# 242 "ml_lexer.mll"
      (
        match !comment_start_loc with
        | [] -> assert false
        | loc :: _ ->
          let start = List.hd (List.rev !comment_start_loc) in
          comment_start_loc := [];
          raise (Error (Unterminated_comment start, loc))
      )
# 763 "ml_lexer.ml"

  | 3 ->
# 251 "ml_lexer.mll"
      (
        update_loc lexbuf None 1 false 0;
        store_lexeme lexbuf;
        comment lexbuf
      )
# 772 "ml_lexer.ml"

  | 4 ->
# 257 "ml_lexer.mll"
      ( store_lexeme lexbuf; comment lexbuf )
# 777 "ml_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_comment_rec lexbuf __ocaml_lex_state

;;

# 259 "ml_lexer.mll"
 
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

# 799 "ml_lexer.ml"