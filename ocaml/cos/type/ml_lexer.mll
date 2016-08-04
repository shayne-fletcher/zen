{
open Lexing
open Ml_parser

let advance_line lexbuf = 
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- 
    { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
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
}

let blank = [' ' '\009' '\012']
let newline = ('\013'* '\010')
let decimal_literal = ['0'-'9'] ['0'-'9' '_']*
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']

let lowercase = ['a'-'z' '_']
let uppercase = ['A'-'Z']


rule token = parse
  | "(*"                                      { comments 0 lexbuf }
  | newline                   { advance_line lexbuf; token lexbuf }
  | blank                                          { token lexbuf }
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
  | eof                                                  { T_eof  }
  | _ as c
          { raise (Ml_ast.Unrecognized_token (String.make 1 c)) }
and comments level = parse
  | "*)" 
      {if level=0 then token lexbuf else comments (level-1) lexbuf}
  | ['\n']         { advance_line lexbuf; (comments level lexbuf) }
  | "(*"                              { comments (level+1) lexbuf }
  | _                                     { comments level lexbuf }
  | eof                         { raise (Ml_ast.Unclosed_comment) }
