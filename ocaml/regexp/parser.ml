type token =
  | Tchar of (int)
  | Tend
  | Tor
  | Tlbracket
  | Trbracket
  | Tstar
  | Tmaybe
  | Tplus
  | Tlparen
  | Trparen

open Parsing;;
let _ = parse_error;;
let yytransl_const = [|
  258 (* Tend *);
  259 (* Tor *);
  260 (* Tlbracket *);
  261 (* Trbracket *);
  262 (* Tstar *);
  263 (* Tmaybe *);
  264 (* Tplus *);
  265 (* Tlparen *);
  266 (* Trparen *);
    0|]

let yytransl_block = [|
  257 (* Tchar *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\000\000"

let yylen = "\002\000\
\002\000\001\000\002\000\002\000\002\000\003\000\002\000\003\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\000\000\009\000\000\000\000\000\001\000\
\000\000\003\000\004\000\005\000\000\000\008\000\000\000"

let yydgoto = "\002\000\
\005\000\013\000"

let yysindex = "\002\000\
\027\255\000\000\000\000\027\255\000\000\009\255\255\254\000\000\
\027\255\000\000\000\000\000\000\013\255\000\000\013\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\021\255\000\000\023\255"

let yygindex = "\000\000\
\000\000\026\000"

let yytablesize = 36
let yytable = "\003\000\
\000\000\009\000\001\000\000\000\010\000\011\000\012\000\004\000\
\014\000\003\000\008\000\009\000\000\000\003\000\010\000\011\000\
\012\000\004\000\010\000\011\000\012\000\004\000\007\000\007\000\
\006\000\006\000\006\000\003\000\000\000\007\000\007\000\000\000\
\006\000\000\000\015\000\004\000"

let yycheck = "\001\001\
\255\255\003\001\001\000\255\255\006\001\007\001\008\001\009\001\
\010\001\001\001\002\001\003\001\255\255\001\001\006\001\007\001\
\008\001\009\001\006\001\007\001\008\001\009\001\002\001\003\001\
\002\001\003\001\001\000\001\001\255\255\004\000\010\001\255\255\
\010\001\255\255\009\000\009\001"

let yynames_const = "\
  Tend\000\
  Tor\000\
  Tlbracket\000\
  Trbracket\000\
  Tstar\000\
  Tmaybe\000\
  Tplus\000\
  Tlparen\000\
  Trparen\000\
  "

let yynames_block = "\
  Tchar\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'regexp) in
    Obj.repr(
# 19 "parser.mly"
     ()
# 96 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 23 "parser.mly"
     ()
# 103 "parser.ml"
               : 'regexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'regexp) in
    Obj.repr(
# 25 "parser.mly"
     ()
# 110 "parser.ml"
               : 'regexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'regexp) in
    Obj.repr(
# 27 "parser.mly"
     ()
# 117 "parser.ml"
               : 'regexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'regexp) in
    Obj.repr(
# 29 "parser.mly"
     ()
# 124 "parser.ml"
               : 'regexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'regexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'regexp) in
    Obj.repr(
# 31 "parser.mly"
     ()
# 132 "parser.ml"
               : 'regexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'regexp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'regexp) in
    Obj.repr(
# 33 "parser.mly"
     ()
# 140 "parser.ml"
               : 'regexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'regexp) in
    Obj.repr(
# 35 "parser.mly"
     ()
# 147 "parser.ml"
               : 'regexp))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : unit)
