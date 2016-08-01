type token =
  | UNIT of (Mini_ml_types.sref)
  | TRUE of (Mini_ml_types.sref)
  | FALSE of (Mini_ml_types.sref)
  | PLUS of (Mini_ml_types.sref)
  | MINUS of (Mini_ml_types.sref)
  | TIMES of (Mini_ml_types.sref)
  | DIVIDE of (Mini_ml_types.sref)
  | LPAREN of (Mini_ml_types.sref)
  | RPAREN of (Mini_ml_types.sref)
  | COMMA of (Mini_ml_types.sref)
  | ARROW of (Mini_ml_types.sref)
  | SEMICOLON of (Mini_ml_types.sref)
  | EQ of (Mini_ml_types.sref)
  | NE of (Mini_ml_types.sref)
  | LT of (Mini_ml_types.sref)
  | LE of (Mini_ml_types.sref)
  | GT of (Mini_ml_types.sref)
  | GE of (Mini_ml_types.sref)
  | FUN of (Mini_ml_types.sref)
  | LET of (Mini_ml_types.sref)
  | REC of (Mini_ml_types.sref)
  | IN of (Mini_ml_types.sref)
  | IF of (Mini_ml_types.sref)
  | THEN of (Mini_ml_types.sref)
  | ELSE of (Mini_ml_types.sref)
  | AND of (Mini_ml_types.sref)
  | OR of (Mini_ml_types.sref)
  | NOT of (Mini_ml_types.sref)
  | EXP of (Mini_ml_types.sref)
  | LOG of (Mini_ml_types.sref)
  | SQRT of (Mini_ml_types.sref)
  | HD of (Mini_ml_types.sref)
  | TL of (Mini_ml_types.sref)
  | LEN of (Mini_ml_types.sref)
  | EOI of (Mini_ml_types.sref)
  | INT of (int*Mini_ml_types.sref)
  | FLOAT of (float*Mini_ml_types.sref)
  | OP of (string*Mini_ml_types.sref)
  | VAR of (string*Mini_ml_types.sref)

open Parsing;;
let _ = parse_error;;
# 2 "mini_ml_parser.mly"
let mkop (s, loc) operand = Mini_ml_types.E_unop (s, operand, loc)
let mkbinop s left right loc = Mini_ml_types.E_binop (s, left, right, loc)
# 48 "mini_ml_parser.ml"
let yytransl_const = [|
    0|]

let yytransl_block = [|
  257 (* UNIT *);
  258 (* TRUE *);
  259 (* FALSE *);
  260 (* PLUS *);
  261 (* MINUS *);
  262 (* TIMES *);
  263 (* DIVIDE *);
  264 (* LPAREN *);
  265 (* RPAREN *);
  266 (* COMMA *);
  267 (* ARROW *);
  268 (* SEMICOLON *);
  269 (* EQ *);
  270 (* NE *);
  271 (* LT *);
  272 (* LE *);
  273 (* GT *);
  274 (* GE *);
  275 (* FUN *);
  276 (* LET *);
  277 (* REC *);
  278 (* IN *);
  279 (* IF *);
  280 (* THEN *);
  281 (* ELSE *);
  282 (* AND *);
  283 (* OR *);
  284 (* NOT *);
  285 (* EXP *);
  286 (* LOG *);
  287 (* SQRT *);
  288 (* HD *);
  289 (* TL *);
  290 (* LEN *);
  291 (* EOI *);
  292 (* INT *);
  293 (* FLOAT *);
  294 (* OP *);
  295 (* VAR *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\006\000\007\000\
\007\000\008\000\008\000\009\000\009\000\010\000\010\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\012\000\012\000\
\012\000\012\000\013\000\013\000\013\000\014\000\014\000\014\000\
\014\000\014\000\014\000\014\000\014\000\014\000\015\000\015\000\
\005\000\017\000\017\000\016\000\018\000\018\000\018\000\018\000\
\018\000\018\000\018\000\004\000\000\000"

let yylen = "\002\000\
\002\000\003\000\001\000\008\000\006\000\007\000\005\000\007\000\
\005\000\006\000\004\000\006\000\004\000\001\000\001\000\002\000\
\001\000\003\000\001\000\003\000\001\000\003\000\001\000\003\000\
\003\000\003\000\003\000\003\000\003\000\001\000\002\000\003\000\
\003\000\001\000\003\000\003\000\001\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\001\000\002\000\001\000\
\001\000\003\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\003\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\053\000\056\000\057\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\054\000\055\000\000\000\060\000\061\000\000\000\003\000\
\058\000\014\000\015\000\000\000\000\000\000\000\000\000\000\000\
\000\000\037\000\000\000\048\000\052\000\031\000\000\000\000\000\
\000\000\000\000\000\000\051\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\047\000\059\000\000\000\
\000\000\000\000\000\000\000\000\000\000\002\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\035\000\036\000\013\000\000\000\000\000\000\000\000\000\
\050\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\010\000\012\000\006\000\000\000\008\000\004\000"

let yydgoto = "\002\000\
\022\000\023\000\024\000\025\000\043\000\026\000\027\000\028\000\
\029\000\030\000\031\000\032\000\033\000\034\000\035\000\036\000\
\045\000\037\000"

let yysindex = "\008\000\
\082\255\000\000\000\000\000\000\000\000\121\255\082\255\082\255\
\004\255\082\255\030\255\030\255\030\255\030\255\030\255\030\255\
\030\255\000\000\000\000\030\255\000\000\000\000\001\255\000\000\
\000\000\000\000\000\000\248\254\243\254\250\254\129\255\012\255\
\017\255\000\000\030\255\000\000\000\000\000\000\019\255\035\255\
\008\255\030\255\044\255\000\000\054\255\047\255\030\255\030\255\
\030\255\030\255\030\255\030\255\030\255\030\255\082\255\000\000\
\121\255\121\255\121\255\121\255\121\255\121\255\121\255\121\255\
\121\255\133\255\133\255\133\255\133\255\000\000\000\000\082\255\
\172\255\059\255\082\255\030\255\082\255\000\000\243\254\250\254\
\129\255\012\255\012\255\012\255\012\255\012\255\012\255\017\255\
\017\255\000\000\000\000\000\000\082\255\060\255\082\255\052\255\
\000\000\051\255\055\255\082\255\057\255\082\255\082\255\082\255\
\058\255\082\255\000\000\000\000\000\000\082\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\255\001\230\001\203\001\175\001\250\000\
\169\000\000\000\177\255\000\000\000\000\000\000\000\000\000\000\
\000\000\005\255\000\000\000\000\068\255\000\000\209\255\236\255\
\007\000\034\000\061\000\088\000\115\000\142\000\000\000\000\000\
\018\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\249\001\222\001\
\194\001\021\001\048\001\075\001\102\001\129\001\156\001\196\000\
\223\000\000\000\000\000\000\000\000\000\000\000\000\000\010\255\
\000\000\000\000\245\000\000\000\016\001\000\000\000\000\000\000\
\043\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\249\255\001\000\222\255\000\000\000\000\000\000\
\025\000\031\000\032\000\254\255\216\255\217\255\034\003\002\000\
\000\000\000\000"

let yytablesize = 822
let yytable = "\039\000\
\040\000\057\000\046\000\038\000\003\000\004\000\005\000\074\000\
\001\000\042\000\044\000\007\000\055\000\058\000\058\000\066\000\
\067\000\058\000\011\000\059\000\011\000\011\000\068\000\069\000\
\041\000\088\000\089\000\071\000\090\000\091\000\003\000\004\000\
\005\000\011\000\011\000\056\000\070\000\007\000\094\000\018\000\
\019\000\073\000\021\000\044\000\011\000\072\000\021\000\078\000\
\070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
\075\000\082\000\083\000\084\000\085\000\086\000\087\000\076\000\
\092\000\018\000\019\000\096\000\021\000\098\000\077\000\095\000\
\100\000\102\000\044\000\103\000\104\000\097\000\106\000\110\000\
\049\000\079\000\003\000\004\000\005\000\099\000\006\000\101\000\
\080\000\007\000\081\000\000\000\105\000\000\000\107\000\108\000\
\109\000\000\000\111\000\000\000\008\000\009\000\112\000\000\000\
\010\000\000\000\000\000\000\000\000\000\011\000\012\000\013\000\
\014\000\015\000\016\000\017\000\000\000\018\000\019\000\020\000\
\021\000\003\000\004\000\005\000\000\000\006\000\000\000\000\000\
\007\000\000\000\000\000\000\000\000\000\003\000\004\000\005\000\
\000\000\000\000\000\000\000\000\007\000\060\000\061\000\062\000\
\063\000\064\000\065\000\000\000\011\000\012\000\013\000\014\000\
\015\000\016\000\017\000\000\000\018\000\019\000\020\000\021\000\
\011\000\012\000\013\000\014\000\015\000\016\000\017\000\000\000\
\018\000\019\000\020\000\021\000\003\000\004\000\005\000\000\000\
\000\000\000\000\000\000\007\000\046\000\046\000\046\000\046\000\
\093\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
\046\000\046\000\046\000\000\000\000\000\000\000\046\000\000\000\
\046\000\046\000\046\000\046\000\000\000\000\000\000\000\018\000\
\019\000\000\000\021\000\046\000\038\000\038\000\038\000\038\000\
\000\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
\038\000\038\000\038\000\000\000\000\000\000\000\038\000\000\000\
\038\000\038\000\038\000\038\000\000\000\000\000\000\000\040\000\
\040\000\040\000\040\000\038\000\040\000\040\000\040\000\040\000\
\040\000\040\000\040\000\040\000\040\000\040\000\000\000\000\000\
\000\000\040\000\000\000\040\000\040\000\040\000\040\000\000\000\
\000\000\000\000\041\000\041\000\041\000\041\000\040\000\041\000\
\041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
\041\000\000\000\000\000\000\000\041\000\000\000\041\000\041\000\
\041\000\041\000\000\000\000\000\000\000\039\000\039\000\039\000\
\039\000\041\000\039\000\039\000\039\000\039\000\039\000\039\000\
\039\000\039\000\039\000\039\000\000\000\000\000\000\000\039\000\
\000\000\039\000\039\000\039\000\039\000\000\000\000\000\000\000\
\043\000\043\000\043\000\043\000\039\000\043\000\043\000\043\000\
\043\000\043\000\043\000\043\000\043\000\043\000\043\000\000\000\
\000\000\000\000\043\000\000\000\043\000\043\000\043\000\043\000\
\000\000\000\000\000\000\044\000\044\000\044\000\044\000\043\000\
\044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
\044\000\044\000\000\000\000\000\000\000\044\000\000\000\044\000\
\044\000\044\000\044\000\000\000\000\000\000\000\042\000\042\000\
\042\000\042\000\044\000\042\000\042\000\042\000\042\000\042\000\
\042\000\042\000\042\000\042\000\042\000\000\000\000\000\000\000\
\042\000\000\000\042\000\042\000\042\000\042\000\000\000\000\000\
\000\000\045\000\045\000\045\000\045\000\042\000\045\000\045\000\
\045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
\000\000\000\000\000\000\045\000\000\000\045\000\045\000\045\000\
\045\000\000\000\000\000\000\000\034\000\034\000\000\000\000\000\
\045\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
\034\000\034\000\034\000\000\000\000\000\000\000\034\000\000\000\
\034\000\034\000\034\000\034\000\000\000\000\000\000\000\032\000\
\032\000\000\000\000\000\034\000\032\000\032\000\032\000\032\000\
\032\000\032\000\032\000\032\000\032\000\032\000\000\000\000\000\
\000\000\032\000\000\000\032\000\032\000\032\000\032\000\000\000\
\000\000\000\000\033\000\033\000\000\000\000\000\032\000\033\000\
\033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
\033\000\000\000\000\000\000\000\033\000\000\000\033\000\033\000\
\033\000\033\000\000\000\000\000\000\000\007\000\000\000\007\000\
\007\000\033\000\030\000\030\000\030\000\030\000\030\000\030\000\
\030\000\030\000\030\000\030\000\007\000\007\000\000\000\030\000\
\000\000\030\000\030\000\030\000\030\000\000\000\000\000\007\000\
\009\000\000\000\009\000\009\000\030\000\024\000\024\000\024\000\
\024\000\024\000\024\000\024\000\024\000\024\000\024\000\009\000\
\009\000\000\000\024\000\000\000\024\000\024\000\024\000\024\000\
\000\000\000\000\009\000\005\000\000\000\005\000\005\000\024\000\
\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
\025\000\025\000\005\000\005\000\000\000\025\000\000\000\025\000\
\025\000\025\000\025\000\000\000\000\000\005\000\000\000\000\000\
\000\000\000\000\025\000\026\000\026\000\026\000\026\000\026\000\
\026\000\026\000\026\000\026\000\026\000\000\000\000\000\000\000\
\026\000\000\000\026\000\026\000\026\000\026\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\026\000\027\000\027\000\
\027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
\000\000\000\000\000\000\027\000\000\000\027\000\027\000\027\000\
\027\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\027\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
\028\000\028\000\028\000\000\000\000\000\000\000\028\000\000\000\
\028\000\028\000\028\000\028\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\028\000\029\000\029\000\029\000\029\000\
\029\000\029\000\029\000\029\000\029\000\029\000\000\000\000\000\
\000\000\029\000\000\000\029\000\029\000\029\000\029\000\023\000\
\023\000\023\000\023\000\000\000\000\000\000\000\029\000\000\000\
\000\000\000\000\000\000\000\000\023\000\000\000\023\000\023\000\
\023\000\023\000\022\000\022\000\022\000\022\000\000\000\000\000\
\000\000\023\000\000\000\021\000\021\000\021\000\021\000\022\000\
\000\000\022\000\022\000\022\000\022\000\000\000\000\000\000\000\
\021\000\000\000\021\000\021\000\022\000\021\000\020\000\020\000\
\020\000\020\000\000\000\000\000\000\000\021\000\019\000\019\000\
\019\000\019\000\000\000\020\000\000\000\020\000\020\000\000\000\
\020\000\000\000\000\000\019\000\000\000\019\000\019\000\000\000\
\020\000\018\000\018\000\018\000\018\000\000\000\000\000\017\000\
\019\000\017\000\017\000\000\000\000\000\000\000\018\000\000\000\
\018\000\018\000\000\000\000\000\017\000\000\000\017\000\017\000\
\000\000\000\000\016\000\018\000\016\000\016\000\000\000\000\000\
\000\000\017\000\000\000\000\000\000\000\000\000\000\000\016\000\
\000\000\016\000\016\000\000\000\047\000\048\000\049\000\050\000\
\051\000\052\000\053\000\000\000\016\000\054\000"

let yycheck = "\007\000\
\008\000\010\001\010\000\006\000\001\001\002\001\003\001\042\000\
\001\000\009\000\009\000\008\001\012\001\027\001\010\001\004\001\
\005\001\013\001\009\001\026\001\011\001\012\001\006\001\007\001\
\021\001\066\000\067\000\009\001\068\000\069\000\001\001\002\001\
\003\001\024\001\025\001\035\001\035\000\008\001\073\000\036\001\
\037\001\041\000\039\001\042\000\035\001\011\001\039\001\055\000\
\047\000\048\000\049\000\050\000\051\000\052\000\053\000\054\000\
\013\001\060\000\061\000\062\000\063\000\064\000\065\000\010\001\
\072\000\036\001\037\001\075\000\039\001\077\000\024\001\013\001\
\013\001\022\001\073\000\025\001\022\001\076\000\022\001\022\001\
\013\001\057\000\001\001\002\001\003\001\093\000\005\001\095\000\
\058\000\008\001\059\000\255\255\100\000\255\255\102\000\103\000\
\104\000\255\255\106\000\255\255\019\001\020\001\110\000\255\255\
\023\001\255\255\255\255\255\255\255\255\028\001\029\001\030\001\
\031\001\032\001\033\001\034\001\255\255\036\001\037\001\038\001\
\039\001\001\001\002\001\003\001\255\255\005\001\255\255\255\255\
\008\001\255\255\255\255\255\255\255\255\001\001\002\001\003\001\
\255\255\255\255\255\255\255\255\008\001\013\001\014\001\015\001\
\016\001\017\001\018\001\255\255\028\001\029\001\030\001\031\001\
\032\001\033\001\034\001\255\255\036\001\037\001\038\001\039\001\
\028\001\029\001\030\001\031\001\032\001\033\001\034\001\255\255\
\036\001\037\001\038\001\039\001\001\001\002\001\003\001\255\255\
\255\255\255\255\255\255\008\001\004\001\005\001\006\001\007\001\
\013\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\255\255\255\255\255\255\022\001\255\255\
\024\001\025\001\026\001\027\001\255\255\255\255\255\255\036\001\
\037\001\255\255\039\001\035\001\004\001\005\001\006\001\007\001\
\255\255\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\255\255\255\255\255\255\022\001\255\255\
\024\001\025\001\026\001\027\001\255\255\255\255\255\255\004\001\
\005\001\006\001\007\001\035\001\009\001\010\001\011\001\012\001\
\013\001\014\001\015\001\016\001\017\001\018\001\255\255\255\255\
\255\255\022\001\255\255\024\001\025\001\026\001\027\001\255\255\
\255\255\255\255\004\001\005\001\006\001\007\001\035\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\255\255\255\255\255\255\022\001\255\255\024\001\025\001\
\026\001\027\001\255\255\255\255\255\255\004\001\005\001\006\001\
\007\001\035\001\009\001\010\001\011\001\012\001\013\001\014\001\
\015\001\016\001\017\001\018\001\255\255\255\255\255\255\022\001\
\255\255\024\001\025\001\026\001\027\001\255\255\255\255\255\255\
\004\001\005\001\006\001\007\001\035\001\009\001\010\001\011\001\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\255\255\
\255\255\255\255\022\001\255\255\024\001\025\001\026\001\027\001\
\255\255\255\255\255\255\004\001\005\001\006\001\007\001\035\001\
\009\001\010\001\011\001\012\001\013\001\014\001\015\001\016\001\
\017\001\018\001\255\255\255\255\255\255\022\001\255\255\024\001\
\025\001\026\001\027\001\255\255\255\255\255\255\004\001\005\001\
\006\001\007\001\035\001\009\001\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\255\255\255\255\255\255\
\022\001\255\255\024\001\025\001\026\001\027\001\255\255\255\255\
\255\255\004\001\005\001\006\001\007\001\035\001\009\001\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
\255\255\255\255\255\255\022\001\255\255\024\001\025\001\026\001\
\027\001\255\255\255\255\255\255\004\001\005\001\255\255\255\255\
\035\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\255\255\255\255\255\255\022\001\255\255\
\024\001\025\001\026\001\027\001\255\255\255\255\255\255\004\001\
\005\001\255\255\255\255\035\001\009\001\010\001\011\001\012\001\
\013\001\014\001\015\001\016\001\017\001\018\001\255\255\255\255\
\255\255\022\001\255\255\024\001\025\001\026\001\027\001\255\255\
\255\255\255\255\004\001\005\001\255\255\255\255\035\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\255\255\255\255\255\255\022\001\255\255\024\001\025\001\
\026\001\027\001\255\255\255\255\255\255\009\001\255\255\011\001\
\012\001\035\001\009\001\010\001\011\001\012\001\013\001\014\001\
\015\001\016\001\017\001\018\001\024\001\025\001\255\255\022\001\
\255\255\024\001\025\001\026\001\027\001\255\255\255\255\035\001\
\009\001\255\255\011\001\012\001\035\001\009\001\010\001\011\001\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\024\001\
\025\001\255\255\022\001\255\255\024\001\025\001\026\001\027\001\
\255\255\255\255\035\001\009\001\255\255\011\001\012\001\035\001\
\009\001\010\001\011\001\012\001\013\001\014\001\015\001\016\001\
\017\001\018\001\024\001\025\001\255\255\022\001\255\255\024\001\
\025\001\026\001\027\001\255\255\255\255\035\001\255\255\255\255\
\255\255\255\255\035\001\009\001\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\255\255\255\255\255\255\
\022\001\255\255\024\001\025\001\026\001\027\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\035\001\009\001\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
\255\255\255\255\255\255\022\001\255\255\024\001\025\001\026\001\
\027\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\035\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\255\255\255\255\255\255\022\001\255\255\
\024\001\025\001\026\001\027\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\035\001\009\001\010\001\011\001\012\001\
\013\001\014\001\015\001\016\001\017\001\018\001\255\255\255\255\
\255\255\022\001\255\255\024\001\025\001\026\001\027\001\009\001\
\010\001\011\001\012\001\255\255\255\255\255\255\035\001\255\255\
\255\255\255\255\255\255\255\255\022\001\255\255\024\001\025\001\
\026\001\027\001\009\001\010\001\011\001\012\001\255\255\255\255\
\255\255\035\001\255\255\009\001\010\001\011\001\012\001\022\001\
\255\255\024\001\025\001\026\001\027\001\255\255\255\255\255\255\
\022\001\255\255\024\001\025\001\035\001\027\001\009\001\010\001\
\011\001\012\001\255\255\255\255\255\255\035\001\009\001\010\001\
\011\001\012\001\255\255\022\001\255\255\024\001\025\001\255\255\
\027\001\255\255\255\255\022\001\255\255\024\001\025\001\255\255\
\035\001\009\001\010\001\011\001\012\001\255\255\255\255\009\001\
\035\001\011\001\012\001\255\255\255\255\255\255\022\001\255\255\
\024\001\025\001\255\255\255\255\022\001\255\255\024\001\025\001\
\255\255\255\255\009\001\035\001\011\001\012\001\255\255\255\255\
\255\255\035\001\255\255\255\255\255\255\255\255\255\255\022\001\
\255\255\024\001\025\001\255\255\011\000\012\000\013\000\014\000\
\015\000\016\000\017\000\255\255\035\001\020\000"

let yynames_const = "\
  "

let yynames_block = "\
  UNIT\000\
  TRUE\000\
  FALSE\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  LPAREN\000\
  RPAREN\000\
  COMMA\000\
  ARROW\000\
  SEMICOLON\000\
  EQ\000\
  NE\000\
  LT\000\
  LE\000\
  GT\000\
  GE\000\
  FUN\000\
  LET\000\
  REC\000\
  IN\000\
  IF\000\
  THEN\000\
  ELSE\000\
  AND\000\
  OR\000\
  NOT\000\
  EXP\000\
  LOG\000\
  SQRT\000\
  HD\000\
  TL\000\
  LEN\000\
  EOI\000\
  INT\000\
  FLOAT\000\
  OP\000\
  VAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exp_seq) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Mini_ml_types.sref) in
    Obj.repr(
# 27 "mini_ml_parser.mly"
               ( List.rev (_1) )
# 436 "mini_ml_parser.ml"
               : Mini_ml_types.expression list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp_seq) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Mini_ml_types.sref) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 30 "mini_ml_parser.mly"
                         ( _3::_1 )
# 445 "mini_ml_parser.ml"
               : 'exp_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 31 "mini_ml_parser.mly"
       ( [_1] )
# 452 "mini_ml_parser.ml"
               : 'exp_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : Mini_ml_types.sref) in
    let _2 = (Parsing.peek_val __caml_parser_env 6 : Mini_ml_types.sref) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'id) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'atom) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : Mini_ml_types.sref) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : Mini_ml_types.sref) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 34 "mini_ml_parser.mly"
                                 ( Mini_ml_types.E_let_rec_in(_3, Mini_ml_types.E_fun (_4, _6, _1), _8, _1) )
# 466 "mini_ml_parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Mini_ml_types.sref) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Mini_ml_types.sref) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'id) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'atom) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Mini_ml_types.sref) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 35 "mini_ml_parser.mly"
                          ( Mini_ml_types.E_let_rec (_3, Mini_ml_types.E_fun (_4, _6, _1), _1) )
# 478 "mini_ml_parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : Mini_ml_types.sref) in
    let _2 = (Parsing.peek_val __caml_parser_env 5 : Mini_ml_types.sref) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'id) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : Mini_ml_types.sref) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : Mini_ml_types.sref) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 36 "mini_ml_parser.mly"
                            ( Mini_ml_types.E_let_rec_in (_3, _5, _7, _1) )
# 491 "mini_ml_parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : Mini_ml_types.sref) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Mini_ml_types.sref) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'id) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Mini_ml_types.sref) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 37 "mini_ml_parser.mly"
                     ( Mini_ml_types.E_let_rec (_3, _5, _1) )
# 502 "mini_ml_parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : Mini_ml_types.sref) in
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'id) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'atom) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : Mini_ml_types.sref) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : Mini_ml_types.sref) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 38 "mini_ml_parser.mly"
                             ( Mini_ml_types.E_let_in (_2, Mini_ml_types.E_fun (_3, _5, _1), _7, _1) )
# 515 "mini_ml_parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : Mini_ml_types.sref) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'id) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'atom) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Mini_ml_types.sref) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 39 "mini_ml_parser.mly"
                       ( Mini_ml_types.E_let (_2, Mini_ml_types.E_fun (_3, _5, _1), _1) )
# 526 "mini_ml_parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Mini_ml_types.sref) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'atom) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Mini_ml_types.sref) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Mini_ml_types.sref) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 40 "mini_ml_parser.mly"
                          ( Mini_ml_types.E_let_in (_2, _4, _6, _1) )
# 538 "mini_ml_parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Mini_ml_types.sref) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'atom) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Mini_ml_types.sref) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 41 "mini_ml_parser.mly"
                    ( Mini_ml_types.E_let (_2, _4, _1) )
# 548 "mini_ml_parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Mini_ml_types.sref) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Mini_ml_types.sref) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Mini_ml_types.sref) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 42 "mini_ml_parser.mly"
                            ( Mini_ml_types.E_if (_2, _4, _6, _1) )
# 560 "mini_ml_parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Mini_ml_types.sref) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Mini_ml_types.sref) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 43 "mini_ml_parser.mly"
                     ( Mini_ml_types.E_fun (_2, _4, _1) )
# 570 "mini_ml_parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp2) in
    Obj.repr(
# 44 "mini_ml_parser.mly"
        ( _1 )
# 577 "mini_ml_parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tup) in
    Obj.repr(
# 48 "mini_ml_parser.mly"
    (
        match _1 with
        | ([e], false) -> e (*Collapse to scalar*)
        | (arg, _) ->
          let l = List.rev arg in
          match l with
          | (hd::tl) -> Mini_ml_types.E_tuple (l, Mini_ml_types.sref_of_expression (List.hd l))
          | _ -> assert false (*Never reach here*)
    )
# 592 "mini_ml_parser.ml"
               : 'exp2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exp3) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Mini_ml_types.sref) in
    Obj.repr(
# 60 "mini_ml_parser.mly"
                ( (fst _1, true) )
# 600 "mini_ml_parser.ml"
               : 'tup))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp3) in
    Obj.repr(
# 61 "mini_ml_parser.mly"
         ( _1 )
# 607 "mini_ml_parser.ml"
               : 'tup))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp3) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Mini_ml_types.sref) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp4) in
    Obj.repr(
# 64 "mini_ml_parser.mly"
                    ( (_3 :: (fst _1), true) )
# 616 "mini_ml_parser.ml"
               : 'exp3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp4) in
    Obj.repr(
# 65 "mini_ml_parser.mly"
         ( ([_1], false) )
# 623 "mini_ml_parser.ml"
               : 'exp3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp4) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Mini_ml_types.sref) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp5) in
    Obj.repr(
# 69 "mini_ml_parser.mly"
                 ( mkbinop "or" _1 _3 _2 )
# 632 "mini_ml_parser.ml"
               : 'exp4))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp5) in
    Obj.repr(
# 70 "mini_ml_parser.mly"
         ( _1 )
# 639 "mini_ml_parser.ml"
               : 'exp4))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp5) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Mini_ml_types.sref) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp6) in
    Obj.repr(
# 74 "mini_ml_parser.mly"
                  ( mkbinop "and" _1 _3 _2 )
# 648 "mini_ml_parser.ml"
               : 'exp5))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp6) in
    Obj.repr(
# 75 "mini_ml_parser.mly"
         ( _1 )
# 655 "mini_ml_parser.ml"
               : 'exp5))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp6) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Mini_ml_types.sref) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp7) in
    Obj.repr(
# 79 "mini_ml_parser.mly"
                 ( mkbinop "="  _1 _3 _2 )
# 664 "mini_ml_parser.ml"
               : 'exp6))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp6) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Mini_ml_types.sref) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp7) in
    Obj.repr(
# 80 "mini_ml_parser.mly"
                 ( mkbinop "<>" _1 _3 _2 )
# 673 "mini_ml_parser.ml"
               : 'exp6))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp6) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Mini_ml_types.sref) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp7) in
    Obj.repr(
# 81 "mini_ml_parser.mly"
                 ( mkbinop "<"  _1 _3 _2 )
# 682 "mini_ml_parser.ml"
               : 'exp6))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp6) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Mini_ml_types.sref) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp7) in
    Obj.repr(
# 82 "mini_ml_parser.mly"
                 ( mkbinop "<=" _1 _3 _2 )
# 691 "mini_ml_parser.ml"
               : 'exp6))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp6) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Mini_ml_types.sref) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp7) in
    Obj.repr(
# 83 "mini_ml_parser.mly"
                 ( mkbinop ">"  _1 _3 _2 )
# 700 "mini_ml_parser.ml"
               : 'exp6))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp6) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Mini_ml_types.sref) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp7) in
    Obj.repr(
# 84 "mini_ml_parser.mly"
                 ( mkbinop ">=" _1 _3 _2 )
# 709 "mini_ml_parser.ml"
               : 'exp6))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp7) in
    Obj.repr(
# 85 "mini_ml_parser.mly"
         ( _1 )
# 716 "mini_ml_parser.ml"
               : 'exp6))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Mini_ml_types.sref) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exp7) in
    Obj.repr(
# 89 "mini_ml_parser.mly"
                            ( mkop ("-", _1) _2 )
# 724 "mini_ml_parser.ml"
               : 'exp7))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp7) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Mini_ml_types.sref) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp8) in
    Obj.repr(
# 90 "mini_ml_parser.mly"
                    ( mkbinop "+" _1 _3 _2 )
# 733 "mini_ml_parser.ml"
               : 'exp7))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp7) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Mini_ml_types.sref) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp8) in
    Obj.repr(
# 91 "mini_ml_parser.mly"
                    ( mkbinop "-" _1 _3 _2 )
# 742 "mini_ml_parser.ml"
               : 'exp7))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp8) in
    Obj.repr(
# 92 "mini_ml_parser.mly"
         ( _1 )
# 749 "mini_ml_parser.ml"
               : 'exp7))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp8) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Mini_ml_types.sref) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp9) in
    Obj.repr(
# 96 "mini_ml_parser.mly"
                    ( mkbinop "*" _1 _3 _2 )
# 758 "mini_ml_parser.ml"
               : 'exp8))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp8) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Mini_ml_types.sref) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp9) in
    Obj.repr(
# 97 "mini_ml_parser.mly"
                     ( mkbinop "/" _1 _3 _2 )
# 767 "mini_ml_parser.ml"
               : 'exp8))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp9) in
    Obj.repr(
# 98 "mini_ml_parser.mly"
         ( _1 )
# 774 "mini_ml_parser.ml"
               : 'exp8))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Mini_ml_types.sref) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exp10) in
    Obj.repr(
# 102 "mini_ml_parser.mly"
              ( mkop ("not", _1) _2 )
# 782 "mini_ml_parser.ml"
               : 'exp9))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Mini_ml_types.sref) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exp10) in
    Obj.repr(
# 103 "mini_ml_parser.mly"
               ( mkop ("sqrt", _1) _2 )
# 790 "mini_ml_parser.ml"
               : 'exp9))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Mini_ml_types.sref) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exp10) in
    Obj.repr(
# 104 "mini_ml_parser.mly"
              ( mkop ("exp", _1) _2 )
# 798 "mini_ml_parser.ml"
               : 'exp9))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Mini_ml_types.sref) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exp10) in
    Obj.repr(
# 105 "mini_ml_parser.mly"
              ( mkop ("log", _1) _2 )
# 806 "mini_ml_parser.ml"
               : 'exp9))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Mini_ml_types.sref) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exp10) in
    Obj.repr(
# 106 "mini_ml_parser.mly"
              ( mkop ("len", _1) _2 )
# 814 "mini_ml_parser.ml"
               : 'exp9))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Mini_ml_types.sref) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exp10) in
    Obj.repr(
# 107 "mini_ml_parser.mly"
             ( mkop ("hd", _1) _2 )
# 822 "mini_ml_parser.ml"
               : 'exp9))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Mini_ml_types.sref) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exp10) in
    Obj.repr(
# 108 "mini_ml_parser.mly"
             ( mkop ("tl", _1) _2 )
# 830 "mini_ml_parser.ml"
               : 'exp9))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string*Mini_ml_types.sref) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exp10) in
    Obj.repr(
# 109 "mini_ml_parser.mly"
             ( mkop _1 _2 )
# 838 "mini_ml_parser.ml"
               : 'exp9))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp10) in
    Obj.repr(
# 110 "mini_ml_parser.mly"
          ( _1 )
# 845 "mini_ml_parser.ml"
               : 'exp9))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exp10) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exp11) in
    Obj.repr(
# 114 "mini_ml_parser.mly"
                ( Mini_ml_types.E_apply (_1, _2, (Mini_ml_types.sref_of_expression _1) ) )
# 853 "mini_ml_parser.ml"
               : 'exp10))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp11) in
    Obj.repr(
# 115 "mini_ml_parser.mly"
          ( _1 )
# 860 "mini_ml_parser.ml"
               : 'exp10))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atom_more) in
    Obj.repr(
# 120 "mini_ml_parser.mly"
    (
      match _1 with
      | [e] -> e
      | _ ->
        let l = List.rev _1 in
        match l with
        | (hd::tl) -> Mini_ml_types.E_tuple (l, Mini_ml_types.sref_of_expression (List.hd l))
        | _ -> assert false (*Never reach here*)
    )
# 875 "mini_ml_parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atom_more) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Mini_ml_types.sref) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp11) in
    Obj.repr(
# 131 "mini_ml_parser.mly"
                          ( _3::_1 )
# 884 "mini_ml_parser.ml"
               : 'atom_more))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp11) in
    Obj.repr(
# 132 "mini_ml_parser.mly"
          ( [_1] )
# 891 "mini_ml_parser.ml"
               : 'atom_more))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp12) in
    Obj.repr(
# 135 "mini_ml_parser.mly"
          ( _1 )
# 898 "mini_ml_parser.ml"
               : 'exp11))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Mini_ml_types.sref) in
    Obj.repr(
# 139 "mini_ml_parser.mly"
         ( Mini_ml_types.E_unit _1 )
# 905 "mini_ml_parser.ml"
               : 'exp12))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int*Mini_ml_types.sref) in
    Obj.repr(
# 140 "mini_ml_parser.mly"
        ( let i, p = _1 in Mini_ml_types.E_number (Mini_ml_types.E_int (i, p), p) )
# 912 "mini_ml_parser.ml"
               : 'exp12))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float*Mini_ml_types.sref) in
    Obj.repr(
# 141 "mini_ml_parser.mly"
          ( let f, p = _1 in Mini_ml_types.E_number (Mini_ml_types.E_float (f, p), p) )
# 919 "mini_ml_parser.ml"
               : 'exp12))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Mini_ml_types.sref) in
    Obj.repr(
# 142 "mini_ml_parser.mly"
         ( Mini_ml_types.E_bool (true, _1) )
# 926 "mini_ml_parser.ml"
               : 'exp12))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Mini_ml_types.sref) in
    Obj.repr(
# 142 "mini_ml_parser.mly"
                                                     ( Mini_ml_types.E_bool (false, _1) )
# 933 "mini_ml_parser.ml"
               : 'exp12))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'id) in
    Obj.repr(
# 143 "mini_ml_parser.mly"
       ( _1 )
# 940 "mini_ml_parser.ml"
               : 'exp12))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Mini_ml_types.sref) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Mini_ml_types.sref) in
    Obj.repr(
# 144 "mini_ml_parser.mly"
                      ( _2 )
# 949 "mini_ml_parser.ml"
               : 'exp12))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string*Mini_ml_types.sref) in
    Obj.repr(
# 147 "mini_ml_parser.mly"
        ( Mini_ml_types.E_var (_1) )
# 956 "mini_ml_parser.ml"
               : 'id))
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Mini_ml_types.expression list)
;;
# 151 "mini_ml_parser.mly"
(*Trailer*)
# 983 "mini_ml_parser.ml"
