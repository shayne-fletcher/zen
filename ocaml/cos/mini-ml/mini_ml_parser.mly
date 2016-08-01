%{
let mkop (s, loc) operand = Mini_ml_types.E_unop (s, operand, loc)
let mkbinop s left right loc = Mini_ml_types.E_binop (s, left, right, loc)
%}

%token <Mini_ml_types.sref> UNIT TRUE FALSE
%token <Mini_ml_types.sref> PLUS MINUS TIMES DIVIDE
%token <Mini_ml_types.sref> LPAREN RPAREN COMMA ARROW SEMICOLON
%token <Mini_ml_types.sref> EQ NE LT LE GT GE
%token <Mini_ml_types.sref> FUN LET REC IN IF THEN ELSE AND OR NOT
%token <Mini_ml_types.sref> EXP LOG SQRT HD TL LEN
%token <Mini_ml_types.sref> EOI

%token <int*Mini_ml_types.sref> INT
%token <float*Mini_ml_types.sref> FLOAT
%token <string*Mini_ml_types.sref> OP
%token <string*Mini_ml_types.sref> VAR

%left EQ PLUS MINUS
%left TIMES DIVIDE IN
%nonassoc UMINUS

%start main
%type <Mini_ml_types.expression list> main
%%
main:
 | exp_seq EOI { List.rev ($1) }
 ;
exp_seq:
 | exp_seq SEMICOLON exp { $3::$1 }
 | exp { [$1] }
 ;
exp:
 | LET REC id atom EQ exp IN exp { Mini_ml_types.E_let_rec_in($3, Mini_ml_types.E_fun ($4, $6, $1), $8, $1) }
 | LET REC id atom EQ exp { Mini_ml_types.E_let_rec ($3, Mini_ml_types.E_fun ($4, $6, $1), $1) }
 | LET REC id EQ exp IN exp { Mini_ml_types.E_let_rec_in ($3, $5, $7, $1) }
 | LET REC id EQ exp { Mini_ml_types.E_let_rec ($3, $5, $1) }
 | LET id atom EQ exp IN exp { Mini_ml_types.E_let_in ($2, Mini_ml_types.E_fun ($3, $5, $1), $7, $1) }
 | LET id atom EQ exp  { Mini_ml_types.E_let ($2, Mini_ml_types.E_fun ($3, $5, $1), $1) }
 | LET atom EQ exp IN exp { Mini_ml_types.E_let_in ($2, $4, $6, $1) }
 | LET atom EQ exp  { Mini_ml_types.E_let ($2, $4, $1) }
 | IF exp THEN exp ELSE exp { Mini_ml_types.E_if ($2, $4, $6, $1) }
 | FUN exp ARROW exp { Mini_ml_types.E_fun ($2, $4, $1) }
 | exp2 { $1 }
 ;
exp2:
  | tup
    {
        match $1 with
        | ([e], false) -> e (*Collapse to scalar*)
        | (arg, _) ->
          let l = List.rev arg in
          match l with
          | (hd::tl) -> Mini_ml_types.E_tuple (l, Mini_ml_types.sref_of_expression (List.hd l))
          | _ -> assert false (*Never reach here*)
    }
  ;
/* Some trickery to accomodate single element tuples */
tup: 
  | exp3 COMMA  { (fst $1, true) }
  | exp3 { $1 }
  ;
exp3:
  | exp3 COMMA exp4 { ($3 :: (fst $1), true) }
  | exp4 { ([$1], false) }
  ;
/*Parses OR expressions*/
exp4:
  | exp4 OR exp5 { mkbinop "or" $1 $3 $2 }
  | exp5 { $1 }
  ;
/*Parses AND expressions*/
exp5:
  | exp5 AND exp6 { mkbinop "and" $1 $3 $2 }
  | exp6 { $1 }
  ;
/*Parses EQ, NE, LT, LE, GT, GE expressions*/
exp6:
  | exp6 EQ exp7 { mkbinop "="  $1 $3 $2 }
  | exp6 NE exp7 { mkbinop "<>" $1 $3 $2 }
  | exp6 LT exp7 { mkbinop "<"  $1 $3 $2 }
  | exp6 LE exp7 { mkbinop "<=" $1 $3 $2 }
  | exp6 GT exp7 { mkbinop ">"  $1 $3 $2 }
  | exp6 GE exp7 { mkbinop ">=" $1 $3 $2 }
  | exp7 { $1 }
  ;
/*Parses PLUS and MINUS expressions*/
exp7:
  | MINUS exp7 %prec UMINUS { mkop ("-", $1) $2 }
  | exp7 PLUS exp8  { mkbinop "+" $1 $3 $2 }
  | exp7 MINUS exp8 { mkbinop "-" $1 $3 $2 }
  | exp8 { $1 }
  ;
/*Parses TIMES and DIVIDE expressions*/
exp8:
  | exp8 TIMES exp9 { mkbinop "*" $1 $3 $2 }
  | exp8 DIVIDE exp9 { mkbinop "/" $1 $3 $2 }
  | exp9 { $1 }
  ;
/*Parses unary ops like not, exp, sqrt, ...*/
exp9:
  | NOT exp10 { mkop ("not", $1) $2 }
  | SQRT exp10 { mkop ("sqrt", $1) $2 }
  | EXP exp10 { mkop ("exp", $1) $2 }
  | LOG exp10 { mkop ("log", $1) $2 }
  | LEN exp10 { mkop ("len", $1) $2 }
  | HD exp10 { mkop ("hd", $1) $2 }
  | TL exp10 { mkop ("tl", $1) $2 }
  | OP exp10 { mkop $1 $2 } 
  | exp10 { $1 }
  ;
/*Parses function application*/
exp10:
  | exp10 exp11 { Mini_ml_types.E_apply ($1, $2, (Mini_ml_types.sref_of_expression $1) ) }
  | exp11 { $1 }
  ;
/*Parse an atom (may be tuple)*/
atom:
  | atom_more
    {
      match $1 with
      | [e] -> e
      | _ ->
        let l = List.rev $1 in
        match l with
        | (hd::tl) -> Mini_ml_types.E_tuple (l, Mini_ml_types.sref_of_expression (List.hd l))
        | _ -> assert false (*Never reach here*)
    }
  ;
atom_more:
  | atom_more COMMA exp11 { $3::$1 }
  | exp11 { [$1] }
  ;
exp11:
  | exp12 { $1 }
  ;
/*Parses constants, numbers (INT, FLOAT), strings (VAR), and expressions in parentheses*/
exp12:
  | UNIT { Mini_ml_types.E_unit $1 }
  | INT { let i, p = $1 in Mini_ml_types.E_number (Mini_ml_types.E_int (i, p), p) }
  | FLOAT { let f, p = $1 in Mini_ml_types.E_number (Mini_ml_types.E_float (f, p), p) }
  | TRUE { Mini_ml_types.E_bool (true, $1) } | FALSE { Mini_ml_types.E_bool (false, $1) }
  | id { $1 }
  | LPAREN exp RPAREN { $2 }
 ;
id:
  | VAR { Mini_ml_types.E_var ($1) }
  ;

%%
(*Trailer*)
