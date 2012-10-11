/*paser.mly

  ocamlyacc poly_var parser for use with ocamllex poly_var lexer (lexer.mll)
*/

%token <int> INT
%token <float> REAL
%token <string> STRING
%token NIL W NUM STR ARRAY LPAREN RPAREN COMMA LSB RSB SEMICOLON

%start input
%type <Poly_var.poly_var> input
%%
input: 
 | exp                                                                    { $1 }
 ;
exp:
    NIL                                                         { Poly_var.Nil }
  | W INT                                                      { Poly_var.W $2 }
  | NUM REAL                                                 { Poly_var.Num $2 }
  | STR STRING                                               { Poly_var.Str $2 }
  | ARRAY LPAREN 
      LSB exp list_more RSB 
      COMMA INT COMMA INT RPAREN          { Poly_var.Array ($4 :: $5, $8, $10) }
 ;
list_more:
    /*Nothing*/                                                           { [] }
  | SEMICOLON exp list_more                                         { $2 :: $3 }
;
