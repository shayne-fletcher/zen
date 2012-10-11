/*expr2_parser.mly */
%{
  (*Header*)
%}
/*Declarations*/
%token <int> INT
%token <string> VAR
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOI              /*End of input.*/
%left PLUS MINUS        /*Lowest precedence.*/
%left TIMES DIV         /*Medium precedence.*/
%nonassoc UMINUS        /*Highest precedence.*/
%start main             /*The entry point.*/
%type <Expr2.expr2> main
%%
/*Rules*/
main:
  expr EOI                                                                { $1 }
 ;
expr:
   INT                                                        { Expr2.Const $1 }
 | VAR                                                          { Expr2.Var $1 }
 | arithmetic_expr                                                        { $1 }
 | LPAREN expr RPAREN                                                     { $2 }
 | MINUS expr %prec UMINUS                              { Expr2.UnOp ("-", $2) }
 ;
arithmetic_expr:
   expr PLUS expr                                  { Expr2.BinOp ("+", $1, $3) }
 | expr MINUS expr                                 { Expr2.BinOp ("-", $1, $3) }
 | expr TIMES expr                                 { Expr2.BinOp ("*", $1, $3) }
 | expr DIV expr                                   { Expr2.BinOp ("/", $1, $3) }
 ;
%%
(*Trailer*)
