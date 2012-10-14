/*expr2_parser.mly */
%{
  (*Header*)
%}
/*Declarations*/
%token <int> INT
%token <string> VAR
%token <float> FLOAT
%token PLUS MINUS TIMES DIV MAX MIN
%token LPAREN RPAREN COMMA
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
   INT                                                     { Expr2.IntConst $1 }
 | FLOAT                                                 { Expr2.FloatConst $1 }
 | VAR                                                          { Expr2.Var $1 }
 | arithmetic_expr                                                        { $1 }
 | LPAREN expr RPAREN                                                     { $2 }
 | MINUS expr %prec UMINUS                              { Expr2.UnOp ("-", $2) }
 ;
arithmetic_expr:
 | expr PLUS expr                                  { Expr2.BinOp ("+", $1, $3) }
 | expr MINUS expr                                 { Expr2.BinOp ("-", $1, $3) }
 | expr TIMES expr                                 { Expr2.BinOp ("*", $1, $3) }
 | expr DIV expr                                   { Expr2.BinOp ("/", $1, $3) }
 | MAX LPAREN expr COMMA expr RPAREN             { Expr2.BinOp ("max", $3, $5) }          
 | MIN LPAREN expr COMMA expr RPAREN             { Expr2.BinOp ("min", $3, $5) }          
 ;
%%
(*Trailer*)
