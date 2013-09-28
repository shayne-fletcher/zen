/*parser.mly*/

%token <string> STRING
%token LPAREN RPAREN COMMA EOI

%start main
%type <(string, string) Term_types.term> main
%%
main:
 | exp EOI { $1 }
 ;
exp:
 | tag exp_more           
     { 
       match $2 with
       | None -> Term_types.Var $1
       | Some l -> Term_types.Term ($1, l)
     }
 ;
exp_more:
 | /*nothing*/ { None }
 | LPAREN exp_list RPAREN { Some $2 }
;
exp_list:
 | /*nothing*/ { [] }
 | exp exp_list_more { $1::$2 }
 ;
exp_list_more:
 | /*nothing*/ {[]}
 | COMMA exp exp_list_more { $2::$3 }
;
tag:
 | STRING { $1 }
 ;
