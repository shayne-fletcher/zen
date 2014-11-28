/*parser.mly*/

%{
open Syntax
%}

%token <int> Tchar

%token Tend Tor Tlbracket Trbracket
%token Tstar Tmaybe Tplus Tlparen Trparen

%left Tor
%nonassoc CONCAT
%nonassoc Tmaybe Tstar Tplus
%nonassoc Tchar Tlbracket Tlparen

%start main
%type <Syntax.regular_expression> main
%%

main:
 | regexp Tend
     { $1 }
 ;
regexp:
 | Tchar
     { Character $1 }
 | regexp Tstar
     { Repetition $1 }
 | regexp Tmaybe
     { Alternative (Epsilon, $1) }
 | regexp Tplus
     { Sequence (Repetition ($1), $1)}
 | regexp Tor regexp
     { Alternative ($1, $3) }
 | regexp regexp %prec CONCAT
     { Sequence ($1, $2) }
 | Tlparen regexp Trparen
     { $2 }
 ;
