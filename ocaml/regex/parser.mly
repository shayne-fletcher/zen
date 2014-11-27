/*parser.mly*/

%token <int> Tchar

%token Tend Tor Tlbracket Trbracket
%token Tstar Tmaybe Tplus Tlparen Trparen

%left Tor
%nonassoc CONCAT
%nonassoc Tmaybe Tstar Tplus
%nonassoc Tchar Tlbracket Tlparen

%start main
%type <unit> main
%%

main:
 | regexp Tend
     {}
 ;
regexp:
 | Tchar
     {}
 | regexp Tstar
     {}
 | regexp Tmaybe
     {}
 | regexp Tplus
     {}
 | regexp Tor regexp
     {}
 | regexp regexp %prec CONCAT
     {}
 | Tlparen regexp Trparen
     {}
 ;
