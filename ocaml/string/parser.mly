%token <string> STRING
%start main
%type <string> main
%%
main:
  |  STRING { $1 }
;
