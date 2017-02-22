%{
  open Ast
%}

/*Tokens*/

%token T_eof
%token T_hash
%token <string> T_ident
%token T_join
%token T_nick
%token T_privmsg
%token <string> T_txt
%token <string> T_token

%start parse_message
%type <Ast.ast> parse_message
%%

parse_message:
 | body T_eof                                                { $1 }
 ;
body:
 | nick                                         { Ast_connect $1 }
 | token nick                                 { Ast_nick ($1, $2) }
 | token join                                 { Ast_join ($1, $2) }
 | token privmsg 
            { let chan, msg = $2 in Ast_privmsg (($1, chan), msg) }
 ;
nick:
 | T_nick user                                               { $2 }
 ;
join:
 | T_join channel                                            { $2 }
 ;
privmsg:
 | T_privmsg channel message                           { ($2, $3) }
 ;
message:
 | T_txt                                                     { $1 }
 ;
channel:
 | T_hash ident                                              { $2 }
 ;
user:
 | ident                                                     { $1 }
 ;
token:
 | T_token                                                   { $1 }
 ;
ident:
 | T_ident                                                   { $1 }
 ;
