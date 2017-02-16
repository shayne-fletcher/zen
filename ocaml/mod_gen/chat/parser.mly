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

%start parse_message
%type <Ast.ast> parse_message
%%

parse_message:
 | body T_eof                                             { $1 }
 ;
body:
 | T_nick user                                   { Ast_nick $2 }
 | T_join user channel                     { Ast_join ($2, $3) }
 | T_privmsg user channel message { Ast_privmsg (($2, $3), $4) }
 ;

message:
 | T_txt                                                  { $1 }
 ;
channel:
 | T_hash ident                                           { $2 }
 ;
user:
 | ident                                                  { $1 }
 ;
ident:
 | T_ident                                                { $1 }
 ;
