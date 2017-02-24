%{
  open Request_ast

  let mk_connect tok = Ast_connect tok
  let mk_nick (tok, usr) = Ast_nick (tok, usr)
  let mk_join (tok, chan) = Ast_join (tok, chan)
  let mk_poll (tok, chan) = Ast_poll (tok, chan)
  let mk_privmsg (tok, (chan, msg)) = Ast_privmsg ((tok, chan), msg)
%}

/*Tokens*/

%token T_eof
%token T_hash
%token <string> T_ident
%token T_join
%token T_nick
%token T_poll
%token T_privmsg
%token <string> T_txt
%token <string> T_token

%start parse_message
%type <Request_ast.ast> parse_message
%%

parse_message:
 | body T_eof                                                { $1 }
 ;
body:
 | nick                                           { mk_connect $1 }
 | token nick                                  { mk_nick ($1, $2) }
 | token join                                  { mk_join ($1, $2) }
 | token poll                                  { mk_poll ($1, $2) }
 | token privmsg                            { mk_privmsg ($1, $2) }
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
poll:
 | T_poll channel                                            { $2 }
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
