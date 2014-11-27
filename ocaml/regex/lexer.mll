{
open Parser

let incr_loc lexbuf delta =
  let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
    Lexing.pos_bol = pos.Lexing.pos_cnum - delta;
  }
}

rule main = parse
    [' ' '\013' '\009' '\012' ] +
    { main lexbuf }
  | '\010'
    { incr_loc lexbuf 0;
      main lexbuf }
  | [^ '\\' '|' '*' '?' '+' '(' ')']
    { Tchar(Char.code(Lexing.lexeme_char lexbuf 0)) }
  | '\\' ['\\' '|' '*' '?' '+' '(' ')']
    { Tchar (Char.code(Lexing.lexeme_char lexbuf 1))) }
  | '|'  { Tor }
  | '*'  { Tstar }
  | '?'  { Tmaybe }
  | '+'  { Tplus }
  | '('  { Tlparen }
  | ')'  { Trparen }
  | eof  { Tend }
