{
open Parser

let incr_loc lexbuf delta =
  let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
    Lexing.pos_bol = pos.Lexing.pos_cnum - delta;
  }

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

}

let special_chars = 
  ['|' '*' '?' '+' '(' ')']

let backslash_escapes =
  ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']

rule main = parse
    [' ' '\013' '\009' '\012' ] +
    { main lexbuf }
  | '\010'
    { incr_loc lexbuf 0;
      main lexbuf }
  | [^ '\\' '|' '*' '?' '+' '(' ')']
    { Tchar(Char.code (Lexing.lexeme_char lexbuf 0)) }
  | '\\' backslash_escapes
    { Tchar (Char.code (char_for_backslash (Lexing.lexeme_char lexbuf 1))) }
  | '\\' special_chars
    { Tchar (Char.code (Lexing.lexeme_char lexbuf 1)) }
  | '|'  { Tor }
  | '*'  { Tstar }
  | '?'  { Tmaybe }
  | '+'  { Tplus }
  | '('  { Tlparen }
  | ')'  { Trparen }
  | eof  { Tend }
