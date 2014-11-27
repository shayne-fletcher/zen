type token =
  | Tchar of (int)
  | Tend
  | Tor
  | Tlbracket
  | Trbracket
  | Tstar
  | Tmaybe
  | Tplus
  | Tlparen
  | Trparen

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
