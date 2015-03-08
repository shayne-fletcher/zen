(*lexer.mll

  Lexical analyzer returns one of the tokens:

    - the token T_NUM of a float;
    - operators (T_PLUS, T_MINUS, T_MULTIPLY, T_DIVIDE, T_CARET, T_UMINUS);
    - newline (T_NEWLINE)

  It skips all blanks and tabs, unknown characters and raises
  End_of_file on EOF.
*)
{
  open Rpcalc (*Parser file is rpcalc.mly*)
}
let digit = ['0'- '9']
rule token = parse
  | [' ' '\t'] { token lexbuf }
  | '\n'       { T_NEWLINE }
  | digit+
  | "." digit+
  | digit+ "." digit* as num
               { T_NUM (float_of_string num) }
  | '+'        { T_PLUS }
  | '-'        { T_MINUS }
  | '*'        { T_MULTIPLY }
  | '/'        { T_DIVIDE }
  | '^'        { T_CARET }
  | 'n'        { T_UMINUS }
  | _          { token lexbuf }
  | eof        { raise End_of_file }
