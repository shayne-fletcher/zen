/*rpcalc.mly
 {{:http:http://plus.kaist.ac.kr/~shoh/ocaml/ocamllex-ocamlyacc/ocamlyacc-tutorial/chap-examples.html}Reverse polish notation calculator}
 */

%{
open Printf

let parse_error s = (*Called by the parser funtion on error*)
  (*[s] is the string "syntax error"*)
  flush stdout
  (*We have no error recovery rules so next a [Parsing.Parse_error] will
    be raised*)

%}

%token <float> T_NUM
%token T_PLUS T_MINUS T_MULTIPLY T_DIVIDE T_CARET T_UMINUS
%token T_NEWLINE

%start input
%type <unit> input

%% /*Grammar rules and actions follow*/
input :
  | /*empty*/    {}
  | input line   {}
  ;

line :
  | T_NEWLINE     {}
  | exp T_NEWLINE { printf "\t%.10g\n" $1; flush stdout }
  ;

exp :
  | T_NUM             { $1 }
  | exp exp T_PLUS    { $1 +. $2 }
  | exp exp T_MINUS   { $1 -. $2 }
  | exp exp T_MULTIPLY      { $1 *. $2 }
  | exp exp T_DIVIDE        { $1 /. $2 }
  | exp exp T_CARET   { $1 ** $2 }
  | exp T_UMINUS      { -. $1 }
  ;
%%
