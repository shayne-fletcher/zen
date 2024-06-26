module type S = sig

  type sref = Lexing.position*Lexing.position

  exception Unrecognized_token of string
  exception Unclosed_comment

  type number =
  | E_int of (int * sref)
  | E_float of (float * sref)

  type expression =
  | E_unit of sref
  | E_var of (string * sref)
  | E_tuple of (expression list * sref)
  | E_number of (number * sref)
  | E_bool of (bool * sref)
  | E_apply of (expression * expression * sref) (*f x*)
  | E_unop of (string * expression * sref) (*not e, log e, etc.*)
  | E_binop of (string * expression * expression * sref) (*=, <>, +, etc.*)
  | E_let of (expression * expression * sref) (*let x = e*)
  | E_let_in of (expression * expression * expression * sref) (*let x = e1 in e2*)
  | E_let_rec of (expression * expression * sref) (*let rec x = e*)
  | E_let_rec_in of (expression * expression * expression * sref) (*let rec x = e1 in e2*)
  | E_if of (expression * expression * expression * sref) (*if p then t else f*)
  | E_fun of (expression * expression * sref) (*fun x -> e*)

  val sref_of_expression : expression -> sref

end
