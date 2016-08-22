(**Some foundational types participating in AST values*)

(**A value of this type indicates whether an expression is recursive
   or not*)
type rec_flag = Nonrecursive | Recursive

(**The cases of unary operators in this language*)
type unop = 
| Unop_fst  (**The [fst] operator*)
| Unop_snd  (**The [snd] operator*)

(**The cases of binary (infix) operators in this language*)
type binop = 
| Binop_add  (**Addition*)
| Binop_sub  (**Subtraction*)
| Binop_mul  (**Multiplication*)
| Binop_eq   (**Equality comparison*)
| Binop_less (**Lexicographical comparison*)

(**A value of type ['a loc] associates an ['a] with its location in
   the source code*)
type 'a loc = 'a Ml_location.loc = {
  txt : 'a;  (**The ['a] value*)
  loc : Ml_location.t; (**The location of the ['a]*)
}

