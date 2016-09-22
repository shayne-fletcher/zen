(**Some foundational types participating in AST values*)

(**A value of this type indicates whether an expression is recursive
   or not*)
type rec_flag = Nonrecursive | Recursive

(**A value of type ['a loc] associates an ['a] with its location in
   the source code*)
type 'a loc = 'a Ml_location.loc = {
  txt : 'a;  (**The ['a] value*)
  loc : Ml_location.t; (**The location of the ['a]*)
}

