(**Some foundational types participating in AST values*)

type constant =
  | Const_int of int
  | Const_char of char
  | Const_string of string * string option
  | Const_float of string
  | Const_int32 of int32 (*1l*)
  | Const_int64 of int64 (*1L*)
  | Const_nativeint of nativeint (*1n*)

(**A value of this type indicates whether an expression is recursive
   or not*)
type rec_flag = Nonrecursive | Recursive

(**A value of type ['a loc] associates an ['a] with its location in
   the source code*)
type 'a loc = 'a Ml_location.loc = {
  txt : 'a;  (**The ['a] value*)
  loc : Ml_location.t; (**The location of the ['a]*)
}

