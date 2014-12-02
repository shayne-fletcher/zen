(**Recognition*)

(**A mini-combinator library of 'recognizers'*)
module type S = sig

  (**Result of a 'recognizer'. Indicates the result of attempting to
     recognize something from an ['a list]*)
  type 'a remaining =
  | Remains of 'a list (**Recognition succeeded*)
  (**The ['a list] is the input remaining*)
  | Recognition_fails

  (**An [('a recognizer] is a function from an ['a list] to a value of
     ['a remaining]*)
  type 'a recognizer = 'a list -> 'a remaining

  (**A recognizer that recognizes the empty string. It always succeeds
     and no input is ever consumed*)
  val epsilon : 'a recognizer

  (**Given a predicate, 'token' produces the recognizer associated
     with the elements that satisfy this predicate*)
  val recognizer_of_token : ('a -> bool) -> 'a recognizer

  (**A recognizer constructor function that recognizes only it's
     argument*)
  val recognizer_of_char : 'a -> 'a recognizer

  (**Recognizer disjunction*)
  val compose_or : 'a recognizer -> 'a recognizer -> 'a recognizer

  (**Recognizer conjunction*)
  val compose_and : 'a recognizer -> 'a recognizer -> 'a recognizer

  (**Kleene star recognizer*)
  val zero_or_more : 'a recognizer -> 'a recognizer

end
