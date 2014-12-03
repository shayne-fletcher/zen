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

  (**A recognizer that only recognizes  the empty ['a list].*)
  val end_of_input : 'a recognizer

  (**Given a predicate, 'token' produces the recognizer associated
     with the elements that satisfy this predicate*)
  val recognizer_of_token : ('a -> bool) -> 'a recognizer

  (**A recognizer constructor function that recognizes only it's
     argument*)
  val recognizer_of_char : 'a -> 'a recognizer

  (**Kleene star recognizers*)
  val zero_or_more : 'a recognizer -> 'a recognizer
  val one_or_more : 'a recognizer -> 'a recognizer

  (**Recognizer disjunction*)
  val compose_or : 'a recognizer -> 'a recognizer -> 'a recognizer
  val compose_or_list : 'a recognizer -> 'a recognizer list -> 'a recognizer

  (**Recognizer conjunction*)
  val compose_and : 'a recognizer -> 'a recognizer -> 'a recognizer
  val compose_and_list : 'a recognizer list -> 'a recognizer(**Note that the **)
  (**conjunction of an empty list is taken to be the recognizer [epsilon]*)

  (**{2 Character classification functions}*)

  (**A predicate to test whether a character belongs to the union of
     closed intervals of characters*)
  val char_range : char -> (char * char) list -> bool

  (**Check if character is a decimal digit*)
  val isdigit : char -> bool

  (**Check if character is alphabetic*)
  val isalpha : char -> bool

  (**Check if character is alphanumeric*)
  val isalnum : char -> bool

  (**Check if charcter is a blank*)
  val isblank : char -> bool

  (**Check if charcter is a control character*)
  val iscntrl : char -> bool

  (**Check if charcter is a printable character*)
  val isprint : char -> bool

  (**Check if charcter has a graphical representation*)
  val isgraph : char -> bool

  (**Check if charcter is a lower case letter*)
  val islower : char -> bool

  (**Check if charcter is a upper case letter*)
  val isupper : char -> bool

  (**Check if charcter is a white-space*)
  val isspace : char -> bool

  (**Check if charcter is a hexadecimal digit*)
  val isxdigit : char -> bool

end
