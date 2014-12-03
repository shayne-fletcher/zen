(**Character classification*)

(**A set of functions to classify individual characters*)
module type S = sig

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
