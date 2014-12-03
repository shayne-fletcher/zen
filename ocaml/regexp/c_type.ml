let rec char_range c = function
  | [] -> false
  | ((c1, c2)::l) ->
    (int_of_char c1 <= int_of_char c && int_of_char c <= int_of_char c2) ||
      char_range c l

let isdigit c = char_range c [('0', '9')]

let isalpha c = char_range c [('a', 'z'); ('A', 'Z')]

let isalnum c = isalpha c || isdigit c

let isblank c = 
  match (Recognizer.compose_or (Recognizer.recognizer_of_char ' ') (Recognizer.recognizer_of_char '\t')) [c] with
  | Recognizer.Remains [] -> true
  | _ -> false

let iscntrl c =
  char_range c [('\x00', '\x1f')] || 
    (match (Recognizer.recognizer_of_char '\x7f') [c]  with
    | Recognizer.Remains [] -> true
    | _ -> false)

let isprint c = not (iscntrl c)

let isgraph c = not (
  match (Recognizer.recognizer_of_char ' ') [c] with 
  |  Recognizer.Remains [] -> true
  | _ -> false) && isprint c

let islower c = char_range c ['a', 'z']

let isupper c = char_range c ['A', 'Z']

let isspace c =
  let spc = Recognizer.recognizer_of_char '\x20' (*space SPC*)in
  let spcs =
      [Recognizer.recognizer_of_char '\x09'; (*horizontal tab TAB ('\t')*)
       Recognizer.recognizer_of_char '\x0a'; (*newline LF ('\n')*)
       Recognizer.recognizer_of_char '\x0b'; (*vertical tab VT ('\v')*)
       Recognizer.recognizer_of_char '\x0c';  (*form feed FF ('\f')*)
       Recognizer.recognizer_of_char '\x0d'] (*carriage return CR ('\r')*) in
  let skip = Recognizer.compose_or_list spc spcs in
  match skip [c] with
  | Recognizer.Remains [] -> true
  | _ -> false

let isxdigit c = char_range c [('0', '9'); ('a', 'f'); ('A', 'F')]
