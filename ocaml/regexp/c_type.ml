let rec char_range c = function
  | [] -> false
  | ((c1, c2)::l) ->
    let i = int_of_char c in 
    (int_of_char c1 <= i && i <= int_of_char c2) || char_range c l

let isdigit c = char_range c [('0', '9')]
let isalpha c = char_range c [('a', 'z'); ('A', 'Z')]
let isalnum c = isalpha c || isdigit c
let isblank c = c = ' ' || c = '\t'
let iscntrl c = char_range c [('\x00', '\x1f')] ||  c = '\x7f'
let isprint c = not (iscntrl c)
let isgraph c = c <> ' ' && isprint c
let islower c = char_range c ['a', 'z']
let isupper c = char_range c ['A', 'Z']
let isspace c =
  c = '\x20' (*space SPC*) ||
  c = '\x09' (*horizontal tab TAB ('\t')*) ||
  c = '\x0a' (*newline LF ('\n')*) ||
  c = '\x0b' (*vertical tab VT ('\v')*) ||
  c = '\x0c'  (*form feed FF ('\f')*) ||
  c = '\x0d' (*carriage return CR ('\r')*)
let isxdigit c = char_range c [('0', '9'); ('a', 'f'); ('A', 'F')]
