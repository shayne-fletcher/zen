type 'a remaining =
| Remains of 'a list
| Recognition_fails

type 'a recognizer = 'a list -> 'a remaining

let epsilon toks = Remains toks

let end_of_input = function
  | [] -> Remains []
  | _ -> Recognition_fails

let recognizer_of_token test = 
  fun toks -> 
    match toks with
  | (t :: ts) -> if test t then Remains ts else Recognition_fails
  | _ -> Recognition_fails

let recognizer_of_char c = recognizer_of_token (fun c' -> c = c')

let compose_or r1 r2 = fun toks ->
    match r1 toks with
    | (Remains _) as res -> res
    | _ -> r2 toks

let compose_and r1 r2 = fun toks ->
  match r1 toks with
  | Remains toks1 -> r2 toks1
  | _ -> Recognition_fails

let compose_or_list r rl = List.fold_right compose_or rl r

let compose_and_list rl = List.fold_left compose_and epsilon rl

let rec zero_or_more r = compose_or (compose_and r (zero_or_more r)) epsilon

let rec one_or_more r = compose_and r (zero_or_more r)

let rec char_range c = function
  | [] -> false
  | ((c1, c2)::l) ->
    (int_of_char c1 <= int_of_char c && int_of_char c <= int_of_char c2) ||
      char_range c l

let isdigit c = char_range c [('0', '9')]

let isalpha c = char_range c [('a', 'z'); ('A', 'Z')]

let isalnum c = isalpha c || isdigit c

let isblank c = 
  match (compose_or (recognizer_of_char ' ') (recognizer_of_char '\t')) [c] with
  | Remains [] -> true
  | _ -> false

let iscntrl c =
  char_range c [('\x00', '\x1f')] || 
    (match (recognizer_of_char '\x7f') [c]  with
    | Remains [] -> true
    | _ -> false)

let isprint c = not (iscntrl c)

let isgraph c = not (
  match (recognizer_of_char ' ') [c] with 
  |  Remains [] -> true
  | _ -> false) && isprint c

let islower c = char_range c ['a', 'z']

let isupper c = char_range c ['A', 'Z']

let isspace c =
  let spc:char recognizer =recognizer_of_char '\x20' (*space SPC*)in
  let spcs:(char recognizer list)=
      [recognizer_of_char '\x09'; (*horizontal tab TAB ('\t')*)
       recognizer_of_char '\x0a'; (*newline LF ('\n')*)
       recognizer_of_char '\x0b'; (*vertical tab VT ('\v')*)
       recognizer_of_char '\x0c';  (*form feed FF ('\f')*)
       recognizer_of_char '\x0d'] (*carriage return CR ('\r')*) in
  let skip:char recognizer = compose_or_list spc spcs in
  match skip [c] with
  | Remains [] -> true
  | _ -> false

let isxdigit c = char_range c [('0', '9'); ('a', 'f'); ('A', 'F')]
