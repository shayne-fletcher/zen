(*Parser combinators*)

(*The type of the result of a parse*)
type ('a, 'b) parse_result =
| Prreturns of 'b * 'a list
| Prparse_failed

let accept : ('a, 'b) parse_result -> 'b = function
  | Prreturns (v, []) -> v  
  | Prreturns (_, _ ::_) -> failwith "Couldn't consume all the input"
  | Prparse_failed -> failwith "Failed"

(*Parsers are functions*)
type ('a, 'b) parser = 'a list -> ('a, 'b) parse_result

(*[empty v b] recognizes the empty string. It always returns a value
  and never consumes any tokens*)
let empty (v : 'b) (toks : 'a list) : ('a, 'b) parse_result =
  Prreturns (v, toks)

(*[token test l] computes a parser. The parser succeeds or not by
  testing the predicate [test] against the head of the token list*)
let token (test : 'a -> 'b option) : ('a, 'b) parser =
  fun (l : 'a list) : ('a, 'b) parse_result ->
    match l with
    | t :: ts -> begin
      match test t with 
      | Some r -> Prreturns (r, ts)
      | None -> Prparse_failed
    end
    | [] -> Prparse_failed

(*[char_ ch] computes a parser that matches a specific token*)
let char_ (ch : 'a) : ('a, 'a) parser = 
  token (fun tok -> if tok = ch then Some ch else None)

(*[disjunction (p, q)] is the disjunction or two parsers : "p or else
  q"*)
let disjunction 
    ((p : ('a, 'b) parser), (q : ('a, 'b) parser)) : ('a, 'b) parser =
  fun (toks : 'a list) : ('a, 'b) parse_result ->
    match p toks with
    | Prparse_failed -> q toks
    | res -> res

(*[conjunction (p, q)] is the conjunction of two parsers : "p and then q"*)
let conjunction
    ((p : ('a, 'b) parser), 
     (q : ('a, 'c) parser)) : ('a, ('b * 'c)) parser =
  fun (toks : 'a list) : ('a, ('b * 'c)) parse_result ->
    match p toks with
    | Prreturns (r1, toks') -> begin
      match q toks' with
      | Prreturns (r2, toks'') -> 
        Prreturns ((r1, r2), toks'')
      | _ -> Prparse_failed
    end
    | _ -> Prparse_failed

(*[give (p, f)] parses with [p] and gives the result to [f]*)
let gives ((p : ('a, 'b) parser), (f : 'b -> 'c)) : ('a, 'c) parser =
  fun (toks : 'a list) : ('a, 'c) parse_result ->
    match p toks with
    | Prreturns (r, toks') -> Prreturns (f r, toks')
    | _ -> Prparse_failed

(*[given_to p f] passes the result of parsing with [p] to the function
  [f] which computes a new parser from the result*)
let given_to 
    ((p1 : ('a, 'b) parser), 
     (p2 : 'b -> ('a, 'c) parser)) : ('a, 'c) parser = 
  fun (toks : 'a list) : ('a, 'c) parse_result ->
    match p1 toks with
    | Prreturns (r1, toks') -> p2 r1 toks'
    | Prparse_failed -> Prparse_failed

(*Syntax*)
let ( |~ ) p q = disjunction (p, q)
let ( &~ ) p q = conjunction (p, q)
let ( >~ ) p f = gives (p, f)
let ( >>~ ) p f = given_to (p, f)

(*Utilities for brevity in what follows*)
let cons ((c : 'a), (cs : 'a list)) : 'a list = c :: cs
let join ((l : 'a list), (m : 'a list)) : 'a list = l @ m

(*[zero_or_more p] uses [p] to produce a list of results*)
let rec zero_or_more (p : ('a, 'b) parser) : ('a, 'b list) parser =
  fun (toks : 'a list) : ('a, 'b list) parse_result ->
    (p &~ zero_or_more p >~ cons |~ empty []) toks

(*Lexical analysis*)

(*[explode s] turns the string [s] into a list of chars*)
let explode : string -> char list =
  fun s ->
    let n = String.length s in
    let rec loop acc i =
      if i = n then List.rev acc
      else loop (String.get s i :: acc) (i + 1) in
    loop [] 0

(*[implode l] turns the list of chars [l] into a string*)
let implode : char list -> string =
  fun l ->
    let n = List.length l in
    let buf = Bytes.create n in
    let f i c = Bytes.set buf i c in
    List.iteri f l ; Bytes.to_string buf

(*[char_range c l] tests if [c] is in one of the ranges in [l]
  e.g. [char c [('A', 'Z'); ('a', 'z')]]*)
let rec char_range (c : char) (l : (char * char) list) : bool =
  match l with
  | [] -> false
  | (c1, c2) :: cs ->
    (Char.code c1 <= Char.code c && Char.code c <= Char.code c2) || 
      char_range c cs
    
(*[letter] is a parser of alphabetic characters*)
let letter : (char, char) parser =
  token (
    fun (c : char) : char option ->
      if char_range c [('A', 'Z'); ('a', 'z')] then Some c else None
  )

(*[digit] is a parser of digit characters*)
let digit : (char, char) parser =
  token (
    fun (c : char) : char option ->
      if char_range c [('0', '9')] then Some c else None
  )

(*[digits] is a parser of a sequence of digits*)
let digits : (char, char list) parser = digit &~ zero_or_more digit >~ cons

(*optsign := '-'|'+'|epsilon *)
let optsign : (char, char list) parser =
  token (
    fun (c : char) : char list option ->
      match c with
      | c when (c = '-' || c = '+') -> Some [c]
      | _ -> None
  ) |~ empty []

(*optfrac := ('.' *digit)|epsilon *)
let optfrac : (char, char list) parser = 
  char_ '.' &~ zero_or_more digit >~ cons |~ empty []

(*optexp := (('e'|'E') optsign digits)|epsilon *)
let optexp : (char, char list) parser =
  char_ 'e' |~ char_ 'E' &~ optsign >~ cons &~ digits >~ join |~ empty []

(*number := digits optfrac optexp*)
let number : (char, float) parser = 
  digits &~ optfrac &~ optexp >~  
    fun ((csi , csf), cse) -> 
      implode @@ csi @ csf @ cse |> float_of_string

(*Test*)
let _ =  accept (number (explode "123456"))
let _ =  accept (number (explode "123.456e-03"))
let _ =  accept (number (explode "123.456e+03"))


