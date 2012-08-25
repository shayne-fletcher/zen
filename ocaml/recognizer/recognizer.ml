(* Recognizers 

   Read a grammar directly as a recognizer.
*)


(* Recognizer constructors. *)

type 'a remaining = 
  | Remains of 'a list
  | Fails
  ;;

(* The type of a recognizer is an abbreviation. *)

type 'a recognizer = 'a list -> 'a remaining ;;

(* The recognizer associated with epsilon always succeeds and never
   consumes anything from its list-argument. *)

let empty : 'a recognizer = fun toks -> Remains toks ;;

let token : ('a -> bool) -> 'a recognizer = 
 fun test x ->
  match x with
  | (t :: ts) -> if test t then Remains ts else Fails
  | _ -> Fails
  ;;

let char c : 'a recognizer = token ((=) c) ;;

(* We associate the recognizer p1 orelse p2 with the disjunction of
   the recognizers p1 and p2.*)

let orelse : 'a recognizer -> 'a recognizer -> 'a recognizer  =
  fun p1 p2 toks -> 
    match p1 toks with 
    | Fails -> p2 toks
    | remains -> remains
  ;;

(* 
  #infix "orelse" ;;

  Try to make orelse infix. Technique from
  https://groups.google.com/forum/?fromgroups=#!topic/fa.caml/x597j7t9Mbc 

  "We use the following two infix operators.
  
     let ( /* ) x y = y x
     and ( */ ) x y = x y
  
  Then we can make an infix operator /*f*/ for a binary function f.
  For example, using binary functions 'min' and 'max', we can write
  
     3 /*min*/ 4 + 6 /*max*/ 8
  
  to get 11 as 'min 3 4 + max 5 8'."
*)

let ( /* ) x y = y x  ;;
let ( */ ) x y = x y  ;;

(*

# ((char 'a') /* orelse */ (char 'b')) ['b'];;
- : char remaining = Remains []
# ((char 'a') /* orelse */ empty)['a'];;
- : char remaining = Remains []

  Beware. orelse is not commutative. You must take careful of the order
  of arguments, especially when one of its arguments can recognize a
  stubstring recognizable by the other. As a genearl rule, the
  recognizer capable of recognizing the longest strings should be the
  first argument of orelse.

# (empty /* orelse */ (char 'a'))['a'];;
- : char remaining = Remains ['a']

*)

(* Concatenation is represented by andalso. It tries to apply its
   arguments sequentially and fails when one of them failse. *)

let andalso : 'a recognizer -> 'a recognizer -> 'a recognizer =
  fun p1 p2 toks ->
    match p1 toks with
    | Remains toks' -> p2 toks'
    | _ -> Fails
  ;;

(*

# ((char 'a') /* andalso */ (char 'b'))['a'; 'b'; 'c'];;
- : char remaining = Remains ['c']

*)

(* The '*' iterator is translated by the recursive function
   zero_or_more. *)

let rec zero_or_more p = 
(fun toks -> 
  ((p /* andalso */ (zero_or_more p)) /* orelse */ empty) toks) 
;; (* val zero_or_more : 'a recognizer -> 'a recognizer = <fun> *)

(*

# (zero_or_more (char 'a'))['a';'a';'a';'b'];;
- : char remaining = Remains ['b']
# (zero_or_more (char 'a'))['b'];;
- : char remaining = Remains ['b']

*)

(* Recognizers for regular expressions.*)

let rec char_range c (*: char -> (char * char) list ->bool *) =
  fun x ->
    match x with
      | [] -> false
      | ((c1, c2) :: tl) ->
	  (int_of_char c1 <= int_of_char c && int_of_char c <= int_of_char c2)
	      || char_range c tl
  ;;

let is_digit : char -> bool = fun c -> char_range c [('0', '9')] ;;
let is_alpha : char -> bool = fun c -> char_range c [('a', 'z'); ('A','Z')];;

(* 

   For review, here is the regular expression defining numbers.

   digit    :=    '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9';
   digits   :=    digit digit*;
   optsign  :=    '-'|'+'|eps;
   optfrac  :=    ('.' digit* )|eps;
   optexp   :=    (('e'|'E') optsign digits)|eps;

   To construct the recognizer for numbers we follow the same scheme.

*)

let digit : char recognizer = token is_digit ;;
let digits : char recognizer  = digit /* andalso */(zero_or_more digit) ;;
let optsign : char recognizer = 
  token (fun x -> 
           match x with
           | '-' -> true
           | '+' -> true
           | _ -> false
  )
;;

let optfrac : char recognizer =
  (token ((fun x ->
           match x with
	   | '.' -> true
           | _ -> false)) 
   /* andalso */ (zero_or_more digit)) 
  /*orelse*/ empty
;;

let optexp : char recognizer =
  ((token (fun x ->
           match x with
             | 'e' -> true
             | 'E' -> true
             | _ -> false) 
     /* andalso */ optsign) 
    /* andalso */ digits) 
  /* orelse */ empty
  ;;

(* Finally, the recognizer for numbers itself. *)

let number : char recognizer = digits /* andalso */ optfrac /* andalso */ optexp ;;

(*
  We need 

    explode : string -> char list
    implode : char list -> string

  Found these in http://caml.inria.fr/mantis/view.php?id=5367.
*)

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1)(s.[i] :: l) in
  exp (String.length s - 1) []
;;

let implode l = 
  let res = String.create (List.length l) in
  let rec imp i = 
    fun l ->
      match l with
        | [] -> res
        | (c :: l) -> res.[i] <- c; imp (i + 1) l in imp 0 l
 ;;

(*

# number (explode "123") ;;
- : char remaining = Remains []
# number (explode "123abc") ;;
- : char remaining = Remains ['a'; 'b'; 'c']
# number (explode "123.45e-67");;
- : char remaining = Remains []
# number (explode "abc");;
- : char remaining = Fails

*)

(* Various derived forms. *)

(* "." *)
let any : 'a recognizer =
  fun x -> token (fun _ -> true) x 
;; 

(* "?" *)
let optional : 'a recognizer -> 'a recognizer = 
  (fun p -> (p /* orelse*/ empty)) 
;;

(* "+" *)
let one_or_more : 'a recognizer -> 'a recognizer = 
  (fun p -> (p /* andalso */ (zero_or_more p))) 
;;






