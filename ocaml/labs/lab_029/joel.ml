(* Some utilities *)
module Utils = struct

  let const x _ = x

  let string_to_list s =
    let rec aux i l =
      if i < 0 then l else aux (i - 1) (s.[i] :: l) 
    in
    aux (String.length s - 1) []

  let calc_int xs =
    let accum x (n, ix) =
      let n = n +. (float_of_int x) *. (10. ** (float_of_int ix)) in
      (n, ix + 1)
    in
    fst @@ List.fold_right accum xs (0., 0)

  let calc_frac xs =
    let accum (n, ix) x =
      let n = n +. (float_of_int x) *. (10. ** (-1. *. float_of_int ix)) in
      (n, ix + 1)
    in
    fst @@ List.fold_left accum (0., 1) xs

end

module Option = struct
  
  let join = function
    | Some x  -> x
    | None    -> None

  let map f = function
    | Some x  -> Some (f x)
    | None    -> None

  let (>>=) o f = join (map f o)

  let default f = function
    | Some y  -> y
    | None    -> f ()

end

(* Minimal parser implementation exposing an applicative interface *)
module Parser = struct
  open Utils
  open Option

  type 'a t = char list -> ('a * char list) option

  let run p s = map fst @@ p @@ string_to_list s

  (* This parser always fails *)
  let fail = const None

  (* Tries to parse one token *)
  let one f = function
    | c :: cs -> f c >>= fun x -> Some (x, cs)
    | []      -> None

  (* Matches empty input *)
  let empty = function
    | []  -> Some ((), [])
    | _   -> None

  (* Delay parser creation *)
  let delay f cs = f () cs

  (* Returns a constant value and does not consume any input *)
  let pure x cs = Some (x, cs)

  (* Applicative style composition *)
  let ( <*> ) pf p cs = 
    pf cs >>= fun (f, cs) -> p cs >>= fun (x, cs) -> Some (f x, cs)

  (* Tries to execute first parser, if it fails runs the second *)
  let ( <|> ) p1 p2 cs =
    match p1 cs with
    | Some r  -> Some r
    | None    -> p2 cs

  let ( <$> ) f x = pure f <*> x
  let choose xs = List.fold_right (<|>) xs fail

  (* Parses zero or more elements using the given parser *)
  let rec many p =
    choose 
      [
        List.cons <$> p <*> (delay @@ fun _ -> many p);
        pure []
      ]
end


(* Number parser *)
module Numbers = struct
  open Parser
  open Utils

  let token c = one (fun x -> if x = c then Some () else None)

  let digit =
    "0123456789"
    |> string_to_list 
    |> List.mapi (fun i c -> const i <$> token c)
    |> choose
  
  let digits = 
    (fun d ds -> calc_int (d :: ds))
    <$> digit 
    <*> many digit
    
  let optsign = 
    choose 
      [
        const (-1.) <$> token '-';
        const 1.    <$> token '+';
        pure 1.
      ]

  let optfrac = 
    choose 
      [
        (fun _ ds -> calc_frac ds)  <$> token '.' <*> (many digit);
        pure 0.
      ]

  let optexp =
    let exp =
      (fun _ sgn n -> 10. ** (sgn *. n))
      <$> (token 'e' <|> token 'E')
      <*> optsign
      <*> digits
    in
    choose [ exp ; pure 1.]

  (* Also allow initial +/- (not part of grammar spec) *)
  let number =
    (fun sgn d f e -> sgn *. (d +. f) *. e)
    <$> optsign
    <*> digits
    <*> optfrac
    <*> optexp

end

(* Tests (requires CamlCheck) *)
module Test_numbers = struct
  open Bb_camlcheck
  open Bb_camlcheck_generator

  module G  = Bb_camlcheck_generator
  module GS = Bb_camlcheck_generators

  let sof = string_of_float

  let prop_parse_int x =
    let x = float_of_int x in
    let s = sof x in
    is_true @@ 
      match Parser.run Numbers.number s with
      | Some y  -> s = sof y
      | None    -> false
  
  let prop_parse_float x =
    let s = sof x in
    is_true @@ 
      match Parser.run Numbers.number s with
      | Some y  -> s = sof y
      | None    -> false
  
  let prop_parse_big_or_small_float (x,n) =
    let z = x  *. 10. ** (float_of_int (n mod 64)) in
    let s = sof z in
    is_true @@ 
      match Parser.run Numbers.number s with
      | Some y  -> s = sof y
      | None    -> false

  type float_string = Float_string of string

  let prop_parse_float_string (Float_string fs) =
    let x = float_of_string fs in
    is_true @@ 
      match Parser.run Numbers.number fs with
      | Some y ->
          begin
            match Parser.run Numbers.number (sof x) with
            | Some z  -> sof x = sof y && sof y = sof z
            | _       -> false
          end
      | None ->
        false

  (* Custom register for float_string *)
  let float_string_register =
    (* Custom float_string generator *)
    let gen =
      let intg : int G.t = GS.get_generator () in
      let sign = one_of [pure "+"; pure "-"; pure ""] in
      let frac = (fun n -> Printf.sprintf ".%d" (abs n)) <$> intg in
      let exp =
        (fun e s n -> String.concat "" [e; s; n])
        <$> one_of [pure "e"; pure "E";] 
        <*> one_of [pure "+"; pure "-"; pure ""]
        <*> ((fun n -> string_of_int @@ abs @@ n mod 23) <$> intg)
      in
      (fun s n f e -> Float_string (String.concat "" [s;n;f;e]))
      <$> sign
      <*> ((fun n -> string_of_int @@ abs n) <$> intg)
      <*> one_of [pure ""; frac]
      <*> one_of [pure ""; exp]
    in
    GS.Register.custom (GS.Register.add gen)

  let run_tests () =
    check prop_parse_int;
    check prop_parse_float;
    check prop_parse_big_or_small_float;
    check ~register:float_string_register prop_parse_float_string
end
