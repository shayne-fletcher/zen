module type Bitset_sig = sig

  type t

  val empty : unit -> t (*The bitset will expand automatically as necessary*)
  val create : int -> t (*Empty bitset with at least an initial capacity in number of bits*)
  val set : t -> int -> unit (*[set s n] sets the nth-bit in the bitset to true*)

end

module Bitset : Bitset_sig = struct

  type t = string ref

  let empty : unit -> t = fun () -> ref ""

  let create_ (tag : string) (c : char) (n : int) =
    if n < 0 then invalid_arg ("Bit_set."^tag^": negative size");
    let size = n / 8 + (if n mod 8 = 0 then 0 else 1) in
    ref (String.make size c)

  let create : int -> t = create_ "create" '\000' (*The null character*)

  let capacity (t : t) = (String.length !t) * 8

  let extend (t : t) (n : int) = (*len in bits*)
    if n > capacity t then
      let t' = create n in
      String.blit !t 0 !t' 0 (String.length !t);
      t := !t'

  type bit_op =
  | Set
  | Unset
  | Toggle

  let rec apply_bit_op (tag : string) (op : bit_op) (t : t) (x : int) =
    let pos = x / 8 in
    if pos < 0 then invalid_arg ("Bit_set"^tag^": negative index")
    else if pos < String.length !t then
      let delta = x mod 8 in
      let c = Char.code (String.unsafe_get !t pos) in
      let mask = 1 lsl delta in
      let v = (c land mask) <> 0 in
      let bset c = String.unsafe_set !t pos (Char.unsafe_chr c) in
      match op with
      | Set -> 
        if not v then
          bset (c lor mask)
      | Unset ->
        if v then
          bset (c lxor mask)
      | Toggle ->
        bset (c lxor mask);
    else
      match op with
      | Set | Toggle -> 
        extend t (x + 1);
        apply_bit_op tag op t x
      | Unset ->
        ()

let set (t : t) (x : int) = apply_bit_op "set" Set t x
          
    
end
