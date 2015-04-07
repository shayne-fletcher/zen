module type Bitset_sig =
  sig
    type t
    val empty : unit -> t
    val create : int -> t
    val set : t -> int -> unit
    val mem : t -> int -> bool
    val string_of_set : t -> string
  end

module Bitset : Bitset_sig = struct

  type t = string ref

  let string_of_c (c : int) : string =
    (*Pre : [c] in [0 .. 255]*)
    let buf = Buffer.create 8 in
    let rc = ref c in
    Buffer.clear buf;
    for i = 1 to 8 do
      Buffer.add_char buf (if !rc land 1 == 1 then '1' else '0');
      rc := !rc lsr 1
    done;
    Buffer.contents buf
    
  let string_of_set (s : t) : string =
    let res : string ref = ref "" in
    for i = 0 to (String.length !s) - 1 do
      res := !res ^ (string_of_c (Char.code (String.unsafe_get !s i)))
    done;
    !res

  let mem (s : t) (i : int) : bool =
    let pos = i / 8 in 
    if pos < 0 then
      invalid_arg ("Bit_set.mem : negative index")
    else
      if pos < String.length !s then
        let delta = i mod 8 in
        let c = Char.code (String.unsafe_get !s pos) in
        (c land (1 lsl delta)) <> 0
      else false

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

  let rec apply_bit_op (tag : string) (op : bit_op) (t : t) (x : int) : unit =

    (*First, find the affected byte*)
    let pos = x / 8 in
    if pos < 0 then 
      invalid_arg ("Bit_set"^tag^": negative index")
    else 
      if pos < String.length !t then

      (*Get the byte*)
        let c = Char.code (String.unsafe_get !t pos) in

      (*Offset of bit into the byte*)
        let delta = x mod 8 in 

      (*[n lsl m ]shifts [n] to the left by [m] bits*)
        let mask = 1 lsl delta in

      (*Bitwise logical and, to test if the bit needs setting*)
        let v = (c land mask) <> 0 in

      (*Utility function to install a [c] at [pos] *)
        let bset c = 
          String.unsafe_set !t pos (Char.unsafe_chr c) in

        match op with
        | Set -> 
          if not v then
          (*The result of the logical and was zero...*)
            bset (c lor mask) (*... which means the bit needs setting*)

        | Unset ->
        (*The result of the logical and was non-zero*)
          if v then
            bset (c lxor mask) (*... which means the bit needs erasing*)
        | Toggle -> bset (c lxor mask);
    else
      (*We need to extend the length of the vector*)            
      match op with
      | Set | Toggle -> 
        extend t (x + 1);
        apply_bit_op tag op t x (*Try again*)
      | Unset -> () (*Not implemented*)

let set (t : t) (x : int) : unit = apply_bit_op "set" Set t x
          
end

