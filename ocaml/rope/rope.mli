(*Jean-Christophe Filliâtre

  https://www.lri.fr/~filliatr/software.en.html*)

exception Out_of_bounds

module type STRING = sig

  type t
  type char

  val empty : t

  val length : t -> int
  val singleton : char -> t
  val append : t -> t -> t
  val get : t -> int -> char

  val sub : t -> int -> int -> t
  (**[sub t ofs len] extracts the substring of length [len] at
     offset [ofs], that is [t[ofs..ofs + len - 1]]. Will always be
     called with a valid range*)

  val iter_range : (char -> unit) -> t -> int -> int -> unit
  (**[iter_range f t ofs len] successively iterates [f] over
     characters of [t] at offsets [ofs], [ofs + 1], ..., [ofs + len -
     1], in this order. Will always be called with a valid range*)

  val print : Format.formatter -> t -> unit

end

module type ROPE = sig

  include STRING

  val set : t -> int -> char -> t
  (**[set t i c] computes a new rope identical to [t], apart from
     character [i] which is set to [c]. Raises [Out_of_bounds] if [i <
     0 || i >= length t]*)

  val delete : t -> int -> t
  (**[delete t i] returns a new rope obtained by removing character
     [i] from [t]. Raises [Out_of_bounds] if [i < 0 || i >= length
     t]*)

  val insert_char : t -> int -> char -> t
  (**[insert t i c] returns a new rope obtained from the insertion of
     character [c] at position [i]. Raises [Out_of_bounds] if [i < 0
     || i > length t]*)

  val insert : t -> int -> t -> t
  (**[insert t i r] computes a new rope resulting from the insertion
     of rope [r] at position [i] in [t]. Raises [Out_of_bounds] if [i < 0
     || i > length t]*)

    (**Cursors are persistent data structures to navigate within
       ropes. It is convenient to see the cursor as placed between two
       characters, so that a rope of length [n] has [n + 1] cursor
       positions*)
  module Cursor : sig

    type cursor
    (**The type of a cursor*)

    val create : t -> int -> cursor
    (**[create t i] returns a cursor placed before character [i] of
       rope [t]. Raises [Out_of_bounds] if [i < 0 || i > length t]*)

    val position : cursor -> int
    (**[position c] returns the position of cursor [c] in its rope*)

    val to_rope : cursor -> t
    (**[to_rope c] returns the rope corresponding to cursor [c]*)

    val move_forward : cursor -> int -> cursor
    (**[move_forward c n] moves cursor [c] forward [n]
       characters. Raises [Invalid_argument] if [n < 0]. Raises
       [Out_of_bounds] if it moves the cursor beyond the end of the
       rope*)

    val move_backward : cursor -> int -> cursor
    (**[move_backward c n] moves cursor [c] backward [n]
       characters. Raises [Invalid_argument] if [n < 0]. Raises
       [Out_of_bounds] if it moves the cursor beyond the start of the
       rope*)

    val move : cursor -> int -> cursor
    (**[move c n] moves cursor [c] away [n] characters.  Raises
       [Out_of_bounds] if it moves the cursor beyond the start or end
       of the rope*)

    val get : cursor -> char
    (**[get c] returns the character right after cursor [c]. Raises
       [Out_of_bounds] if the cursor is located at the end of the
       rope*)

    val set : cursor -> char -> cursor
    (**[set c x] returns a new cursor identical to [c] apart from
       the character right after the cursor position which is set to
       [x]. Raises [Out_of_bounds] if the cursor is located at the end
       of the rope*)

    val insert_char : cursor -> char -> cursor
    (**[insert_char c x] returns a new cursor obtained from [c] by
       inserting character [x] at the current position. The new cursor
       is located right before the newly inserted character.*)

    val insert : cursor -> t -> cursor
   (**[insert c r] is similar to [insert_char] but inserts a rope
      [r] at the cursor point instead of a character*)

    val print : Format.formatter -> cursor -> unit
    (**[print fmt c] prints the cursor [c] on formatter [fmt] as a
       string ["abc...|def..."] where ["abc..."] is the portion of the
       rope before the cursor position and ["def..."] the portion
       after*)

  end

end

(**A functor to build ropes, turning an implementation of strings [S]
   into an implementation of ropes.

   It is controlled by two parameters:

   - [small_length] is the maximal length under which we perform
   concatentation of flat strings, i.e. when two ropes of length at
   most [small_length] are concatenated, then the corresponding flat
   string is built

   - [maximal_height] is the threshold for rebalancing : when a rope
   has height at least [maximal_height] it is then rebalanced;
   setting [small_length] to [max_int] will result in ropes that are
   never rebalanced (fine for many applications)
*)

module type CONTROL = sig

  val small_length : int
  val maximal_height : int

end

module Make (S : STRING) (C : CONTROL) : sig
  include ROPE with type char = S.char

  val of_string : S.t -> t

end

(**Instance : Usual strings (i.e. with [type char = Char.t] is a
   particualr instance of function [Make] above, which is directly
   provided here as module [S])*)
module S : sig
  include ROPE with type char = Char.t

  val of_string : string -> t

end

(**Another instance : Ropes with function leaves. This allows for
   having pieces of the rope represented as functions (instead of flat
   strings). This is convenient to represent large strings such as files
   on disk*)
module SF : sig
  include ROPE with type char = Char.t

  val of_string : string -> t
  val of_function : int -> (int -> char) -> t

end

(**Elements of ropes can be of any type of course. In that case, they
   must rather be seen as arrays instead of strings. The following
   functor builds ropes for a given (printable) type of elements (using
   arrays internally for flat strings)*)
module type Printable_type = sig
  type t
  val print : Format.formatter -> t -> unit
end

module Make_array (X : Printable_type) : sig
  include ROPE with type char = X.t

  val of_array : X.t array -> t
  val create : int -> X.t -> t
  val init : int -> (int -> X.t) -> t

end
