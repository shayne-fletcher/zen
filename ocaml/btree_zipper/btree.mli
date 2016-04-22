type 'a t

val nil : 'a t
val make_tree : 'a t * 'a * 'a t -> 'a t

module type Cursor_sig = sig

  type 'a cursor

  exception Top
  exception Bottom

  val of_tree : 'a t -> 'a cursor
  val to_tree : 'a cursor -> 'a t

  val replace : 'a cursor -> 'a t -> 'a cursor
  val delete : 'a cursor -> 'a cursor

  val move_up : 'a cursor -> 'a cursor
  val move_left : 'a cursor -> 'a cursor
  val move_right : 'a cursor -> 'a cursor

  val most_left : 'a t -> 'a cursor
  val most_right : 'a t -> 'a cursor

  val first_leaf : 'a t -> 'a cursor
  val last_leaf : 'a t -> 'a cursor
  val next_leaf : 'a cursor -> 'a cursor
  val prev_leaf : 'a cursor -> 'a cursor

end

module Cursor : Cursor_sig
