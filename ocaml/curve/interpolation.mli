(** Interpolation algorithms *)
module Interpolation :
  sig

    (** {2 Utilities} *)

    (** Find the first position in xs that x can be inserted without
	violating the ordering*)
    val lower_bound : float -> float list -> int

    (** Find the last position in xs that x can be inserted without
	violating the ordering*)
    val upper_bound : float -> float list -> int

    (** Find the largest subrange in xs in which x could be inserted
	in any position without violating the ordering *)
    val equal_range : float -> float list -> (int*int)

    (** It's an error if {i x} lies to the left or the right of the
	domain of interpolation otherwise, find indicies {i i}, {i j}
	such that [List.nth xs i] {i <=} x {i <=} [List.nth xs j] *)
    val bind : float -> float list -> (int*int)

    (** {2 Interpolation schemes} *)

    (** Linear interpolation (in one dimension) *)
    val linear_interpolation : float list -> float list -> float -> float

    (** Log-linear interpolation (in one dimension) *)
    val loglinear_interpolation : float list -> float list -> float -> float

  end
;;

