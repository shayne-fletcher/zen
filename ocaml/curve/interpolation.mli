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

    (** {2 Interpolation schemes} *)

  end
;;

