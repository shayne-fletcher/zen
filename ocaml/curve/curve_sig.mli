(** Curve interface *)
module type S = sig

  module Dates: Dates_sig.S
    (** Financial date arithemetic. *)

  module Roots: Roots_sig.S
    (** Root finding implementations. *)

  module Interpolation: Interpolation_sig.S
    (** Interpolation algorithms. *)

  module Flows: Flows_sig.S
    (** Flow representations. *)

end