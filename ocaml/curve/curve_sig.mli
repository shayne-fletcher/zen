(** Curve interface *)
module type S = sig

  module Roots: Roots_sig.S
    (** Roots *)

  module Interpolation: Interpolation_sig.S
    (** Interpolation *)

  module Dates: Dates_sig.S
    (** Dates *)

  module Flows: Flows_sig.S
    (** Flows *)

  module Deals : Deals_sig.S
    (** Deals *)

  module Curves : Curves_sig.S
    (** Curves *)

end

