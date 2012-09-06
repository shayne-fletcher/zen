(** Curves.curve interface *)
module type S = sig

  (** {2 Curve operations }*)

  (** The curve type *)
  type curve = 
    {
      curve_dates : CalendarLib.Date.t list ;
      curve_abscissae : float list ;
      curve_ordinates : float list ;
      curve_interpolation : float list -> float list -> float -> float
    }

  (** Make a string of curve *)
  val string_of_curve : curve -> string

  (** Append the implied discount factor to the curve *)
  val append_deposit : curve -> Deals.cash -> curve

  (** Append the implied discount factor to the curve *)
  val append_vanilla_swap : curve -> Deals.vanilla_swap -> curve

  (** {2 Curve querying }*)

  (** Extract the base date from a curve *)
  val base_date : curve -> CalendarLib.Date.t

  (** Query the curve for a response *)
  val evaluate : curve -> float -> float

end
