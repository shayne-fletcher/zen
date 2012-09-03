(** Financial dates interface *)
module type S =
  sig

    (** {2 Holidays} *)

    (** Return [true] if a day is a holiday; [false] otherwise. *)
    val is_business_day : CalendarLib.Date.t -> string -> bool

    (** {2 Date rolling} *)

    (** Supported market shift conventions.*)
    type shift_convention =
    | None
    | Following
    | ModifiedFollowing
    | Preceding
    | ModifiedPreceding
    ;;

    (** Shift a date to the next good business day using a shift
	convention.*)
    val shift : CalendarLib.Date.t -> shift_convention -> string -> CalendarLib.Date.t

    (** {2 Year fractions} *)

    (** Supported day-count conventions (sometimes called day-count
	basis).*)
    type day_count = 
    | DC_30_360
    | DC_ACT_360
    | DC_ACT_365
    | DC_ACT_ACT
    ;;
    
    (** Compute the year fraction between two dates using a given
	day-count method.*)
    val year_fraction : (CalendarLib.Date.t * CalendarLib.Date.t) -> day_count -> float

    (** The number of days between two dates *) 
    val day_diff : CalendarLib.Date.t -> CalendarLib.Date.t -> int
    (** The number of years between two dates computed as the number of days / 365.0 *)
    val year_diff : CalendarLib.Date.t -> CalendarLib.Date.t -> float

  end
