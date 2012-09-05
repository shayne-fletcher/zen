(** Financial dates interface *)
module type S =
  sig

    (** {2 Date } *)

    (** The date type*)
    type t = CalendarLib.Date.t

    (** {2 Date I/O } *)

    val string_of_date : t -> string

    (** Parse a date from a token stream *)
    val parse_date : Genlex.token Stream.t -> t

    (** {2 Holidays} *)

    (** Return [true] if a day is a holiday; [false] otherwise *)
    val is_business_day : t -> string -> bool

    (** {2 Date rolling} *)

    (** Supported market shift conventions.*)
    type shift_convention =
    | NoShift
    | Following
    | ModifiedFollowing
    | Preceding
    | ModifiedPreceding
    ;;

    (** Parse a shift convention from a token stream *)
    val parse_shift_convention : Genlex.token Stream.t -> shift_convention

    (** shift convention as a string *)
    val string_of_shift_convention : shift_convention -> string

    (** Parse a shift convention from a string *)
    val shift_convention_of_string : string -> shift_convention

    (** Shift a date to the next good business day using a shift
	convention.*)
    val shift : t -> shift_convention -> string -> t

    (** {2 Year fractions} *)

    (** Supported day-count conventions (sometimes called day-count
	basis).*)
    type day_count = 
    | DC_30_360
    | DC_ACT_360
    | DC_ACT_365
    | DC_ACT_ACT
    ;;

    (** Day-count as a string *)
    val day_count_of_string : string -> day_count

    (** Parse a day-count from a token stream *)
    val parse_day_count : Genlex.token Stream.t -> day_count

    (** Parse a day-count from a string *)
    val string_of_day_count : day_count -> string
    
    (** Compute the year fraction between two dates using a given
	day-count method.*)
    val year_fraction : (t * t) -> day_count -> float

    (** The number of days between two dates *) 
    val day_diff : t -> t -> int

    (** The number of years between two dates computed as the number of days / 365.0 *)
    val year_diff : t -> t -> float

  end
