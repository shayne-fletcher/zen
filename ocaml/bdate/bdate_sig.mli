(**[Bdate] module interface*)

(** A set of date-time libraries based on generic programming
    concepts. See
    {{:http://www.boost.org/doc/libs/1_55_0/doc/html/date_time.html}
    Boost.Date_time} *)
module type S = sig

  (** {2 Types}*)

  type t
  (**The type of a date*)

  val compare : t -> t -> int
  (** Lexicographical ordering *)

  val string_of_date : t -> string
  (** @return a string representation of a date*)

  (**{2 Functions}*)

  val mk_date : int * int * int -> t
  (** Create a date with the given year, month and day *)

  val local_day : unit -> t
  (** Get the local day based on the time zone settings of the
      computer*)

  val year : t -> int
  (** Project the year of a date*)

  val month : t -> int
  (** Project the month of a date*)

  val day : t -> int
  (** Project the day of the month of a date*)

  val year_month_day : t -> int * int * int
    (** @return tuple of components*)

end
