(**[Bdate] module interface*)

(** A set of date-time libraries based on generic programming
    concepts. See
    {{:http://www.boost.org/doc/libs/1_55_0/doc/html/date_time.html}
    Boost.Date_time} *)
module type S = sig

  (** {2 Types}*)

  type t
  (**The type of a date*)

  val string_of_date : t -> string
  (** @return a string represenation of a date*)

  (**{2 Functions}*)

  val local_day : unit -> t
  (** Get the local day based on the time zone settings of the
      computer*)

end
