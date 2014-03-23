type t=Unix.tm

external boost_gregorian_day_clock_local_day : unit -> t = "caml_boost_gregorian_day_clock_local_day"
external boost_gregorian_ctor : int -> int -> int -> t = "caml_boost_gregorian_ctor"

let string_of_date tm = 
  Printf.sprintf "%04d-%02d-%02d" 
    (tm.Unix.tm_year+1900) (tm.Unix.tm_mon+1) (tm.Unix.tm_mday)

let local_day () = boost_gregorian_day_clock_local_day ()

let mk_date (yr, mon, day) = boost_gregorian_ctor yr mon day

let day t = t.Unix.tm_mday
let month t = t.Unix.tm_mon + 1
let year t = t.Unix.tm_year + 1900

let year_month_day t = (year t, month t, day t)

let compare t u  =
  if t.Unix.tm_year < u.Unix.tm_year then (-1)
  else if u.Unix.tm_year > t.Unix.tm_year then 1
  else if t.Unix.tm_mon < u.Unix.tm_mon then (-1)
  else if u.Unix.tm_mon > t.Unix.tm_mon then 1
  else if t.Unix.tm_mday < u.Unix.tm_mday then (-1)
  else if u.Unix.tm_mday > u.Unix.tm_mday then 1
  else 0

