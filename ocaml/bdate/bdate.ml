type t=Unix.tm

external boost_gregorian_day_clock_local_day : unit -> t = "caml_boost_gregorian_day_clock_local_day"

let local_day () = boost_gregorian_day_clock_local_day ()
let string_of_date tm = 
  Printf.sprintf "%04d-%02d-%02d" 
    (tm.Unix.tm_year+1900) (tm.Unix.tm_mon+1) (tm.Unix.tm_mday)
