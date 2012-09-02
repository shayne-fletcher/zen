module Flows = 
  struct

  type flow = 
    {
	start : CalendarLib.Date.t ; 
	end_ : CalendarLib.Date.t  ;
	pay : CalendarLib.Date.t ;
	accrual : float ;
     } 
  ;;

  let string_of_flow : flow -> string =
    fun f ->
      "{start="^(CalendarLib.Printer.Date.to_string f.start^", ")^ 
      "end_="^ (CalendarLib.Printer.Date.to_string f.end_^", ")^ 
      "pay="^(CalendarLib.Printer.Date.to_string f.pay^", ")^  
      "accrual="^(string_of_float f.accrual)^"}"
  ;;

  end
;;

(* Test *)

open Flows ;;
open Dates ;;

let start = CalendarLib.Date.make 2012 01 03
and end_ = CalendarLib.Date.make 2012 07 03
in 
  let pay = CalendarLib.Date.add start (CalendarLib.Date.Period.day 2)
  and accrual = Dates.year_fraction (start, end_) Dates.DC_ACT_360
  in 
  let
      flo = {start=start; end_=end_; pay=pay; accrual=accrual}
  in Printf.printf "%s\n" (string_of_flow flo) 
;;
