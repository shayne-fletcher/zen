module Flows :
  sig
    type flow = 
      {
	start : CalendarLib.Date.t ; 
	end_ : CalendarLib.Date.t  ;
	pay : CalendarLib.Date.t ;
	accrual : float ;
      } ;;

    val string_of_flow : flow -> string

  end
;;

