type flow = 
  {
    flow_start : CalendarLib.Date.t ; 
    flow_end : CalendarLib.Date.t  ;
    flow_pay : CalendarLib.Date.t ;
    flow_accrual : float ;
  } 
;;

let string_of_flow : flow -> string =
  fun f ->
    "{flow_start="^(CalendarLib.Printer.Date.to_string f.flow_start^", ")^ 
      "flow_end="^ (CalendarLib.Printer.Date.to_string f.flow_end^", ")^ 
      "flow_pay="^(CalendarLib.Printer.Date.to_string f.flow_pay^", ")^  
      "flow_accrual="^(string_of_float f.flow_accrual)^"}"
;;

type resolution = DAY | WEEK | MONTH | YEAR ;;

let make_tenor u n =
  match u with
  | DAY -> CalendarLib.Date.Period.day n
  | WEEK -> CalendarLib.Date.Period.week n
  | MONTH -> CalendarLib.Date.Period.month n
  | YEAR -> CalendarLib.Date.Period.year n
;;

type gen_flows_param_pack =
  {
    gfp_start : CalendarLib.Date.t ;
    gfp_end : CalendarLib.Date.t ;
    gfp_period : int ;
    gfp_unit : resolution ;
    gfp_accrual_shift_conv : Dates.shift_convention ;
    gfp_accrual_basis : Dates.day_count ;
    gfp_accrual_hols : string ;
    gfp_payment_delay : int ;
    gfp_payment_shift_conv : Dates.shift_convention ;
    gfp_payment_basis : Dates.day_count ;
    gfp_payment_hols : string
  }
;;

let gen_flows : gen_flows_param_pack -> (flow) list =
  fun params ->
    let rec gen_flows_ =
      fun params day i ->
    	let start = params.gfp_start
    	and end_ = params.gfp_end
    	and u = params.gfp_unit
    	in
        if day >= end_ then []
    	else
      	  let roll_start = CalendarLib.Date.add start (make_tenor u (i*params.gfp_period))
    	  and roll_end = CalendarLib.Date.add start (make_tenor u ((i+1)*params.gfp_period))
    	  in
          let acc_start = Dates.shift roll_start params.gfp_accrual_shift_conv params.gfp_accrual_hols
    	  and acc_end = Dates.shift roll_end params.gfp_accrual_shift_conv params.gfp_accrual_hols
    	  and pay_delay = make_tenor DAY params.gfp_payment_delay
    	  in 
      	  let pay = Dates.shift (CalendarLib.Date.add roll_end pay_delay) params.gfp_payment_shift_conv params.gfp_payment_hols
      	  and alpha = Dates.year_fraction (acc_start, acc_end) params.gfp_accrual_basis
    	  in {flow_start=acc_start; flow_end=acc_end; flow_pay=pay; flow_accrual=alpha} :: gen_flows_ params roll_end (i + 1)
    and  day = params.gfp_start 
    in 
    gen_flows_ params day 0
;;
    
    
(* Test *)

open Dates ;;

let string_of_list f l = "[" ^ String.concat ";\n" (List.map f l) ^ "]" ;;

let start = CalendarLib.Date.make 2012 01 03
and end_ = CalendarLib.Date.make 2022 01 03
in 
  let schedule = 
    {
      gfp_start=start; 
      gfp_end=end_; 
      gfp_period=3; 
      gfp_unit=MONTH; 
      gfp_accrual_shift_conv=ModifiedFollowing; 
      gfp_accrual_basis=DC_ACT_360; 
      gfp_accrual_hols="nyc"; 
      gfp_payment_delay=2 ; (* 2 day lag. *)
      gfp_payment_shift_conv=ModifiedFollowing;
      gfp_payment_basis=DC_ACT_360;
      gfp_payment_hols="nyc"
    }
  in Printf.printf "%s\n" (string_of_list string_of_flow (gen_flows schedule))
;;

