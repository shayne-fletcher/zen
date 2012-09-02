open Dates ;;

(** Cash flow generation *)
module Flows :
  sig

    (** A cash flow *)
    type flow = 
      {
	flow_start : CalendarLib.Date.t ; 
	flow_end : CalendarLib.Date.t  ;
	flow_pay : CalendarLib.Date.t ;
	flow_accrual : float ;
      } 
    ;;

    (** Make a string of a flow *)
    val string_of_flow : flow -> string

    (** Flow period units *)
    type resolution = DAY | WEEK | MONTH | YEAR ;;

    (** Make a flow period of a certain length *)
    val make_tenor: resolution -> int -> CalendarLib.Date.field CalendarLib.Date.Period.period

    (** Bundle parameters for [generate_flows]*)
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

    (** Roll out a set of flows *)
    val gen_flows : gen_flows_param_pack -> (flow) list

  end
;;

