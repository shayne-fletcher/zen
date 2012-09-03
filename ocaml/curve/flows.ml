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
    let flow_start = CalendarLib.Printer.Date.sprint "%Y %m %d" f.flow_start
    and flow_end =CalendarLib.Printer.Date.sprint "%Y %m %d" f.flow_end
    and flow_pay = CalendarLib.Printer.Date.sprint "%Y %m %d" f.flow_pay
    and flow_accrual = string_of_float f.flow_accrual
    in "{flow_start="^flow_start^"; flow_end="^ flow_end^"; flow_pay="^flow_pay^"; flow_accrual="^flow_accrual^"}"
;;

open Genlex;;

let flow_of_string : string -> flow =
  fun s ->
    let lexer = make_lexer 
      [
	"{"; 
	";";
	"}";
	"=";
	"}";
	"flow_start";
	"flow_end";
	"flow_pay";
	"flow_accrual";
      ]
    in
      let rec parse_flow = parser
	[< 'Kwd "{" ; 
	   'Kwd "flow_start" ; 'Kwd "=" ; flow_start_year=parse_int; flow_start_month=parse_int ; flow_start_day = parse_int ; 'Kwd ";" ;
	   'Kwd "flow_end" ; 'Kwd "=" ; flow_end_year=parse_int; flow_end_month=parse_int ; flow_end_day = parse_int ; 'Kwd ";" ;
	   'Kwd "flow_pay" ; 'Kwd "=" ; flow_pay_year=parse_int; flow_pay_month=parse_int ; flow_pay_day = parse_int ; 'Kwd ";" ;
	   'Kwd "flow_accrual" ; 'Kwd "=" ; flow_accrual=parse_float ;
	   'Kwd "}" >] ->
	{flow_start=(CalendarLib.Date.make flow_start_year flow_start_month flow_start_day);
	 flow_end=(CalendarLib.Date.make flow_end_year flow_end_month  flow_end_day);
	 flow_pay=(CalendarLib.Date.make flow_pay_year flow_pay_month  flow_pay_day);
	 flow_accrual=flow_accrual
	}
      and parse_int = parser
	  | [< 'Int i>] -> i
	  | [< _ >] -> failwith "parse error"
      and parse_float = parser
	  | [< 'Float f>] -> f
	  | [< _ >] -> failwith "parse error"
      in
        parse_flow (lexer (Stream.of_string s)) 
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
