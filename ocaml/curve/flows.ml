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

type resolution = | DAY | WEEK | MONTH | YEAR ;;

let string_of_resolution : resolution -> string =
  function 
  | DAY -> "DAY"
  | WEEK -> "WEEK"
  | MONTH -> "MONTH"
  | YEAR -> "YEAR"
;;

let resolution_of_string : string -> resolution =
  function
  | "DAY" -> DAY
  | "WEEK" -> WEEK
  | "MONTH" -> MONTH
  | "YEAR" -> YEAR
  | s -> failwith ("Convert convert \""^s^"\" to a resolution")
;;

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

let string_of_gen_flows_param_pack : gen_flows_param_pack -> string =
  fun pack ->
    "{"^
      (CalendarLib.Printer.Date.sprint "%Y %m %d" pack.gfp_start)^";"^
      (CalendarLib.Printer.Date.sprint "%Y %m %d" pack.gfp_end)^";"^
      (string_of_int pack.gfp_period)^";"^
      (string_of_resolution pack.gfp_unit)^";"^
      (Dates.string_of_shift_convention pack.gfp_accrual_shift_conv)^";"^
      (Dates.string_of_day_count pack.gfp_accrual_basis)^";"^
      "\""^pack.gfp_accrual_hols^"\";"^
      (string_of_int pack.gfp_payment_delay)^";"^
      (Dates.string_of_shift_convention pack.gfp_payment_shift_conv)^";"^
      (Dates.string_of_day_count pack.gfp_payment_basis)^";"^
      "\""^pack.gfp_payment_hols^"\"}"
;;

let gen_flows_param_pack_of_string : string -> gen_flows_param_pack = 
  fun s ->
     let lexer = make_lexer ["{"; ";";"}"]
     in
     let rec parse_gen_flows_param_pack = parser 
     [<  'Kwd "{"                                 ;
 	 start      = parse_date       ; 'Kwd ";" ;
	 end_       = parse_date       ; 'Kwd ";" ;
	 period     = parse_int        ; 'Kwd ";" ;
	 unit       = parse_resolution ; 'Kwd ";" ;
	 acc_shift  = parse_shift      ; 'Kwd ";" ;
	 acc_basis  = parse_basis      ; 'Kwd ";" ;
	 acc_hols   = parse_string     ; 'Kwd ";" ;
	 pay_delay  = parse_int        ; 'Kwd ";" ;
	 pay_shift  = parse_shift      ; 'Kwd ";" ;
	 pay_basis  = parse_basis      ; 'Kwd ";" ;
	 pay_hols   = parse_string                ;
         'Kwd "}"
     >] ->
      {
      	gfp_start=start;
      	gfp_end=end_;
      	gfp_period=period;
      	gfp_unit=unit;
      	gfp_accrual_shift_conv=acc_shift;
      	gfp_accrual_basis=acc_basis;
      	gfp_accrual_hols=acc_hols;
      	gfp_payment_delay=pay_delay;
      	gfp_payment_shift_conv=pay_shift;
      	gfp_payment_basis=pay_basis;
      	gfp_payment_hols=pay_hols
      }
    and parse_date = parser
    	| [< y = parse_int; m = parse_int; d = parse_int >] -> 
	    CalendarLib.Date.make y m d
    and parse_shift = parser
    	| [< s = parse_ident >] -> Dates.shift_convention_of_string s
    and parse_basis = parser
    	| [< s = parse_ident >] -> Dates.day_count_of_string s
    and parse_resolution = parser
    	| [< s = parse_ident >] -> resolution_of_string s
    and parse_int = parser
    	| [< 'Int i >] -> i
    and parse_string = parser
    	| [< 'String s >] -> s
    and parse_ident = parser
    	| [< 'Ident s >] -> s
    in parse_gen_flows_param_pack (lexer (Stream.of_string s))
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
