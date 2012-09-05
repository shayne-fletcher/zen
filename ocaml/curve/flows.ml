type flow = 
  {
    flow_start : Dates.t ; 
    flow_end : Dates.t  ;
    flow_pay : Dates.t ;
    flow_accrual : float ;
  } 
;;

let rec parse_flow = parser
  [< 'Genlex.Kwd "{" ; 
     s = Dates.parse_date ; 'Genlex.Kwd ";" ;
     e = Dates.parse_date ; 'Genlex.Kwd ";" ;
     p = Dates.parse_date ; 'Genlex.Kwd ";" ;
     a = (parser | [< 'Genlex.Float f >] -> f); 
    'Genlex.Kwd "}" >] ->
  {flow_start=s; flow_end=e; flow_pay=p; flow_accrual=a}
;;

let string_of_flow : flow -> string =
  fun f ->
    let s = Dates.string_of_date f.flow_start
    and e = Dates.string_of_date f.flow_end
    and p = Dates.string_of_date f.flow_pay
    and a = string_of_float f.flow_accrual
    in "{"^s^" ; "^ e^" ; "^p^ "; "^a^"}"
;;

let flow_of_string : string -> flow =
  fun s ->
    let lexer = Genlex.make_lexer ["{"; ";"; "}"] in 
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

let parse_resolution = parser
  | [< 'Genlex.Ident s >]  -> resolution_of_string s
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
    gfp_start : Dates.t ;
    gfp_end : Dates.t ;
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
      (Dates.string_of_date pack.gfp_start)^";"^
      (Dates.string_of_date pack.gfp_end)^";"^
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

let rec parse_gen_flows_param_pack = parser
[<  'Genlex.Kwd "{" ;
    start      = Dates.parse_date ; 'Genlex.Kwd ";" ;
    end_       = Dates.parse_date ; 'Genlex.Kwd ";" ;
    period     = (parser [<'Genlex.Int i>]->i) ; 'Genlex.Kwd ";" ;
    unit       = parse_resolution ; 'Genlex.Kwd ";" ;
    acc_shift  = Dates.parse_shift_convention ; 'Genlex.Kwd ";" ;
    acc_basis  = Dates.parse_day_count ; 'Genlex.Kwd ";" ;
    acc_hols   = (parser [<'Genlex.String s>]->s) ; 'Genlex.Kwd ";" ;
    pay_delay  = (parser [<'Genlex.Int i>]->i) ; 'Genlex.Kwd ";" ;
    pay_shift  = Dates.parse_shift_convention ; 'Genlex.Kwd ";" ;
    pay_basis  = Dates.parse_day_count ; 'Genlex.Kwd ";" ;
    pay_hols   = ((parser [<'Genlex.String s>]->s)) ;
    'Genlex.Kwd "}"
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
;;

let gen_flows_param_pack_of_string : string -> gen_flows_param_pack = 
  fun s ->
     let lexer = Genlex.make_lexer ["{"; ";";"}"]
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
