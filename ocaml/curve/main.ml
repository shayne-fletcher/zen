(* Requires Calendar - http://calendar.forge.ocamlcore.org/ *)

module Parser = 
struct
  let rec parse_list : (Genlex.token Stream.t -> 'a) -> Genlex.token Stream.t -> 'a list = 
    fun f ->
      let rec list = parser | [< e = parse_top >] -> e
      and parse_top = parser [< 'Genlex.Kwd "[" ; h = f ; t = parse_more ; 'Genlex.Kwd "]" >] -> h::t
      and parse_more = parser | [< 'Genlex.Kwd ";" ; h = f ; t = parse_more >] -> h::t | [< >] -> []
      in list
end
;;

(** Parse deposits and bootstrap *)

let defs =
"[ {0.0004 ; {2004 08 24;2004 08 25;1;DAY;MODIFIED_FOLLOWING;DC_ACT_360;\"nyc\";0;MODIFIED_FOLLOWING;DC_ACT_360;\"nyc\"}}"   ^
" ;{0.0004 ; {2004 08 25;2004 08 26;1;DAY;MODIFIED_FOLLOWING;DC_ACT_360;\"nyc\";0;MODIFIED_FOLLOWING;DC_ACT_360;\"nyc\"}}"   ^
" ;{0.0004 ; {2004 08 25;2004 08 26;1;DAY;MODIFIED_FOLLOWING;DC_ACT_360;\"nyc\";0;MODIFIED_FOLLOWING;DC_ACT_360;\"nyc\"}}"   ^
" ;{0.0004 ; {2004 08 26;2004 09 27;1;MONTH;MODIFIED_FOLLOWING;DC_ACT_360;\"nyc\";0;MODIFIED_FOLLOWING;DC_ACT_360;\"nyc\"}}" ^
" ;{0.0054 ; {2004 08 26;2004 10 26;2;MONTH;MODIFIED_FOLLOWING;DC_ACT_360;\"nyc\";0;MODIFIED_FOLLOWING;DC_ACT_360;\"nyc\"}}" ^
" ;{0.0054 ; {2004 08 26;2004 11 26;3;MONTH;MODIFIED_FOLLOWING;DC_ACT_360;\"nyc\";0;MODIFIED_FOLLOWING;DC_ACT_360;\"nyc\"}}" ^
" ;{0.0066 ; {2004 08 26;2005 02 28;6;MONTH;MODIFIED_FOLLOWING;DC_ACT_360;\"nyc\";0;MODIFIED_FOLLOWING;DC_ACT_360;\"nyc\"}}]" 
;;

let t = CalendarLib.Date.make 2004 08 24 ;;
let lexer = Genlex.make_lexer ["[";"]";"{";"}";";"] in
let deposits = (Parser.parse_list Deals.parse_cash) (lexer (Stream.of_string defs)) in 
let c={Curves.curve_dates=[t];
       Curves.curve_abscissae=[0.0];
       Curves.curve_ordinates=[1.0];
       Curves.curve_interpolation=Interpolation.loglinear_interpolation
      } 
in let res = List.fold_left Curves.append_deposit c deposits in  Printf.printf "%s\n" (Curves.string_of_curve res)
;;

(* Bootstrapping *)

let t = CalendarLib.Date.make 2004 08 24 ;;
let tomorrow = CalendarLib.Date.add t (Flows.make_tenor Flows.DAY 1) ;;
let day_after_tomorrow = CalendarLib.Date.add tomorrow (Flows.make_tenor Flows.DAY 1) ;;

let c={Curves.curve_dates=[t];
       Curves.curve_abscissae=[0.0];
       Curves.curve_ordinates=[1.0];
       Curves.curve_interpolation=Interpolation.loglinear_interpolation
      } 
in
  let overnight_deposit = 
    Deals.make_cash 0.00040
      {Flows.gfp_start=t ; 
       Flows.gfp_end=tomorrow; 
       Flows.gfp_period=1;
       Flows.gfp_unit=Flows.DAY;
       Flows.gfp_accrual_shift_conv=Dates.ModifiedFollowing;
       Flows.gfp_accrual_basis=Dates.DC_ACT_360;
       Flows.gfp_accrual_hols="nyc";
       Flows.gfp_payment_delay=0;
       Flows.gfp_payment_shift_conv=Dates.ModifiedFollowing;
       Flows.gfp_payment_basis=Dates.DC_ACT_360;
       Flows.gfp_payment_hols="nyc"
      }
  and tomorrow_next_deposit =
    Deals.make_cash 0.00040
      {Flows.gfp_start=tomorrow;
       Flows.gfp_end=day_after_tomorrow;
       Flows.gfp_period=1;
       Flows.gfp_unit=Flows.DAY;
       Flows.gfp_accrual_shift_conv=Dates.ModifiedFollowing;
       Flows.gfp_accrual_basis=Dates.DC_ACT_360;
       Flows.gfp_accrual_hols="nyc";
       Flows.gfp_payment_delay=0;
       Flows.gfp_payment_shift_conv=Dates.ModifiedFollowing;
       Flows.gfp_payment_basis=Dates.DC_ACT_360;
       Flows.gfp_payment_hols="nyc"
      }
  and one_month_deposit =
    Deals.make_cash 0.00040
      {Flows.gfp_start=day_after_tomorrow;
       Flows.gfp_end=CalendarLib.Date.add day_after_tomorrow (Flows.make_tenor Flows.MONTH 1);
       Flows.gfp_period=1;
       Flows.gfp_unit=Flows.MONTH;
       Flows.gfp_accrual_shift_conv=Dates.ModifiedFollowing;
       Flows.gfp_accrual_basis=Dates.DC_ACT_360;
       Flows.gfp_accrual_hols="nyc";
       Flows.gfp_payment_delay=0;
       Flows.gfp_payment_shift_conv=Dates.ModifiedFollowing;
       Flows.gfp_payment_basis=Dates.DC_ACT_360;
       Flows.gfp_payment_hols="nyc"
      }
  and two_month_deposit =
    Deals.make_cash 0.00054
      {Flows.gfp_start=day_after_tomorrow;
       Flows.gfp_end=CalendarLib.Date.add day_after_tomorrow (Flows.make_tenor Flows.MONTH 2);
       Flows.gfp_period=2;
       Flows.gfp_unit=Flows.MONTH;
       Flows.gfp_accrual_shift_conv=Dates.ModifiedFollowing;
       Flows.gfp_accrual_basis=Dates.DC_ACT_360;
       Flows.gfp_accrual_hols="nyc";
       Flows.gfp_payment_delay=0;
       Flows.gfp_payment_shift_conv=Dates.ModifiedFollowing;
       Flows.gfp_payment_basis=Dates.DC_ACT_360;
       Flows.gfp_payment_hols="nyc"
      }
  and three_month_deposit =
    Deals.make_cash 0.00054
      {Flows.gfp_start=day_after_tomorrow;
       Flows.gfp_end=CalendarLib.Date.add day_after_tomorrow (Flows.make_tenor Flows.MONTH 3);
       Flows.gfp_period=3;
       Flows.gfp_unit=Flows.MONTH;
       Flows.gfp_accrual_shift_conv=Dates.ModifiedFollowing;
       Flows.gfp_accrual_basis=Dates.DC_ACT_360;
       Flows.gfp_accrual_hols="nyc";
       Flows.gfp_payment_delay=0;
       Flows.gfp_payment_shift_conv=Dates.ModifiedFollowing;
       Flows.gfp_payment_basis=Dates.DC_ACT_360;
       Flows.gfp_payment_hols="nyc"
      }
  and six_month_deposit =
    Deals.make_cash 0.00066
      {Flows.gfp_start=day_after_tomorrow;
       Flows.gfp_end=CalendarLib.Date.add day_after_tomorrow (Flows.make_tenor Flows.MONTH 6);
       Flows.gfp_period=6;
       Flows.gfp_unit=Flows.MONTH;
       Flows.gfp_accrual_shift_conv=Dates.ModifiedFollowing;
       Flows.gfp_accrual_basis=Dates.DC_ACT_360;
       Flows.gfp_accrual_hols="nyc";
       Flows.gfp_payment_delay=0;
       Flows.gfp_payment_shift_conv=Dates.ModifiedFollowing;
       Flows.gfp_payment_basis=Dates.DC_ACT_360;
       Flows.gfp_payment_hols="nyc"
      }
  and one_year_swap = 
    Deals.make_vanilla_swap 
      (Deals.make_fixed_leg  0.0007600 
	 {Flows.gfp_start=day_after_tomorrow;
	  Flows.gfp_end=CalendarLib.Date.add day_after_tomorrow (Flows.make_tenor Flows.YEAR 1); 
	  Flows.gfp_period=1;
	  Flows.gfp_unit=Flows.YEAR;
	  Flows.gfp_accrual_shift_conv=Dates.ModifiedFollowing;
	  Flows.gfp_accrual_basis=Dates.DC_ACT_360;
	  Flows.gfp_accrual_hols="nyc";
	  Flows.gfp_payment_delay=0;
	  Flows.gfp_payment_shift_conv=Dates.ModifiedFollowing;
	  Flows.gfp_payment_basis=Dates.DC_ACT_360;
	  Flows.gfp_payment_hols="nyc"}
      ) 
      (Deals.make_floating_leg 
	 {Flows.gfp_start=day_after_tomorrow;
	  Flows.gfp_end=CalendarLib.Date.add day_after_tomorrow (Flows.make_tenor Flows.YEAR 1); 
	  Flows.gfp_period=1;
	  Flows.gfp_unit=Flows.YEAR;
	  Flows.gfp_accrual_shift_conv=Dates.ModifiedFollowing;
	  Flows.gfp_accrual_basis=Dates.DC_ACT_360;
	  Flows.gfp_accrual_hols="nyc";
	  Flows.gfp_payment_delay=0;
	  Flows.gfp_payment_shift_conv=Dates.ModifiedFollowing;
	  Flows.gfp_payment_basis=Dates.DC_ACT_360;
	  Flows.gfp_payment_hols="nyc"}
      )
  and eighteen_month_swap = 
    Deals.make_vanilla_swap 
      (Deals.make_fixed_leg  0.0010400 
	 {Flows.gfp_start=day_after_tomorrow;
	  Flows.gfp_end=CalendarLib.Date.add day_after_tomorrow (Flows.make_tenor Flows.MONTH 18); 
	  Flows.gfp_period=6;
	  Flows.gfp_unit=Flows.MONTH;
	  Flows.gfp_accrual_shift_conv=Dates.ModifiedFollowing;
	  Flows.gfp_accrual_basis=Dates.DC_ACT_360;
	  Flows.gfp_accrual_hols="nyc";
	  Flows.gfp_payment_delay=0;
	  Flows.gfp_payment_shift_conv=Dates.ModifiedFollowing;
	  Flows.gfp_payment_basis=Dates.DC_ACT_360;
	  Flows.gfp_payment_hols="nyc"}
      ) 
      (Deals.make_floating_leg 
	 {Flows.gfp_start=day_after_tomorrow;
	  Flows.gfp_end=CalendarLib.Date.add day_after_tomorrow (Flows.make_tenor Flows.MONTH 18); 
	  Flows.gfp_period=6;
	  Flows.gfp_unit=Flows.MONTH;
	  Flows.gfp_accrual_shift_conv=Dates.ModifiedFollowing;
	  Flows.gfp_accrual_basis=Dates.DC_ACT_360;
	  Flows.gfp_accrual_hols="nyc";
	  Flows.gfp_payment_delay=0;
	  Flows.gfp_payment_shift_conv=Dates.ModifiedFollowing;
	  Flows.gfp_payment_basis=Dates.DC_ACT_360;
	  Flows.gfp_payment_hols="nyc"}
      )
  and two_year_swap = 
    Deals.make_vanilla_swap 
      (Deals.make_fixed_leg  0.0015100
	 {Flows.gfp_start=day_after_tomorrow;
	  Flows.gfp_end=CalendarLib.Date.add day_after_tomorrow (Flows.make_tenor Flows.YEAR 2); 
	  Flows.gfp_period=6;
	  Flows.gfp_unit=Flows.MONTH;
	  Flows.gfp_accrual_shift_conv=Dates.ModifiedFollowing;
	  Flows.gfp_accrual_basis=Dates.DC_ACT_360;
	  Flows.gfp_accrual_hols="nyc";
	  Flows.gfp_payment_delay=0;
	  Flows.gfp_payment_shift_conv=Dates.ModifiedFollowing;
	  Flows.gfp_payment_basis=Dates.DC_ACT_360;
	  Flows.gfp_payment_hols="nyc"}
      ) 
      (Deals.make_floating_leg 
	 {Flows.gfp_start=day_after_tomorrow;
	  Flows.gfp_end=CalendarLib.Date.add day_after_tomorrow (Flows.make_tenor Flows.YEAR 2); 
	  Flows.gfp_period=6;
	  Flows.gfp_unit=Flows.MONTH;
	  Flows.gfp_accrual_shift_conv=Dates.ModifiedFollowing;
	  Flows.gfp_accrual_basis=Dates.DC_ACT_360;
	  Flows.gfp_accrual_hols="nyc";
	  Flows.gfp_payment_delay=0;
	  Flows.gfp_payment_shift_conv=Dates.ModifiedFollowing;
	  Flows.gfp_payment_basis=Dates.DC_ACT_360;
	  Flows.gfp_payment_hols="nyc"}
      )
  and three_year_swap = 
    Deals.make_vanilla_swap 
      (Deals.make_fixed_leg  0.0026400
	 {Flows.gfp_start=day_after_tomorrow;
	  Flows.gfp_end=CalendarLib.Date.add day_after_tomorrow (Flows.make_tenor Flows.YEAR 3); 
	  Flows.gfp_period=6;
	  Flows.gfp_unit=Flows.MONTH;
	  Flows.gfp_accrual_shift_conv=Dates.ModifiedFollowing;
	  Flows.gfp_accrual_basis=Dates.DC_ACT_360;
	  Flows.gfp_accrual_hols="nyc";
	  Flows.gfp_payment_delay=0;
	  Flows.gfp_payment_shift_conv=Dates.ModifiedFollowing;
	  Flows.gfp_payment_basis=Dates.DC_ACT_360;
	  Flows.gfp_payment_hols="nyc"}
      ) 
      (Deals.make_floating_leg 
	 {Flows.gfp_start=day_after_tomorrow;
	  Flows.gfp_end=CalendarLib.Date.add day_after_tomorrow (Flows.make_tenor Flows.YEAR 3); 
	  Flows.gfp_period=6;
	  Flows.gfp_unit=Flows.MONTH;
	  Flows.gfp_accrual_shift_conv=Dates.ModifiedFollowing;
	  Flows.gfp_accrual_basis=Dates.DC_ACT_360;
	  Flows.gfp_accrual_hols="nyc";
	  Flows.gfp_payment_delay=0;
	  Flows.gfp_payment_shift_conv=Dates.ModifiedFollowing;
	  Flows.gfp_payment_basis=Dates.DC_ACT_360;
	  Flows.gfp_payment_hols="nyc"}
      )
  and four_year_swap = 
    Deals.make_vanilla_swap 
      (Deals.make_fixed_leg  0.039800
	 {Flows.gfp_start=day_after_tomorrow;
	  Flows.gfp_end=CalendarLib.Date.add day_after_tomorrow (Flows.make_tenor Flows.YEAR 4); 
	  Flows.gfp_period=6;
	  Flows.gfp_unit=Flows.MONTH;
	  Flows.gfp_accrual_shift_conv=Dates.ModifiedFollowing;
	  Flows.gfp_accrual_basis=Dates.DC_ACT_360;
	  Flows.gfp_accrual_hols="nyc";
	  Flows.gfp_payment_delay=0;
	  Flows.gfp_payment_shift_conv=Dates.ModifiedFollowing;
	  Flows.gfp_payment_basis=Dates.DC_ACT_360;
	  Flows.gfp_payment_hols="nyc"}
      ) 
      (Deals.make_floating_leg 
	 {Flows.gfp_start=day_after_tomorrow;
	  Flows.gfp_end=CalendarLib.Date.add day_after_tomorrow (Flows.make_tenor Flows.YEAR 4); 
	  Flows.gfp_period=6;
	  Flows.gfp_unit=Flows.MONTH;
	  Flows.gfp_accrual_shift_conv=Dates.ModifiedFollowing;
	  Flows.gfp_accrual_basis=Dates.DC_ACT_360;
	  Flows.gfp_accrual_hols="nyc";
	  Flows.gfp_payment_delay=0;
	  Flows.gfp_payment_shift_conv=Dates.ModifiedFollowing;
	  Flows.gfp_payment_basis=Dates.DC_ACT_360;
	  Flows.gfp_payment_hols="nyc"}
      )
  and five_year_swap = 
    Deals.make_vanilla_swap 
      (Deals.make_fixed_leg  0.054900
	 {Flows.gfp_start=day_after_tomorrow;
	  Flows.gfp_end=CalendarLib.Date.add day_after_tomorrow (Flows.make_tenor Flows.YEAR 5); 
	  Flows.gfp_period=6;
	  Flows.gfp_unit=Flows.MONTH;
	  Flows.gfp_accrual_shift_conv=Dates.ModifiedFollowing;
	  Flows.gfp_accrual_basis=Dates.DC_ACT_360;
	  Flows.gfp_accrual_hols="nyc";
	  Flows.gfp_payment_delay=0;
	  Flows.gfp_payment_shift_conv=Dates.ModifiedFollowing;
	  Flows.gfp_payment_basis=Dates.DC_ACT_360;
	  Flows.gfp_payment_hols="nyc"}
      ) 
      (Deals.make_floating_leg 
	 {Flows.gfp_start=day_after_tomorrow;
	  Flows.gfp_end=CalendarLib.Date.add day_after_tomorrow (Flows.make_tenor Flows.YEAR 5); 
	  Flows.gfp_period=6;
	  Flows.gfp_unit=Flows.MONTH;
	  Flows.gfp_accrual_shift_conv=Dates.ModifiedFollowing;
	  Flows.gfp_accrual_basis=Dates.DC_ACT_360;
	  Flows.gfp_accrual_hols="nyc";
	  Flows.gfp_payment_delay=0;
	  Flows.gfp_payment_shift_conv=Dates.ModifiedFollowing;
	  Flows.gfp_payment_basis=Dates.DC_ACT_360;
	  Flows.gfp_payment_hols="nyc"}
      )
  in 
    let deposits = [overnight_deposit; tomorrow_next_deposit; one_month_deposit; two_month_deposit; three_month_deposit; six_month_deposit]
    and swaps = [one_year_swap; eighteen_month_swap; two_year_swap; three_year_swap; four_year_swap; five_year_swap]
    in
       let p = List.fold_left Curves.append_deposit c deposits 
       in 
         let q = List.fold_left Curves.append_vanilla_swap p swaps
	 in
           Printf.printf "----\n";
         Printf.printf "%s\n" (Curves.string_of_curve q)
;;