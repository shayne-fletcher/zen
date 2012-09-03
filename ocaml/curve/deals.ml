type cash =
  {
    cash_coupon : float ;
    cash_flow : Flows.flow ;
  }
;;

let string_of_cash : cash -> string =
  fun dep ->
     "{cash_coupon="^(string_of_float dep.cash_coupon) ^
    ", cash_flow="^(Flows.string_of_flow dep.cash_flow)^
    "}"
    ;;

let make_cash : float -> Flows.gen_flows_param_pack -> cash =
  fun r s ->
    {cash_coupon=r; cash_flow=(List.hd (Flows.gen_flows s))}
;;

type fixed_leg =
{
  fixed_leg_coupon : float ;
  fixed_leg_flows : Flows.flow list
}
;;

let string_of_fixed_leg : fixed_leg -> string = 
  fun leg ->
    let string_of_flow_list l = "[" ^ String.concat ";\n" (List.map Flows.string_of_flow l) ^ "]"
    in "{fixed_leg_coupon="^(string_of_float leg.fixed_leg_coupon)^",fixed_leg_flows="^(string_of_flow_list leg.fixed_leg_flows)^"}"
;;

let make_fixed_leg : float -> Flows.gen_flows_param_pack -> fixed_leg = 
  fun r s ->
    {fixed_leg_coupon = r ; fixed_leg_flows = (Flows.gen_flows s)}
;;

type floating_leg =
{
  floating_leg_flows : Flows.flow list
}
;;

let string_of_floating_leg : floating_leg -> string = 
  fun leg ->
    let string_of_flow_list l = "[" ^ String.concat ";\n" (List.map Flows.string_of_flow l) ^ "]"
    in "{floating_leg_flows="^(string_of_flow_list leg.floating_leg_flows)^"}"
;;

let make_floating_leg : Flows.gen_flows_param_pack -> floating_leg = 
  fun s ->
    {floating_leg_flows = (Flows.gen_flows s)}
;;

type vanilla_swap =
{
  vanilla_swap_fixed_leg : fixed_leg ;
  vanilla_swap_floating_leg : floating_leg
}
;;

let string_of_vanilla_swap : vanilla_swap -> string = 
  fun swap ->
      "{vanilla_swap_fixed_leg="^(string_of_fixed_leg swap.vanilla_swap_fixed_leg)^
      ",vanilla_swap_floating_leg="^(string_of_floating_leg swap.vanilla_swap_floating_leg)^"}"
;;

let make_vanilla_swap : fixed_leg -> floating_leg -> vanilla_swap =
  fun l r -> {vanilla_swap_fixed_leg=l; vanilla_swap_floating_leg=r}
;;
  
(* Test *)

let t = CalendarLib.Date.make 2004 08 24
in 
  let tomorrow= CalendarLib.Date.add t (Flows.make_tenor Flows.DAY 1)
  in
    let day_after_tomorrow = CalendarLib.Date.add tomorrow (Flows.make_tenor Flows.DAY 1)
    in
    let overnight_deposit = 
        make_cash 0.00040
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
        make_cash 0.00040
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
        make_cash 0.00040
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
        make_cash 0.00054
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
        make_cash 0.00054
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
        make_cash 0.00066
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
    in
      let string_of_cash_list l = "[" ^ String.concat ";\n" (List.map string_of_cash l) ^ "]" 
      in 
        Printf.printf "----\n";
        Printf.printf "Deposits:\n";
        Printf.printf "%s\n" (
	  string_of_cash_list [overnight_deposit; tomorrow_next_deposit ; 
			       one_month_deposit; two_month_deposit; three_month_deposit; six_month_deposit]
	)
      ;;
