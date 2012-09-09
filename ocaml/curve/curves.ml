type curve = 
  {
    curve_dates : CalendarLib.Date.t list ;
    curve_abscissae : float list ;
    curve_ordinates : float list ;
    curve_interpolation : float list -> float list -> float -> float
  }
;;

let string_of_curve : curve -> string =
  fun crv ->
    let string_of_list f l = "[" ^ String.concat ";" (List.map f l) ^ "]" in
    "{curve_dates="^(string_of_list CalendarLib.Printer.Date.to_string crv.curve_dates)^
    ", curve_abscissae="^(string_of_list string_of_float crv.curve_abscissae)^
    ", curve_ordinates="^(string_of_list string_of_float crv.curve_ordinates)^", interpolation=<func>}"
;;

let base_date : curve -> CalendarLib.Date.t =
  fun c ->
    List.hd c.curve_dates
;;

let append : curve -> CalendarLib.Date.t -> float -> float -> curve =
  fun crv mat x y ->
    {curve_dates=crv.curve_dates@[mat] ; 
     curve_abscissae=crv.curve_abscissae@[x] ; 
     curve_ordinates=crv.curve_ordinates@[y] ;
     curve_interpolation=crv.curve_interpolation
    }
;;

let find_index : (float -> bool) -> float list -> int option =
  fun pred xs ->
    let rec loop i xs =
      match xs with
      | h::t -> if (pred h) then (Some i) else (loop (i+1) t)
      | _ -> None
    in
    loop 0 xs
;;

(*
let l = [1.0; 2.0; 3.0] ;;
let i = find_index ((=) 2.0) l ;;
*)

let evaluate : curve -> float -> float = 
  fun crv x ->
    let xs = crv.curve_abscissae
    and ys = crv.curve_ordinates
    and interpolate = crv.curve_interpolation
    in 
      let index = find_index ((=) x) xs
      in match index with
         | Some i -> (List.nth ys i)
         | _ -> interpolate xs ys x
;;

let append_deposit : curve -> Deals.cash -> curve =
  fun crv dep ->
    let t = base_date crv
    and r = dep.Deals.cash_coupon 
    and start = dep.Deals.cash_flow.Flows.flow_start
    and mat = dep.Deals.cash_flow.Flows.flow_end
    and accrual = dep.Deals.cash_flow.Flows.flow_accrual
    in
      let s = Dates.year_diff start t
      and off = Dates.year_diff mat t
      in 
        let ps = evaluate crv s
	in append crv mat off (ps/.(1.0+.r*.accrual))
;;

let append_deposit : curve -> Deals.cash -> curve =
  fun crv dep ->
    let t = base_date crv
    and r = dep.Deals.cash_coupon 
    and start = dep.Deals.cash_flow.Flows.flow_start
    and mat = dep.Deals.cash_flow.Flows.flow_end
    and accrual = dep.Deals.cash_flow.Flows.flow_accrual
    in
      let s = Dates.year_diff start t
      and off = Dates.year_diff mat t
      in 
        let ps = evaluate crv s
	in append crv mat off (ps/.(1.0+.r*.accrual))
;;

let value_swap : curve -> Deals.vanilla_swap -> float =
  fun crv swap ->
    let t = base_date crv
    and fixed_leg = swap.Deals.vanilla_swap_fixed_leg
    and floating_leg = swap.Deals.vanilla_swap_floating_leg
    in
       let fixed_leg_coupon = fixed_leg.Deals.fixed_leg_coupon
       in 
         (* Assumes swap convention *)
         let value_flow : Flows.flow -> float = 
   	   fun flo ->
	      let s = flo.Flows.flow_start
	      and e = flo.Flows.flow_end
	      and pay = flo.Flows.flow_pay
	      in
  	        let ps = evaluate crv (Dates.year_diff s t)
	        and pe = evaluate crv (Dates.year_diff e t)
	        and disc = evaluate crv (Dates.year_diff pay t)
	        in 
     	          let libor = (ps/.pe -. 1.0)
		  in  disc*.libor
	 in 
  	   let floating_leg_value = List.fold_left (+.) 0.0 (List.map value_flow floating_leg.Deals.floating_leg_flows)
  	   in
  	     let value_flow : Flows.flow -> float =
	       fun flo ->
 		 let pay = flo.Flows.flow_pay
		 and acc = flo.Flows.flow_accrual
		 in
  		   let disc = evaluate crv (Dates.year_diff pay t)
		   in
		     disc*.fixed_leg_coupon*.acc
	     in
  	        let fixed_leg_value = List.fold_left (+.) 0.0 (List.map value_flow fixed_leg.Deals.fixed_leg_flows)
	        in
	          floating_leg_value -. fixed_leg_value
;;

let swap_maturity : CalendarLib.Date.t -> Deals.vanilla_swap -> (CalendarLib.Date.t * float) =
  fun today swap ->
    let fixed_leg = swap.Deals.vanilla_swap_fixed_leg
    in
    let fixed_leg_flows = fixed_leg.Deals.fixed_leg_flows
    in 
      let last_flow = List.nth fixed_leg_flows (List.length fixed_leg_flows - 1)
      in
        let last_flow_pay = last_flow.Flows.flow_pay
	in (last_flow_pay, (Dates.year_diff last_flow_pay today))
;;
      
let append_vanilla_swap : curve -> Deals.vanilla_swap -> curve =
  fun crv swap ->
    let today = base_date crv 
    in 
    let(mat, off) = swap_maturity today swap
    in 
    let pe = Roots.bisect (fun y -> value_swap (append crv mat off y) swap) (0.0,1.0) 1.0e-6
    in append crv mat off pe
;;
