(* Requires Calendar - http://calendar.forge.ocamlcore.org/ *)

module U = (* Local expressions *)
struct

  (* Parse a list *)
  let rec parse_list : (Genlex.token Stream.t -> 'a) -> Genlex.token Stream.t -> 'a list =
    fun f ->
      let rec list = parser | [< e = parse_top >] -> e
      and parse_top = parser [< 'Genlex.Kwd "[" ; h = f ; t = parse_more ; 'Genlex.Kwd "]" >] -> h::t
      and parse_more = parser | [< 'Genlex.Kwd ";" ; h = f ; t = parse_more >] -> h::t | [< >] -> []
      in list
  ;;

  (* Zip two lists (possibly unequal lengths) into a tuple *)
  let rec zip lst1 lst2 = match lst1,lst2 with
    | [],_ -> []
    | _, []-> []
    | (x::xs),(y::ys) -> (x,y) :: (zip xs ys)
  ;;

  (* Pretty-print a curve fancy*)
  let pprint_curve_fancy : Curves.curve -> unit = 
  (*
    e.g.

    T             P(T)
    ========================
    2004-08-24    1.00000000
    2004-08-25    0.99999889
    2004-08-26    0.99999778
    2004-09-27    0.99996222
    2004-10-26    0.99990629
    2004-11-26    0.99985980
    2005-02-28    0.99965689
    2005-08-26    0.99922776
    2006-08-28    0.99693227
    2007-08-27    0.99198651
    2008-08-26    0.84374309
    2009-08-26    0.74179769

  *)
    fun crv ->
      let dates  = crv.Curves.curve_dates
      and ordinates = crv.Curves.curve_ordinates
      in let print_epoch =
	fun (x, y) ->
	  Printf.printf "%s    %.8f\n" (CalendarLib.Printer.Date.to_string x) y
	 in 
	 Printf.printf "T             P(T)\n" ;
	 Printf.printf "========================\n" ;
	 List.iter print_epoch (zip dates ordinates)
  ;;

  (* Pretty-print a curve simple*)
  let pprint_curve_simple : Curves.curve -> unit =
  (*
    e.g.

    {curve_dates=[2004-08-24;2004-08-25;2004-08-26;2004-09-27;2004-10-26;2004-11-26;
    2005-02-28;2005-08-26;2006-08-28;2007-08-27;2008-08-26;2009-08-26], curve_abscissae=
    [0.;0.0027397260274;0.00547945205479;0.0931506849315;0.172602739726;0.257534246575;
    0.515068493151;1.00547945205;2.01095890411;3.00821917808;4.00821917808;5.00821917808], 
    curve_ordinates=[1.;0.99999888889;0.999997777781;0.999962223569;0.999906286356;
    0.999859797129;0.99965689478;0.999227762222;0.996932268143;0.991986513138;
    0.843743085861;0.741797685623], interpolation=<func>}

  *)
    fun crv ->
      Printf.printf "%s\n" (Curves.string_of_curve crv) ;
      Printf.printf "--\n"
  ;;

end
;;

let test_parse () =

  (* Bootstrap from parse *)
  
  let def1 = (* Cash deposits O/N, T/N, 1m, 2m, 3m, 6m *)
    "[
       {0.00040 ; {2004 08 24 ; 2004 08 25 ; 1 ; DAY   ; MODIFIED_FOLLOWING ; DC_ACT_360 ; \"nyc\"; 0 ; MODIFIED_FOLLOWING ; DC_ACT_360; \"nyc\" }}
      ;{0.00040 ; {2004 08 25 ; 2004 08 26 ; 1 ; DAY   ; MODIFIED_FOLLOWING ; DC_ACT_360 ; \"nyc\"; 0 ; MODIFIED_FOLLOWING ; DC_ACT_360; \"nyc\" }}
      ;{0.00040 ; {2004 08 26 ; 2004 09 27 ; 1 ; MONTH ; MODIFIED_FOLLOWING ; DC_ACT_360 ; \"nyc\"; 0 ; MODIFIED_FOLLOWING ; DC_ACT_360; \"nyc\" }}
      ;{0.00054 ; {2004 08 26 ; 2004 10 26 ; 2 ; MONTH ; MODIFIED_FOLLOWING ; DC_ACT_360 ; \"nyc\"; 0 ; MODIFIED_FOLLOWING ; DC_ACT_360; \"nyc\" }}
      ;{0.00054 ; {2004 08 26 ; 2004 11 26 ; 3 ; MONTH ; MODIFIED_FOLLOWING ; DC_ACT_360 ; \"nyc\"; 0 ; MODIFIED_FOLLOWING ; DC_ACT_360; \"nyc\" }}
      ;{0.00066 ; {2004 08 26 ; 2005 02 28 ; 6 ; MONTH ; MODIFIED_FOLLOWING ; DC_ACT_360 ; \"nyc\"; 0 ; MODIFIED_FOLLOWING ; DC_ACT_360; \"nyc\" }}
    ]"
  and def2 = (* Vanilla swaps 1y, 2y, 3y, 4y, 5y *)
    "[
       { {0.0007600 ; {2004 08 26 ; 2005 08 26 ; 1 ; YEAR  ; MODIFIED_FOLLOWING ; DC_ACT_360 ; \"nyc\" ; 0 ; MODIFIED_FOLLOWING ; DC_ACT_360 ; \"nyc\"}}
     ;               {{2004 08 26 ; 2005 08 26 ; 1 ; YEAR  ; MODIFIED_FOLLOWING ; DC_ACT_360 ; \"nyc\" ; 0 ; MODIFIED_FOLLOWING ; DC_ACT_360 ; \"nyc\"}}}
     ; { {0.0015100 ; {2004 08 26 ; 2006 08 26 ; 6 ; MONTH ; MODIFIED_FOLLOWING ; DC_ACT_360 ; \"nyc\" ; 0 ; MODIFIED_FOLLOWING ; DC_ACT_360 ; \"nyc\"}}
     ;               {{2004 08 26 ; 2006 08 26 ; 6 ; MONTH ; MODIFIED_FOLLOWING ; DC_ACT_360 ; \"nyc\" ; 0 ; MODIFIED_FOLLOWING ; DC_ACT_360 ; \"nyc\"}}}
     ; { {0.0026400 ; {2004 08 26 ; 2007 08 26 ; 6 ; MONTH ; MODIFIED_FOLLOWING ; DC_ACT_360 ; \"nyc\" ; 0 ; MODIFIED_FOLLOWING ; DC_ACT_360 ; \"nyc\"}}
     ;               {{2004 08 26 ; 2007 08 26 ; 6 ; MONTH ; MODIFIED_FOLLOWING ; DC_ACT_360 ; \"nyc\" ; 0 ; MODIFIED_FOLLOWING ; DC_ACT_360 ; \"nyc\"}}}
     ; { {0.0398000 ; {2004 08 26 ; 2008 08 26 ; 6 ; MONTH ; MODIFIED_FOLLOWING ; DC_ACT_360 ; \"nyc\" ; 0 ; MODIFIED_FOLLOWING ; DC_ACT_360 ; \"nyc\"}}
     ; {              {2004 08 26 ; 2008 08 26 ; 6 ; MONTH ; MODIFIED_FOLLOWING ; DC_ACT_360 ; \"nyc\" ; 0 ; MODIFIED_FOLLOWING ; DC_ACT_360 ; \"nyc\"}}}
     ; { {0.0549000 ; {2004 08 26 ; 2009 08 26 ; 6 ; MONTH ; MODIFIED_FOLLOWING ; DC_ACT_360 ; \"nyc\" ; 0 ; MODIFIED_FOLLOWING ; DC_ACT_360 ; \"nyc\"}}
     ; {              {2004 08 26 ; 2009 08 26 ; 6 ; MONTH ; MODIFIED_FOLLOWING ; DC_ACT_360 ; \"nyc\" ; 0 ; MODIFIED_FOLLOWING ; DC_ACT_360 ; \"nyc\"}}}
  
  ]"
  in let lexer = Genlex.make_lexer ["[";"]";"{";"}";";"] in
     let deposits = (U.parse_list Deals.parse_cash) (lexer (Stream.of_string def1))
     and swaps = (U.parse_list Deals.parse_vanilla_swap (lexer (Stream.of_string def2)))
     and t = CalendarLib.Date.make 2004 08 24 in
     let c={Curves.curve_dates=[t];
            Curves.curve_abscissae=[0.0];
            Curves.curve_ordinates=[1.0];
            Curves.curve_interpolation=Interpolation.loglinear_interpolation
           }
     in let c1 = List.fold_left Curves.append_deposit c deposits in
        let c2 = List.fold_left Curves.append_vanilla_swap c1 swaps in
        U.pprint_curve_fancy c2 ;
        print_newline ()
  ;;

let test_basic () =

  (* Construct the same curve as above, this time by hand. *)

  let t = CalendarLib.Date.make 2004 08 24 in
  let tomorrow = CalendarLib.Date.add t (Flows.make_tenor Flows.DAY 1) in
  let day_after_tomorrow = CalendarLib.Date.add tomorrow (Flows.make_tenor Flows.DAY 1) in 
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
        (Deals.make_fixed_leg 0.0007600
           {Flows.gfp_start=day_after_tomorrow;
            Flows.gfp_end=Dates.shift (CalendarLib.Date.add day_after_tomorrow (Flows.make_tenor Flows.YEAR 1)) Dates.ModifiedFollowing "nyc";
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
            Flows.gfp_end=Dates.shift (CalendarLib.Date.add day_after_tomorrow (Flows.make_tenor Flows.YEAR 1)) Dates.ModifiedFollowing "nyc";
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
    and two_year_swap =
      Deals.make_vanilla_swap
        (Deals.make_fixed_leg 0.0015100
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
        (Deals.make_fixed_leg 0.0026400
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
        (Deals.make_fixed_leg 0.039800
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
        (Deals.make_fixed_leg 0.054900
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
      and swaps = [one_year_swap; two_year_swap; three_year_swap ; four_year_swap ; five_year_swap]
      in
         let p = List.fold_left Curves.append_deposit c deposits in
         let q = List.fold_left Curves.append_vanilla_swap p swaps in
         U.pprint_curve_fancy q ; print_newline ()
  ;;
  
(* main*)

let version = ref false

let read_args () = 
  let specification = [("-v", Arg.Set version, "Print the version number")]
  in Arg.parse
  specification (fun s -> Printf.printf "warning : Ignoring unrecognized argument \"%s\"\n" s)
  "Usage : curves [opt]"
;;

let _ =
  read_args() ;
  if !version then
    Printf.printf "1.0.0\n"
  else
    (
      test_basic () ; test_parse ()
    )
;;
