type cash =
  {
    cash_coupon : float ;
    cash_flow : Flows.flow ;
  }
;;

(* To parse a cash you read off a coupon and a flow param
   pack. Generate the flows from the pack and pick off the head. *)
let rec parse_cash = parser
  [< 'Genlex.Kwd "{" ; 
     coupon= (parser [< 'Genlex.Float f>] -> f) ; 'Genlex.Kwd ";" ; 
     args = Flows.parse_gen_flows_param_pack ; 
     'Genlex.Kwd "}" 
  >] -> {cash_coupon=coupon; cash_flow=(List.hd (Flows.gen_flows args))}
;;

let string_of_cash : cash -> string =
  fun dep ->
    "{"^(string_of_float dep.cash_coupon) ^";"^(Flows.string_of_flow dep.cash_flow)^"}"
;;

let cash_of_string : string -> cash =
  fun dep ->
    let lexer = Genlex.make_lexer ["{"; ";" ; "}"]in parse_cash (lexer (Stream.of_string dep))
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
    in "{"^(string_of_float leg.fixed_leg_coupon)^";"^(string_of_flow_list leg.fixed_leg_flows)^"}"
;;

(* To parse a fixed_leg you read off a flow param pack. Generate
   the flows from the pack. *)
let rec parse_fixed_leg = parser
  [< 'Genlex.Kwd "{" ; 
     r = (parser [<'Genlex.Float f>] -> f) ; 'Genlex.Kwd ";" ;
     args = Flows.parse_gen_flows_param_pack ; 
     'Genlex.Kwd "}" 
  >] -> {fixed_leg_coupon=r;fixed_leg_flows=(Flows.gen_flows args)}
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
    "{{"^((fun l -> "[" ^ 
      String.concat ";" (List.map Flows.string_of_flow l) ^ "]") leg.floating_leg_flows)^"}}"
;;

(* To parse a floating_leg you read off a flow param pack. Generate
   the flows from the pack. *)
let rec parse_floating_leg = parser
  [< 'Genlex.Kwd "{" ; 
     args = Flows.parse_gen_flows_param_pack ; 
     'Genlex.Kwd "}" 
  >] -> {floating_leg_flows=(Flows.gen_flows args)}
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
    "{"^(string_of_fixed_leg swap.vanilla_swap_fixed_leg)^
      ";"^(string_of_floating_leg swap.vanilla_swap_floating_leg)^"}"
;;

let make_vanilla_swap : fixed_leg -> floating_leg -> vanilla_swap =
  fun l r -> {vanilla_swap_fixed_leg=l; vanilla_swap_floating_leg=r}
;;

(* To parse a vanilla_swap you read off a fixed_leg and a
   floating_leg. *)
let rec parse_vanilla_swap = parser
  [< 'Genlex.Kwd "{" ; 
     l = parse_fixed_leg ; 'Genlex.Kwd ";" ;
     o = parse_floating_leg  ;
     'Genlex.Kwd "}"
  >] -> {vanilla_swap_fixed_leg=l;vanilla_swap_floating_leg=o}
;;
