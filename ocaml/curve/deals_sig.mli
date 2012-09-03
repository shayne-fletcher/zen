(** Deal interface *)
module type S = 
sig

  type cash =
    {
      cash_coupon : float ;
      cash_flow : Flows.flow ;
    }

  val string_of_cash : cash -> string
  val make_cash : float -> Flows.gen_flows_param_pack -> cash

  type fixed_leg =
  {
    fixed_leg_coupon : float ;
    fixed_leg_flows : Flows.flow list
  }

  val make_fixed_leg : float -> Flows.gen_flows_param_pack -> fixed_leg

  type floating_leg =
  {
    floating_leg_flows : Flows.flow list
  }

  val string_of_floating_leg : floating_leg -> string

  type vanilla_swap =
  {
    vanilla_swap_fixed_leg : fixed_leg ;
    vanilla_swap_floating_leg : floating_leg
  }

  val make_vanilla_swap : fixed_leg -> floating_leg -> vanilla_swap

end
