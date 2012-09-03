(** Deal interface *)
module type S = 
sig

  (**{2 Cash}*)

  (** Cash deposit type *)
  type cash =
    {
      cash_coupon : float ;
      cash_flow : Flows.flow ;
    }

  (** Make a string representation of a cash *)
  val string_of_cash : cash -> string
  (** Construct a cash *)
  val make_cash : float -> Flows.gen_flows_param_pack -> cash

  (**{2 Swaps}*)

  (** Fixed leg type *)
  type fixed_leg =
  {
    fixed_leg_coupon : float ;
    fixed_leg_flows : Flows.flow list
  }

  (** Make a string representation of a fixed leg *)
  val string_of_fixed_leg : fixed_leg -> string
  (** Construct a fixed leg *)
  val make_fixed_leg : float -> Flows.gen_flows_param_pack -> fixed_leg

  (** Floating leg type *)
  type floating_leg =
  {
    floating_leg_flows : Flows.flow list
  }

  (** Make a string representation of a floating leg *)
  val string_of_floating_leg : floating_leg -> string
  val make_floating_leg : Flows.gen_flows_param_pack -> floating_leg

  (** Vanilla swap type *)
  type vanilla_swap =
  {
    vanilla_swap_fixed_leg : fixed_leg ;
    vanilla_swap_floating_leg : floating_leg
  }

  (** Make a string representation of a floating leg *)
  val string_of_vanilla_swap: vanilla_swap -> string

  (** Construct a vanilla swap *)
  val make_vanilla_swap : fixed_leg -> floating_leg -> vanilla_swap

end
