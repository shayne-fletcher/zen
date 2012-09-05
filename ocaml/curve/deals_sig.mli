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

  (** Construct a cash *)
  val make_cash : float -> Flows.gen_flows_param_pack -> cash

  (** Make a string representation of a cash *)
  val string_of_cash : cash -> string

  (** Parse a cash from a string *)
  val cash_of_string : string -> cash

  (** Parse a cash from a token stream *)

  val parse_cash : Genlex.token Stream.t -> cash

  (**{2 Swaps}*)

  (** Fixed leg type *)
  type fixed_leg =
  {
    fixed_leg_coupon : float ;
    fixed_leg_flows : Flows.flow list
  }

  (** To parse a fixed_leg you read off a flow param pack. Generate
   the flows from the pack. *)
  val parse_fixed_leg : Genlex.token Stream.t -> fixed_leg

  (** Make a string representation of a fixed leg *)
  val string_of_fixed_leg : fixed_leg -> string

  (** Construct a fixed leg *)
  val make_fixed_leg : float -> Flows.gen_flows_param_pack -> fixed_leg

  (** Floating leg type *)
  type floating_leg =
  {
    floating_leg_flows : Flows.flow list
  }

  (** To parse a floating_leg you read off a flow param pack. Generate
   the flows from the pack. *)
  val parse_floating_leg : Genlex.token Stream.t -> floating_leg

  (** Make a string representation of a floating leg *)
  val string_of_floating_leg : floating_leg -> string

  (** Basic constructor *)
  val make_floating_leg : Flows.gen_flows_param_pack -> floating_leg

  (** Vanilla swap type *)
  type vanilla_swap =
  {
    vanilla_swap_fixed_leg : fixed_leg ;
    vanilla_swap_floating_leg : floating_leg
  }

  (** To parse a vanilla_swap you read off a fixed_leg and a
   floating_leg. *)
  val parse_vanilla_swap : Genlex.token Stream.t -> vanilla_swap

  (** Make a string representation of a floating leg *)
  val string_of_vanilla_swap: vanilla_swap -> string

  (** Construct a vanilla swap *)
  val make_vanilla_swap : fixed_leg -> floating_leg -> vanilla_swap

end
