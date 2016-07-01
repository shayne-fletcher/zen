(***************************************************************************)
(*  Copyright (C) 2000-2016 LexiFi SAS. All rights reserved.               *)
(*                                                                         *)
(*  No part of this document may be reproduced or transmitted in any       *)
(*  form or for any purpose without the express permission of LexiFi SAS.  *)
(***************************************************************************)

(* $Id: transldynamic.mli 87430 2016-01-10 21:54:06Z jmeber $ *)

open Typedtree
open Lambda

val set_module_name: string -> unit

val transl_use_ttype: expression -> expression -> lambda

val transl_use_ttype_toplevel: expression -> Ident.t * lambda

val transl_use_ttype_cont: expression -> (unit -> 'a) -> Ident.t * lambda * 'a

val transl_typeof: expression -> lambda

val illegal_dyn_use: Location.t -> 'a

val add_mlfi_specific_to_module: lambda -> lambda

(* Forward declaration -- to be filled in by Translcore.transl_expr *)
val transl_exp_forward: (expression -> lambda) ref
