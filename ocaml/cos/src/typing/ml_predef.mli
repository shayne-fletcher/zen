open Ml_types

val type_int: type_expr
val type_bool: type_expr
val type_unit: type_expr
val type_list: type_expr -> type_expr

val path_int: Ml_path.t
val path_bool: Ml_path.t
val path_unit: Ml_path.t
val path_list: Ml_path.t

(* To build the initial environment, since there is a nasty mutual
   recursion between predef and env, we break it by parameterizing
   over [Ml_env.t] and [Ml_env.add_type] *)

val build_initial_env: (Ml_ident.t -> type_declaration -> 'a -> 'a) -> 'a -> 'a
