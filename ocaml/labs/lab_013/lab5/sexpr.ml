(*
 * sexpr.ml
 *
 *     S-expressions.
 *
 *)

type atom =
   | Atom_unit
   | Atom_bool of bool
   | Atom_int  of int
   | Atom_id   of string

type expr =
   | Expr_atom of atom
   | Expr_list of expr list


(* Convert an atom into a string. *)
let string_of_atom a =
   match a with
      | Atom_unit   -> "#u"
      | Atom_bool b -> if b then "#t" else "#f"
      | Atom_int  i -> string_of_int i
      | Atom_id   s -> s


(* Return a string of n spaces. *)
let spaces n = String.make n ' '


let string_of_expr sx =
   let rec iter_string_of_expr sx indent =
      begin
         match sx with
            | Expr_atom a -> 
                 let s = string_of_atom a in
                    Printf.sprintf "ATOM[%s]" s
            | Expr_list slist ->
                 "\n" 
                 ^ (spaces indent) 
                 ^ "LIST[ "
                 ^ (iter_string_of_expr_list slist (indent + 2))
                 ^ " ]"
      end
   and iter_string_of_expr_list slist indent =
      begin
         match slist with
            | [] -> ""
            | [s] -> iter_string_of_expr s indent
            | h :: t -> 
                 (iter_string_of_expr h indent)
                 ^ " "
                 ^ (iter_string_of_expr_list t indent)
      end
   in
      iter_string_of_expr sx 0


let string_of_expr2 sx =
   let rec iter_string_of_expr sx indent =
      begin
         match sx with
            | Expr_atom a -> 
                 let s = string_of_atom a in
                    Printf.sprintf "%s" s
            | Expr_list slist ->
                 "\n" 
                 ^ (spaces indent) 
                 ^ "("
                 ^ (iter_string_of_expr_list slist (indent + 2))
                 ^ ")"
      end
   and iter_string_of_expr_list slist indent =
      begin
         match slist with
            | [] -> ""
            | [s] -> iter_string_of_expr s indent
            | h :: t -> 
                 (iter_string_of_expr h indent)
                 ^ " "
                 ^ (iter_string_of_expr_list t indent)
      end
   in
      iter_string_of_expr sx 0



