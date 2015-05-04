(*
 * ast.ml
 *
 *     Abstract syntax tree.
 *
 *)

type id = string

type expr =
   | Expr_unit
   | Expr_bool   of bool
   | Expr_int    of int
   | Expr_id     of id
   | Expr_define of id * expr
   | Expr_if     of expr * expr * expr
   | Expr_lambda of id list * expr list
   | Expr_apply  of expr * expr list

let rec ast_of_sexpr (sx : Sexpr.expr) : expr =
  let raise_syntax_error () = failwith "Syntax error" in
  match sx with
  | Sexpr.Expr_atom a -> 
    begin
      match a with
      | Sexpr.Atom_unit -> Expr_unit
      | Sexpr.Atom_bool b -> Expr_bool b
      | Sexpr.Atom_int i -> Expr_int i
      | Sexpr.Atom_id s -> Expr_id s
    end
  | Sexpr.Expr_list el -> 
    begin
      match el with
      | [Sexpr.Expr_atom (Sexpr.Atom_id t); cond; then_; else_] when t  = "if" ->
        Expr_if (ast_of_sexpr cond, ast_of_sexpr then_, ast_of_sexpr else_)
      | [Sexpr.Expr_atom (Sexpr.Atom_id t); Sexpr.Expr_atom (Sexpr.Atom_id tag); body] when t = "define" ->
        Expr_define (tag, ast_of_sexpr body)
      | Sexpr.Expr_atom (Sexpr.Atom_id t) :: Sexpr.Expr_list arglist :: body when t = "lambda" -> 
        Expr_lambda (List.map (function | Sexpr.Expr_atom (Sexpr.Atom_id s) -> s | _ -> raise_syntax_error ()) arglist, (List.map ast_of_sexpr body))
      | func :: args -> Expr_apply ((ast_of_sexpr func), (List.map ast_of_sexpr args))
      | _ -> raise_syntax_error ()
    end

let string_of_ast ast =
   let sprintf  = Printf.sprintf in  (* to make the code cleaner *)
   let spaces n = String.make n ' ' in
   let rec string_of_ids id_lst = 
      match id_lst with
         | [] -> ""
         | [id] -> id
         | h :: t -> h ^ " " ^ (string_of_ids t)
   in

   let rec iter ast indent =
      let string_of_exprs e_list =
         (List.fold_left (^) ""
             (List.map
                 (fun e -> "\n" ^ iter e (indent + 2))
                 e_list))
      in
      match ast with
         | Expr_unit    -> sprintf "%sUNIT"       (spaces indent) 
         | Expr_bool b  -> sprintf "%sBOOL[ %b ]" (spaces indent) b
         | Expr_int  i  -> sprintf "%sINT[ %d ]"  (spaces indent) i
         | Expr_id   id -> sprintf "%sID[ %s ]"   (spaces indent) id
         | Expr_define (id, e) -> 
              sprintf "%sDEFINE[%s\n%s ]" 
                 (spaces indent) id (iter e (indent + 2))
         | Expr_if (test_clause, then_clause, else_clause) ->
              sprintf "%sIF[\n%s\n%s\n%s ]"
                 (spaces indent) 
                 (iter test_clause (indent + 2))
                 (iter then_clause (indent + 2))
                 (iter else_clause (indent + 2))
         | Expr_lambda (ids, body) ->
              sprintf "%sLAMBDA[(%s)%s ]"
                 (spaces indent)
                 (string_of_ids ids)
                 (string_of_exprs body)
         | Expr_apply (operator, operands) ->
              sprintf "%sAPPLY[\n%s%s ]"
                 (spaces indent)
                 (iter operator (indent + 2))
                 (string_of_exprs operands)
   in
      "\n" ^ iter ast 0 ^ "\n"


let ast_test infile =
   let lexbuf = Lexing.from_channel infile in
   let rec loop () =
      let sexpr  = Parser.parse Lexer.lex lexbuf in
         match sexpr with
            | None -> ()
            | Some s ->
                 let expr = ast_of_sexpr s in
                    Printf.printf "%s\n" (string_of_ast expr); 
                    flush stdout;
                    loop ()
   in
      loop ()


