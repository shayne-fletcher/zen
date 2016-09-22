type constant =
  | Pconst_int of int

type pattern = {
  ppat_desc : pattern_desc
}

and pattern_desc =
   | Ppat_any
   | Ppat_constant of constant
   | Ppat_var of string
   | Ppat_tuple of pattern list

and expression = {
   pexp_desc : expression_desc
}

and expression_desc = 
  | Pexp_ident of string
  | Pexp_construct of string * expression option
  | Pexp_constant of constant
  | Pexp_fun of pattern * expression
  | Pexp_apply of expression * expression list
  | Pexp_tuple of expression list
  | Pexp_get_n of int * expression

open Format

let line 
    (i : int) 
    (ppf : formatter) 
    (s : ('a, formatter, unit) format) : 'a =
  fprintf ppf "%s" (String.make ((2 * i) mod 72) ' ');
  fprintf ppf s

let list 
    (i : int)
    (f : int -> formatter -> 'a -> unit)
    (ppf : formatter)
    (l : 'a list) : unit =
  match l with
  | [] -> line i ppf "[]\n";
  | _ :: _ ->
     line i ppf "[\n";
     List.iter (f (i + 1) ppf) l;
     line i ppf "]\n"

let option 
    (i : int) 
    (f : int -> formatter -> 'a -> unit) 
    (ppf : formatter)
    (x : 'a option) : unit =
  match x with
  | None -> line i ppf "None\n";
  | Some x ->
      line i ppf "Some\n";
      f (i + 1) ppf x

let fmt_ident (ppf : formatter) (x : string) : unit =
  fprintf ppf "\"%s\"" x

let fmt_constant (ppf : formatter) (x : constant) : unit =
  match x with
  | Pconst_int i -> fprintf ppf "Pconst_int (%d)" i

let rec pattern (i : int) (ppf : formatter) (x : pattern) : unit =
  line i ppf "pattern\n";
  let i = i + 1 in
  match x.ppat_desc with
  | Ppat_any -> line i ppf "Ppat_any\n";
  | Ppat_var s -> line i ppf "Ppat_var %a\n" fmt_ident s;
  | Ppat_constant c -> line i ppf "Ppat_constant %a\n" fmt_constant c;
  | Ppat_tuple l ->
      line i ppf "Ppat_tuple\n";
      list i pattern ppf l;

and expression (i : int) (ppf : formatter) (x : expression) : unit  =
  line i ppf "expression\n";
  let i = i + 1 in
  match x.pexp_desc with
  | Pexp_ident li -> line i ppf "Pexp_ident %a\n" fmt_ident li
  | Pexp_constant c -> line i ppf "Pexp_constant %a\n" fmt_constant c
  | Pexp_fun (x, e) ->
    line i ppf "Pexp_fun\n";
    pattern i ppf x;
    expression i ppf e
  | Pexp_apply (e, l) ->
     line i ppf "Pexp_apply\n";
     expression i ppf e;
     list i x_expression ppf l
  | Pexp_construct (li, eo) ->
      line i ppf "Pexp_construct %a\n" fmt_ident li;
      option i expression ppf eo
  | Pexp_tuple (l) ->
      line i ppf "Pexp_tuple\n";
      list i expression ppf l

and x_expression (i : int) (ppf : formatter) (e : expression) : unit =
  line i ppf "<arg>\n";
  expression (i + 1) ppf e

let string_of_pattern (p : pattern) : string =
  pattern 0 (str_formatter) p;
  flush_str_formatter ()

let string_of_expression (e : expression) : string =
  expression 0 (str_formatter) e;
  flush_str_formatter ()

let mk_pat (p : pattern_desc) : pattern = {ppat_desc = p}
let mk_pvar (i : string) : pattern = mk_pat (Ppat_var i)
let mk_pconst (i : int) : pattern = mk_pat (Ppat_constant (Pconst_int i))
let mk_ptuple (l : pattern list) : pattern = mk_pat (Ppat_tuple l)

let mk_exp (e : expression_desc) : expression = {pexp_desc = e}
let mk_ident (i : string) : expression = mk_exp (Pexp_ident i)
let mk_tuple (l : expression list) : expression = mk_exp (Pexp_tuple l)
let mk_const (c : int) : expression = mk_exp (Pexp_constant (Pconst_int c))
let mk_get_n ((i, e):(int * expression_desc)) : expression = mk_exp (Pexp_get_n (i, mk_exp e))
let mk_app ((op, args) : (expression * expression list)) : expression = mk_exp (Pexp_apply (op, args))
let mk_fun ((x, e) : (pattern * expression)) : expression = mk_exp (Pexp_fun (x, e))
let mk_constructor ((c, arg) : string * expression option) : expression = mk_exp (Pexp_construct (c, arg))

module Detail = struct
  let gen_var () =
    let counter = ref (-1) in
    let next () =
      counter := !counter + 1;
      "_" ^ (string_of_int (!counter)) in
    let reset () = counter := (-1)
    in (next, reset)
end

let (fresh_var_get, fresh_var_reset) = Detail.gen_var ()

let rec translate_pat ({ppat_desc = p} as pat : pattern) : pattern =
  match p with
  | Ppat_any | Ppat_constant _ | Ppat_var _ | Ppat_tuple _ -> pat

let rec translate_exp ({pexp_desc = e} as exp : expression) : expression =
  match e with
  | Pexp_constant _ | Pexp_ident _ -> exp
  | Pexp_apply (e, l) -> 
     mk_app (translate_exp e, (List.map translate_exp l))
  | Pexp_fun (p, e) -> translate_def exp
  | _ -> assert false

 and translate_def ({pexp_desc = d} : expression) : expression =
    match d with
    | Pexp_fun (p, e) -> 
      let ident = fresh_var_get () in
       mk_fun (
         mk_pvar ident, 
         mk_app (mk_fun (translate_pat p, translate_exp e), [mk_ident ident])
       )
    | _ -> assert false


(*fun x -> x + 1*)
let f = mk_fun (mk_pvar "x", mk_app (mk_ident "+", [mk_ident "x"; mk_const 1]))

(*fun _0 -> (fun x -> x + 1) _0*)
let () = Printf.printf "%s\n" (string_of_expression (translate_exp f))

(*The type of extraction operations to drill down to variables in a
  pattern*)
type extract_t =
| Extract_proj_n of int (*tuple projection*)
| Extract_expr of expression

(*To reach a leaf, apply a list of extractions in turn*)
type extractor_t = extract_t list
(*An association list of variables in an expression and their
  extractors*)
type extractor_tbl_t = (string * extractor_t) list
 
(*[gen_pattern_vars p] associates each variable in [p] with an
  extractor*)
let gen_pattern_vars (p : pattern) : extractor_tbl_t =
  let rec gen_pattern_vars_rec 
      (vars : extractor_tbl_t)
      (extractor : extractor_t) 
      ({ppat_desc=q} : pattern) : extractor_tbl_t =
    match q with
    | Ppat_var ident -> (ident, extractor) :: vars
    | Ppat_tuple pats ->
      let f (i, acc) pat =
        let extractor' = (Extract_proj_n i) :: extractor in
        (i + 1, gen_pattern_vars_rec vars extractor' pat @ acc) in
      snd (List.fold_left f (0, vars) pats)
    | Ppat_constant _ | Ppat_any -> vars 
  in gen_pattern_vars_rec [] [] p

(*[gen_extractor extractor exp] produces an expression from [exp]
  built from the operations of [extractor] applied from right to left*)
let gen_extractor 
    (extractor : extract_t list) 
    (exp : expression) : expression =
  let f ex acc =
    match ex with
    | Extract_proj_n n -> mk_exp (Pexp_get_n (n, acc))
    | Extract_expr e -> e
  in List.fold_right f extractor exp

(*(a, b, (c, d))*)
let x = mk_ptuple 
  [mk_pvar "a"; mk_pvar "b"; 
   mk_ptuple [mk_pvar "c"; mk_pvar "d"]]
let res = gen_pattern_vars x

let y = mk_tuple 
  [mk_const 1; mk_const 2; 
   mk_tuple [mk_const 3; mk_const 4]]
(*An exression for "d" in y*)
let z = gen_extractor (List.assoc "d" res) y

(*[gen_match_check p e] assumes that "structurally", [p] and [e] are
  compatible and generates an expression from them that can be used to
  check on evaluation of [e] that it satisfies the constants in [p]*)
let rec gen_match_check {ppat_desc=p} {pexp_desc=e} : expression =
  let ap12 op a b =  mk_app (mk_ident op, [a; b]) in
  match p with
  | Ppat_var _ -> mk_constructor ("true", None)
  | Ppat_constant (Pconst_int c) -> ap12 "eq" (mk_const c) (mk_exp e)
  | Ppat_tuple [] -> ap12 "eq" (mk_tuple []) (mk_exp e)
  | Ppat_tuple ps ->
    let f (i, acc) p = (i + 1,
      ap12 "and" acc (gen_match_check p (mk_get_n (i, e)))) in
    snd (List.fold_left f 
           (1, gen_match_check (List.hd ps) (mk_get_n (0, e))) (List.tl ps))
  | _ -> failwith "Not implemented"

(*((1, 2) <- ((1 + 2, 1 + 2) can should be expected to yield [false]
  on evaluation*)
let y : expression = mk_app (mk_ident "+", [mk_const 1; mk_const 2])
let z = gen_match_check (mk_ptuple [mk_pconst 1; mk_pconst 2]) (mk_tuple [y; y])
