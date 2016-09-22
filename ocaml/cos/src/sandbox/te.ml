type constant =
  | Pconst_int of int

type pattern = {
  ppat_desc : pattern_desc
}

and pattern_desc =
   | Ppat_any
   | Ppat_constant of constant
   | Ppat_var of string

and expression = {
   pexp_desc : expression_desc
}

and expression_desc = 
  | Pexp_ident of string
  | Pexp_constant of constant
  | Pexp_patfun of pattern * expression
  | Pexp_fun of expression * expression
  | Pexp_apply of expression * expression list

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

and expression (i : int) (ppf : formatter) (x : expression) : unit  =
  line i ppf "expression\n";
  let i = i + 1 in
  match x.pexp_desc with
  | Pexp_ident li -> line i ppf "Pexp_ident %a\n" fmt_ident li
  | Pexp_constant c -> line i ppf "Pexp_constant %a\n" fmt_constant c
  | Pexp_patfun (p, e) ->
    line i ppf "Pexp_patfun\n";
    pattern i ppf p;
    expression i ppf e
  | Pexp_fun (x, e) ->
    line i ppf "Pexp_fun\n";
    expression i ppf x;
    expression i ppf e
  | Pexp_apply (e, l) ->
     line i ppf "Pexp_apply\n";
     expression i ppf e;
     list i x_expression ppf l

and x_expression (i : int) (ppf : formatter) (e : expression) : unit =
  line i ppf "<arg>\n";
  expression (i + 1) ppf e

let string_of_expression (e : expression) : string =
  expression 0 (str_formatter) e;
  flush_str_formatter ()

let mk_pat (p : pattern_desc) : pattern = {ppat_desc = p}
let mk_exp (e : expression_desc) : expression = {pexp_desc = e}

(*fun_pat x -> x + 1*)
let e = 
  mk_exp (
      Pexp_patfun (
          mk_pat (Ppat_var "x"),
          mk_exp (
              Pexp_apply (
                  (mk_exp (Pexp_ident "+")
                  , [mk_exp (Pexp_ident "x"); 
                     mk_exp (Pexp_constant (Pconst_int 1))
                  ])
    ))))

let gen_var () =
  let counter = ref (-1) in
  let next () =
    counter := !counter + 1;
    "_" ^ (string_of_int (!counter)) in
  let reset () = counter := (-1)
  in (next, reset)

let (mk_var, reset) = gen_var ()

let rec translate_pat ({ppat_desc = p} as pat : pattern) : pattern =
  match p with
  | Ppat_any | Ppat_constant _ | Ppat_var _ -> pat

let rec translate_exp ({pexp_desc = e} as exp : expression) : expression =
  match e with
  | Pexp_constant _ | Pexp_ident _ -> exp
  | Pexp_apply (e, l) -> 
     mk_exp (Pexp_apply (translate_exp e, (List.map translate_exp l)))
  | Pexp_patfun (p, e) -> 
     translate_def exp
  | _ -> assert false

 and translate_def ({pexp_desc = d} : expression) : expression =
    match d with
    | Pexp_patfun (p, e) -> 
       let v = mk_exp (Pexp_ident (mk_var ())) in
       mk_exp (Pexp_fun (v, 
           mk_exp (Pexp_apply (
               (mk_exp (Pexp_patfun (translate_pat p, translate_exp e))),
               [v]))
         ))
    | _ -> assert false

(*fun _0 -> (fun_pat x -> x + 1) _0*)
let () = 
  Printf.printf "%s\n" (string_of_expression (translate_exp e))
