open Ml_asttypes
open Ml_ast
open Ml_location
open Lexing
open Format

(*[line i f s] formats whitespace on [pp] proportional to the depth
  indicator [i] before computing the format operation indicated by
  [s]*)
let line 
    (i : int) 
    (ppf : formatter) 
    (s : ('a, formatter, unit) format) : 'a =
  fprintf ppf "%s" (String.make ((2 * i) mod 72) ' ');
  fprintf ppf s

(*[pair i f ppf p] formats a pair on [ppf] with a depth indicator
  given by [i] by way of [f]*)
let pair 
    (i : int)
    (f : int -> formatter -> 'a -> 'b)
    (ppf:formatter) 
    ((u, v) : 'a * 'a ) : unit =
    line i ppf "(\n";
    f (i + 1) ppf u;
    f (i + 1) ppf v;
    line i ppf ")\n"

(*[list i f ppf p] formats a list on [ppf] with a depth indicator
  given by [i] by way of [f]*)
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

(*[option i f ppf x] formats an ['a option] on ppf with a depth
  indicator given by [i] by way of [f]*)
let option i f ppf x =
  match x with
  | None -> line i ppf "None\n";
  | Some x ->
      line i ppf "Some\n";
      f (i + 1) ppf x

(*[fmt_rec_flag ppf x] formats ["Rec"] or ["Nonrec"] on [ppf]
   according to the case of [x]*)
let fmt_rec_flag (ppf : formatter) (x : rec_flag) : unit =
  match x with
  | Nonrecursive -> fprintf ppf "Nonrec"
  | Recursive -> fprintf ppf "Rec"

(*[fmt_position with_name ppf l] formats the position [l] on [ppf]*)
let fmt_position (with_name : bool) (ppf : formatter) (l : position) : unit =
  let fname = if with_name then l.pos_fname else "" in
  if l.pos_lnum = -1
  then fprintf ppf "%s[%d]" fname l.pos_cnum
  else fprintf ppf "%s[%d,%d+%d]" fname l.pos_lnum l.pos_bol
               (l.pos_cnum - l.pos_bol)

(*[fmt_location ppf loc] formats the location [loc] on [ppf]*)
let fmt_location (ppf : formatter) (loc : Ml_location.t) : unit =
  let p_2nd_name = loc.loc_start.pos_fname <> loc.loc_end.pos_fname in
  fprintf ppf "(%a..%a)" (fmt_position true) loc.loc_start
                       (fmt_position p_2nd_name) loc.loc_end

(*[fmt_string_loc ppf loc] formats [loc] of type [string loc] on
  [ppf]*)
let fmt_string_loc (ppf : formatter) (x : string Ml_location.loc) : unit =
  fprintf ppf "\"%s\" %a" x.txt fmt_location x.loc

(*[fmt_constant ppf x] formats the constant [x] on [ppf]*)
let fmt_constant (ppf : formatter) (x : constant) : unit =
  match x with
  | Pconst_int i -> fprintf ppf "Pconst_int (%s)" i

(*[fmt_ident_loc ppf x] formats [x] of type [string loc] on [ppf]. It
  is a reprint of [fmt_string_loc]*)
let fmt_ident_loc (ppf : formatter) (x : string Ml_location.loc) : unit =
  fprintf ppf "\"%s\" %a" x.txt fmt_location x.loc

(*Format function for top-level phrases*)
let rec toplevel_phrase 
    (i : int) 
    (ppf : formatter) 
    (x : toplevel_phrase) : unit =
  match x with
  | Ptop_def s ->
    line i ppf "Ptop_def\n";
    structure (i + 1) ppf s
(*Format function for structures*)
and structure 
    (i : int) 
    (ppf : formatter) 
    (x : structure) : unit = 
  list i structure_item ppf x 
(*Format function for structure items*)
and structure_item 
    (i : int) 
    (ppf : formatter) 
    (x : structure_item) : unit =
  line i ppf "structure_item %a\n" fmt_location x.pstr_loc;
  let i = i + 1 in
  match x.pstr_desc with
  | Pstr_eval e ->
    line i ppf "Pstr_eval\n";
    expression i ppf e
  | Pstr_value (rf, l) ->
    line i ppf "Pstr_value %a\n" fmt_rec_flag rf;
    list i value_binding ppf l
(*Format function for patterns*)
and pattern (i : int) (ppf : formatter) (x : pattern) : unit =
  line i ppf "pattern %a\n" fmt_location x.ppat_loc;
  let i = i + 1 in
  match x.ppat_desc with
  | Ppat_any -> line i ppf "Ppat_any\n";
  | Ppat_var s -> line i ppf "Ppat_var %a\n" fmt_string_loc s;
  | Ppat_constant c -> line i ppf "Ppat_constant %a\n" fmt_constant c;
  | Ppat_construct (li, po) -> 
    line i ppf "Ppat_construct %a\n" fmt_ident_loc li;
    option i pattern ppf po
  | Ppat_tuple l ->
      line i ppf "Ppat_tuple\n";
      list i pattern ppf l
(*Format function for expressions*)
and expression (i : int) (ppf : formatter) (x : expression) : unit  =
  line i ppf "expression %a\n" fmt_location x.pexp_loc;
  let i = i + 1 in
  match x.pexp_desc with
  | Pexp_ident li -> line i ppf "Pexp_ident %a\n" fmt_ident_loc li
  | Pexp_constant c -> line i ppf "Pexp_constant %a\n" fmt_constant c
  | Pexp_construct (li, eo) ->
      line i ppf "Pexp_construct %a\n" fmt_ident_loc li;
      option i expression ppf eo
  | Pexp_let (rf, l, e) ->
    line i ppf "Pexp_let %a\n" fmt_rec_flag rf;
    list i value_binding ppf l;
    expression i ppf e
  | Pexp_fun (p, e) ->
    line i ppf "Pexp_fun\n";
    pattern i ppf p;
    expression i ppf e
  | Pexp_apply (e, l) ->
    line i ppf "Pexp_apply\n";
    expression i ppf e;
    list i x_expression ppf l
  | Pexp_match (e, l) ->
    line i ppf "Pexp_match\n";
    expression i ppf e;
    list i case ppf l
  | Pexp_tuple l ->
    line i ppf "Pexp_tuple\n";
    list i expression ppf l
  | Pexp_if_then_else (e1, e2, e3) ->
    line i ppf "Pexp_if_then_else\n";
    expression i ppf e1;
    expression i ppf e2;
    expression i ppf e3
(*Format function for match cases*)
and case i ppf {pc_lhs; pc_guard; pc_rhs} =
  line i ppf "<case>\n";
  pattern (i + 1) ppf pc_lhs;
  begin match pc_guard with
  | None -> ()
  | Some g -> line (i + 1) ppf "<when>\n"; expression (i + 2) ppf g
  end;
  expression (i + 1) ppf pc_rhs
(*Format function for value bindings*)
and value_binding (i : int) (ppf : formatter) (x : value_binding) : unit =
  line i ppf "<def>\n";
  pattern (i + 1) ppf x.pvb_pat;
  expression (i + 1) ppf x.pvb_expr
(*Format function for applicands*)
and x_expression (i : int) (ppf : formatter) (e : expression) : unit =
  line i ppf "<arg>\n";
  expression (i + 1) ppf e

(*String representation of patterns*)
let string_of_pattern (p : pattern) : string =
  pattern 0 (str_formatter) p;
  flush_str_formatter ()

(*String representation of expressions*)
let string_of_expression (e : expression) : string =
  expression 0 (str_formatter) e;
  flush_str_formatter ()

(*String representation of top-level phrases*)
let string_of_toplevel_phrase (p : toplevel_phrase) : string =
  toplevel_phrase 0 (str_formatter) p;
  flush_str_formatter ()
