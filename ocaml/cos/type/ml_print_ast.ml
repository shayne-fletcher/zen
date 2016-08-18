open Ml_asttypes
open Ml_ast
open Ml_location
open Lexing
open Format

let line 
    (i : int) 
    (f : formatter) 
    (s : ('a, formatter, unit) format) : 'a =
  fprintf f "%s" (String.make ((2 * i) mod 72) ' ');
  fprintf f s

let pair 
    (i : int)
    (f : int -> formatter -> 'a -> 'b)
    (ppf:formatter) 
    ((u, v) : 'a * 'a ) : unit =
    line i ppf "(\n";
    f (i + 1) ppf u;
    f (i + 1) ppf v;
    line i ppf ")\n"

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

let fmt_rec_flag (f : formatter) (x : rec_flag) : unit =
  match x with
  | Nonrecursive -> fprintf f "Nonrec"
  | Recursive -> fprintf f "Rec"

let fmt_position (with_name : bool) (f : formatter) (l : position) : unit =
  let fname = if with_name then l.pos_fname else "" in
  if l.pos_lnum = -1
  then fprintf f "%s[%d]" fname l.pos_cnum
  else fprintf f "%s[%d,%d+%d]" fname l.pos_lnum l.pos_bol
               (l.pos_cnum - l.pos_bol)

let fmt_location (f : formatter) (loc : Ml_location.t) : unit =
  let p_2nd_name = loc.loc_start.pos_fname <> loc.loc_end.pos_fname in
  fprintf f "(%a..%a)" (fmt_position true) loc.loc_start
                       (fmt_position p_2nd_name) loc.loc_end

let fmt_string_loc (f : formatter) (x : string Ml_location.loc) : unit =
  fprintf f "\"%s\" %a" x.txt fmt_location x.loc

let fmt_constant (f : formatter) (x : constant) : unit =
  match x with
  | Pconst_int i -> fprintf f "Pconst_int (%s)" i

let fmt_ident_loc (f : formatter) (x : string Ml_location.loc) : unit =
  fprintf f "\"%s\" %a" x.txt fmt_location x.loc

let rec toplevel_phrase 
    (i : int) 
    (ppf : formatter) 
    (x : toplevel_phrase) : unit =
  match x with
  | Ptop_def s ->
    line i ppf "Ptop_def\n";
    structure (i + 1) ppf s

and structure 
    (i : int) 
    (ppf : formatter) 
    (x : structure) : unit = 
  list i structure_item ppf x 

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

and pattern (i : int) (ppf : formatter) (x : pattern) : unit =
  line i ppf "pattern %a\n" fmt_location x.ppat_loc;
  let i = i + 1 in
  match x.ppat_desc with
  | Ppat_any -> line i ppf "Ppat_any\n";
  | Ppat_var s -> line i ppf "Ppat_var %a\n" fmt_string_loc s;
  | Ppat_constant c -> line i ppf "Ppat_constant %a\n" fmt_constant c;
  | Ppat_construct li -> line i ppf "Ppat_construct %a\n" fmt_ident_loc li
  | Ppat_pair (u, v) ->
      line i ppf "Ppat_pair\n";
      pair i pattern ppf (u, v)

and expression (i : int) (ppf : formatter) (x : expression) : unit  =
  line i ppf "expression %a\n" fmt_location x.pexp_loc;
  let i = i + 1 in
  match x.pexp_desc with
  | Pexp_ident li -> line i ppf "Pexp_ident %a\n" fmt_ident_loc li
  | Pexp_constant c -> line i ppf "Pexp_constant %a\n" fmt_constant c
  | Pexp_construct li ->  line i ppf "Pexp_construct %a\n" fmt_ident_loc li
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
  | Pexp_pair (u, v) ->
    line i ppf "Pexp_pair\n";
    pair i expression ppf (u, v)
  | Pexp_if_then_else (e1, e2, e3) ->
    line i ppf "Pexp_if_then_else\n";
    expression i ppf e1;
    expression i ppf e2;
    expression i ppf e3

and value_binding (i : int) (ppf : formatter) (x : value_binding) : unit =
  line i ppf "<def>\n";
  pattern (i + 1) ppf x.pvb_pat;
  expression (i + 1) ppf x.pvb_expr

and x_expression (i : int) (ppf : formatter) (e : expression) : unit =
  line i ppf "<arg>\n";
  expression (i + 1) ppf e

let string_of_pattern (p : pattern) : string =
  pattern 0 (str_formatter) p;
  flush_str_formatter ()

let string_of_expression (e : expression) : string =
  expression 0 (str_formatter) e;
  flush_str_formatter ()

let string_of_toplevel_phrase (p : toplevel_phrase) : string =
  toplevel_phrase 0 (str_formatter) p;
  flush_str_formatter ()
