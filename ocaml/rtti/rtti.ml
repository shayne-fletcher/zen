module Type_eq : sig

  (*A value of type [('a, 'b) t] is a witness that the two types ['a]
    and ['b] are equal*)
  type (_, _) t = Eq : ('a, 'a) t

  val refl : ('a, 'a) t
  val trans : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  val sym : ('a, 'b) t -> ('b, 'a) t
  val app : ('a, 'b) t -> 'a -> 'b

  module Lift (T : sig type 'a c end) : sig
    val eq : ('a, 'b) t -> ('a T.c, 'b T.c) t
  end

  (* val unsafe : ('a, 'b) t *)
end = struct

  type (_, _) t = Eq : ('a, 'a) t

  let refl = Eq

  let trans (type a) (type b) (type c) 
        (Eq : (a, b) t) (Eq : (b, c) t) = (Eq : (a, c) t)

  let sym (type a) (type b) (Eq : (a, b) t) = (Eq : (b, a) t)

  let app (type a) (type b) (Eq : (a, b) t) (x : a) = (x : b)

  module Lift (T : sig type 'a c end) = struct
    let eq (type a) (type b) (Eq : (a, b) t) = (Eq : (a T.c, b T.c) t)
  end

end

type record_repr = 
  | Record_regular 
  | Record_float 
  | Record_inline of int

type 'node gtype =
  | DT_node of 'node
  | DT_int
  | DT_float
  | DT_string
  | DT_tuple of 'node gtype list
  | DT_list of 'node gtype
  | DT_array of 'node gtype
  | DT_option of 'node gtype
  | DT_arrow of string * 'node gtype * 'node gtype

type stype = node gtype
and node = {
  mutable rec_descr : node_descr;
  rec_uid : int;
  rec_name : string;
  rec_args : stype list;
  mutable rec_has_var : bool option;
  mutable rec_hash : int;
}
and node_descr =
| DT_variant of variant_descr
| DT_record of record_descr
and variant_descr =  {
  variant_constrs : (string * stype variant_args) list
}
and 'stype variant_args =
  | C_tuple of 'stype list
  | C_inline of 'stype
and record_descr = {
  record_fields : (string * stype) list;
  record_repr : record_repr;
}

type list_sep = 
  | STAR
  | COMMA

let is_enumeration = function
  | DT_node {rec_descr = DT_variant {variant_constrs}} ->
    List.for_all (fun (_, args) -> args = C_tuple []) variant_constrs
  | _ -> false

let uninline = function
  | C_tuple tl -> tl
  | C_inline t -> [t]

let print_stype ~show_enumerations ff = 
  let seen = Hashtbl.create 8 in
  let rec is_complex t =
    (show_enumerations || not (is_enumeration t)) &&
      match t with
      | DT_node n -> not (Hashtbl.mem seen n.rec_uid)
      | DT_list t | DT_array t | DT_option t -> is_complex t
      | DT_tuple l -> List.exists is_complex l
      | _ -> false in
  let rec aux ff t = 
    match t with
    | DT_node n when Hashtbl.mem seen n.rec_uid || 
        (not show_enumerations && is_enumeration t) -> name ff n
    | DT_node n ->
      Hashtbl.replace seen n.rec_uid ();
      Format.fprintf ff "(@[<v2>%a =%a@])" name n descr n.rec_descr
    | DT_int -> Format.fprintf ff "int"
    | DT_float -> Format.fprintf ff "float"
    | DT_string -> Format.fprintf ff "string"
    | DT_tuple l -> tlist STAR ff l
    | DT_list t -> Format.fprintf ff "%a list" aux t
    | DT_array t -> Format.fprintf ff "%a array" aux t
    | DT_option t -> Format.fprintf ff "%a option" aux t
    | DT_arrow ("", t1, t2) -> Format.fprintf ff ("%a -> %a") aux t1 aux t2
    | DT_arrow (l, t1, t2) -> Format.fprintf ff ("%s:%a -> %a") l aux t1 aux t2
  and tlist sep ff = function
    | [] -> Format.fprintf ff "[]"
    | x :: [] -> aux ff x
    | hd :: tl (*as l*) ->
      Format.fprintf ff "(";
      Format.fprintf ff "@[<h>";
      aux ff hd;
      List.iter (
        fun x -> 
          begin
            match sep with
            | STAR -> Format.fprintf ff "@ *@ ";
            | COMMA -> Format.fprintf ff ",@ "
          end;
          aux ff x
      ) tl;
      Format.fprintf ff "@])"
  and name ff n =
    match n.rec_args with
    | [] -> Format.fprintf ff "%s" n.rec_name
    | l -> Format.fprintf ff "%a %s" (tlist COMMA) l n.rec_name
  and descr ff = function
    | DT_variant v ->
      let simple =
        List.for_all (fun (_, args) -> args = C_tuple []) v.variant_constrs in
      if simple then
        Format.fprintf ff "@[<h>";
      let first = ref simple in
      List.iter
        (fun (c, args) ->
          if not !first then
            Format.fprintf ff "@ | "
          else
            begin
              Format.fprintf ff "@ ";
              first := false;
            end;
          Format.fprintf ff "%s" c;
          let args = uninline args in
          if args <> [] then
            begin
              Format.fprintf ff " of";
              if List.exists is_complex args then
                Format.fprintf ff "@ ";
              Format.fprintf ff " %a" (tlist STAR) args;
            end
        ) v.variant_constrs;
      if simple then
        Format.fprintf ff "@]";
    | DT_record r ->
      Format.fprintf ff "@ {@[<v1>";
      List.iter
        (fun (c, arg) ->
          Format.fprintf ff "@ %s" c;
          Format.fprintf ff ": ";
          if is_complex arg then
            Format.fprintf ff "@ ";
          aux ff arg;
          Format.fprintf ff ";"
        )
        r.record_fields;
      Format.fprintf ff "@]@ }"
  in aux ff

(*
type r = {
  foo : int;
  bar : float;
  baz : string;
  qux : (int * float * string);
}
*)

let rd : record_descr = {
  record_fields = [
    ("foo", DT_int); 
    ("bar", DT_float); 
    ("baz", DT_string);
    ("qux", DT_tuple [DT_int; DT_float; DT_string])
  ];
  record_repr = Record_regular;
}

let n : node = {
  rec_descr = DT_record rd;
  rec_uid = 0;
  rec_name = "r";
  rec_args = [];
  rec_has_var = None;
  rec_hash = 0;
}

let rt : node gtype = DT_node n

let test_0 : unit -> unit = 
  fun () ->
    print_stype ~show_enumerations:true (Format.std_formatter) rt;
    Format.pp_print_newline (Format.std_formatter) ()

(*
type s = Foo | Bar of int | Baz of (int * float)
*)

let sd : variant_descr = {
  variant_constrs = [
    ("Foo", C_tuple []);
    ("Bar", C_inline DT_int);
    ("Baz", C_tuple [DT_int; DT_float]);
  ]
}

let o : node = {
  rec_descr = DT_variant sd;
  rec_uid = 1;
  rec_name = "s";
  rec_args = [];
  rec_has_var = None;
  rec_hash = 0;
}

let st : node gtype = DT_node o

let test_1 ()  = 
  print_stype ~show_enumerations:true (Format.std_formatter) st;
  Format.pp_print_newline (Format.std_formatter) ()

let () = test_0 (); test_1 ()
