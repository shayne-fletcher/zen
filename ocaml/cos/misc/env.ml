(*
#load "ident.cmo";;
*)

module Env_tbl = struct

  (*A table indexed by identifier, with an extra slot to record
    usage*)
  type 'a t = ('a * (unit -> unit)) Ident.tbl

  let empty : 'a t = Ident.empty
  let nothing : unit -> unit = fun () -> ()

  let already_defined 
      (wrap : ('a * 'b) option -> 'c) 
      (s : string) 
      (tbl : 'a t) 
      (x : 'b) : 'c = 
    wrap (try Some (fst (Ident.find_name s tbl), x) with Not_found -> None)

  let add 
      (slot : (string -> 'a -> unit) option)
      (wrap : ('b * 'c) option -> 'a)
      (id : Ident.t)
      (x : 'c) 
      (tbl : 'c t) 
      (ref_tbl : 'b t) : 'c t =
    let slot = 
      match slot with
      | None -> nothing
      | Some f ->
        (fun () ->
          let s = Ident.name id in
          f s (already_defined wrap s ref_tbl x)
        ) in
    Ident.add id (x, slot) tbl

  let find_same (id : Ident.t) (tbl : 'a t) =
    let (x, slot) = Ident.find_same id tbl in
    slot ();
    x

  let find_same_not_using (id : Ident.t) (tbl : 'a t) =
    fst (Ident.find_same id tbl)

  let find_all (s : string) (tbl : 'a Ident.tbl) : 'a list =
    Ident.find_all s tbl

  let fold_name (f : Ident.t -> 'a -> 'b -> 'b) : 'a t -> 'b -> 'b =
    Ident.fold_name (fun k (d, _) -> f k d)

end

