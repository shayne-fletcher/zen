open Format

type t = {
  stamp : int;
  name : string;
  mutable flags : int
}

let global_flag : int = 1
let predef_exn_flag : int = 2

(*A stamp of 0 denotes a persistent identifier*)
let currentstamp : int ref = ref 0

let create (s : string) : t =
  incr currentstamp;
  {name = s; stamp = !currentstamp; flags = 0}

let create_predef_exn (s : string) : t =
  incr currentstamp;
  { name = s; stamp = !currentstamp; flags = predef_exn_flag }

let create_persistent (s : string) : t =
  { name = s; stamp = 0; flags = global_flag }

let rename (i : t) : t =
  incr currentstamp;
  { i with stamp = !currentstamp }

let name (i : t) : string = i.name
let unique_name (i : t) : string = i.name ^ "_" ^ string_of_int i.stamp
let persistent (i : t) : bool = i.stamp = 0
let equal (u : t) (v : t) : bool = u.name = v.name
let same (u : t) (v : t) : bool = u = v
let compare (u : 'a) (v : 'a) : int = Pervasives.compare u v
let binding_time (i : t) : int = i.stamp

let current_time () : int = !currentstamp
let set_current_time (t : int) : unit = currentstamp := max !currentstamp t

let reinit_level : int ref = ref (-1)

let reinit () : unit =
  if !reinit_level < 0
  then reinit_level := !currentstamp
  else currentstamp := !reinit_level

let hide (i : t) : t = { i with stamp = -1 }
let make_global (i : t) : unit = i.flags <- i.flags lor global_flag
let global (i : t) : bool = (i.flags land global_flag) <> 0
let is_predef_exn (i : t) : bool = (i.flags land predef_exn_flag) <> 0

let print (ppf : formatter) (i : t) =
  match i.stamp with
  | 0 -> fprintf ppf "%s!" i.name
  | -1 -> fprintf ppf "%s#" i.name
  | n -> fprintf ppf "%s/%i%s" i.name n (if global i then "g" else "")

(*An ['a tbl] is a balanced binary search tree*)
type 'a tbl =
| Empty (*Case of an empty tree*)
| Node of 'a tbl * 'a data * 'a tbl * int (*Case of a node*)
(*Each node contains an ['a data]*)
and 'a data = {
  ident : t; (*Stamped identifier*)
  data : 'a; (*The actual data item*)
  previous : 'a data option (*Potentially a previous data item*)
}

let empty : 'a tbl = Empty

let height : 'a tbl -> int = function
  | Empty -> 0
  | Node (_, _, _, h) -> h

let mknode (l : 'a tbl) (d : 'a data) (r : 'a tbl) : 'a tbl= 
  let hl = height l
  and hr = height r in
  Node (l, d, r, (if hl >= hr then hl + 1 else hr + 1))

let balance (l : 'a tbl) (d : 'a data) (r : 'a tbl) =
  let hl = height l
  and hr = height r in
  if hl > hr + 1 then
    match l with
    | Node (ll, ld, lr, _) when height ll >= height lr ->
    (*Case 1*)
      mknode ll ld (mknode lr d r)
    | Node (ll, ld, Node (lrl, lrd, lrr, _), _) ->
    (*Case 2*)
      mknode (mknode ll ld lrl) lrd (mknode lrr d r)
    | _ -> assert false
  else if hr > hl + 1 then
    match r with
    | Node (rl, rd, rr, _) when height rr >= height rl ->
    (*Case 3*)
      mknode (mknode l d rl) rd rr
    | Node (Node (rll, rld, rlr, _), rd, rr, _) ->
    (*Case 4*)
      mknode (mknode l d rll) rld (mknode rlr rd rr)
    | _ -> assert false
  else
    mknode l d r

let rec add (id : t) (data : 'a) : 'a tbl -> 'a tbl = function
  | Empty -> 
    Node (Empty, {ident = id; data = data; previous = None}, Empty, 1)
  | Node (l, k, r, h) ->
    let c = compare id.name k.ident.name in
    if c = 0 then
      Node (l, { ident = id; data = data; previous = Some k}, r, h)
    else if c < 0 then
      balance (add id data l) k r
    else
      balance l k (add id data r)

let rec find_stamp (s : int) : 'a data option -> 'a = function
  | None -> raise Not_found
  | Some k -> if k.ident.stamp = s then k.data else find_stamp s k.previous

let rec find_same (id : t) : 'a tbl -> 'a = function
  | Empty -> raise Not_found
  | Node (l, k, r, _) ->
    (*If the name matches [id]...*)
    let c = compare id.name k.ident.name in
    if c = 0 then
      (*... and the stamp matches [id]...*)
      if id.stamp = k.ident.stamp 
      then k.data (*... then this data...*)
      else find_stamp id.stamp k.previous (*else search previous*)
    else find_same id (if c < 0 then l else r)

let rec find_name (name : string) : 'a tbl -> 'a = function
  | Empty -> raise Not_found
  | Node (l, k, r, _) ->
    let c = compare name k.ident.name in
    if c = 0 then
      k.data
    else 
      find_name name (if c < 0 then l else r)

let rec get_all : 'a data option -> 'a list = function
  | None -> []
  | Some k -> k.data :: get_all k.previous

let rec fold_aux 
    (f : 'a data -> 'b -> 'b) 
    (stack : 'a tbl list) (acc : 'b) : 'a tbl -> 'b = function
  | Empty ->
    begin match stack with
    | [] -> acc
    | a :: l -> fold_aux f l acc a
    end
  | Node (l, k, r, _) ->
    fold_aux f (l :: stack) (f k acc) r

let fold_name 
    (f : t -> 'a -> 'b)
    (tbl : 'a tbl)
    (acc : 'b) : 'b = 
  fold_aux (fun k -> f k.ident k.data) [] acc tbl

let rec fold_data 
    (f : t -> 'a -> 'b -> 'b)
    (d : 'a data option)
    (acc : 'b ) : 'b =
  match d with
  | None -> acc
  | Some k -> f k.ident k.data (fold_data f k.previous acc)

let fold_all 
    (f : t -> 'a -> 'b -> 'b)
    (tbl : 'a tbl)
    (acc : 'b) : 'b = 
  fold_aux (fun k -> fold_data f (Some k)) acc tbl

let rec iter (f : t -> 'a -> 'b) : 'a tbl -> unit = function
  | Empty -> ()
  | Node (l, k, r, _) ->
    iter f l; f k.ident k.data; iter f r

(*Idents for sharing keys*)

(*They should be 'totally fresh' -> neg numbers*)
let key_name = ""

let make_key_generator () =
  let c = ref 1 in
  fun id ->
    let stamp = !c in
    decr c ;
    { id with name = key_name; stamp = stamp }
