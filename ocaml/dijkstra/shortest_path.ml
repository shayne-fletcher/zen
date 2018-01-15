open Core

module type Graph_sig = sig
  type node [@@deriving sexp]
  type t [@@deriving sexp]
  type extern_t

  type state

  val to_sexp : t -> Sexp.t
  val of_sexp : Sexp.t -> t

  val of_adjacency : extern_t -> [ | `Ok of t | `Duplicate_key of node ]
  val to_adjacency : t -> extern_t

  val dijkstra : node -> t -> state
  val d : state -> (node * float) list
  val shortest_paths : state -> (node * node list) list

end

module type GRAPH = sig
  module type Ord = sig
    type t [@@deriving sexp]
    include Comparable.S with type t := t
  end

  module type S = sig
    include Graph_sig
  end

  module Make : functor (M : Ord) ->
    S with type node = M.t
       and type extern_t = (M.t * (M.t * float) list) list
end

module Graph : GRAPH = struct
  module type Ord = sig
    type t [@@deriving sexp]
    include Comparable.S with type t := t
end

  module type S = sig
    include Graph_sig
  end

  module Make : functor (M : Ord) -> S
    with type node = M.t
     and type extern_t = (M.t * (M.t * float) list) list
    =
    functor (M : Ord) -> struct
      type node = M.t [@@deriving sexp]
      type extern_t = (node * (node * float) list) list
      type t = (node * float) list M.Map.t [@@deriving sexp]

      let to_sexp g = sexp_of_t g
      let of_sexp s = t_of_sexp s
      let to_adjacency g = M.Map.to_alist g
      let of_adjacency l = M.Map.of_alist l

      type state = {
        src    :                 node
      ; g      :                    t
      ; d      :        float M.Map.t
      ; pred   :         node M.Map.t
      ; s      :              M.Set.t
      ; v_s    :  (float * node) list
      }

      let initial_state src g =
        let vs = M.Map.keys g in
        let d =
          List.fold vs ~init:M.Map.empty
            ~f:(
              fun acc x ->
                M.Map.add acc
                  ~key:x
                  ~data:(if src = x then 0.0
                         else Float.infinity)) in
        {
          src
        ; g
        ; s = M.Set.empty
        ; d
        ; pred = M.Map.empty
        ; v_s = (List.map (M.Map.to_alist d) ~f:(fun (n, e) -> (e, n)))
        }

      let relax state u v w =
        let {d; pred; _} = state in
        let dv = M.Map.find_exn d v in
        let du = M.Map.find_exn d u in
        let state' =
          if dv > du +. w then
            { state with
              d = M.Map.change d v
                  ~f:(function
                      | Some _ -> Some (du +. w)
                      | None -> failwith "relax : missing"
                    )
            ; pred = M.Map.add (M.Map.remove pred v) ~key:v ~data:u }
          else state in
        state'

      let find_min (v_s : (float * node) list) =
        match List.min_elt v_s
                ~cmp:(fun (e1, _) (e2, _) -> Float.compare e1 e2)
        with
        | Some m -> m
        | None -> failwith "find_min : min. element not found"

      let dijkstra src g =
        let rec loop ({s; v_s; _} as state) =
          if List.is_empty v_s then state
          else
            let (_, u) = find_min v_s in
            let state' =
              List.fold (M.Map.find_exn g u)
                ~init:{
                  state with
                  s = M.Set.add s u
                ; v_s = List.filter v_s ~f:(fun (_, x) -> x <> u)
                }
                ~f:(fun state (v, w) -> relax state u v w) in
            let v_s =
              List.fold state'.v_s ~init:[]
                ~f:(fun acc (_, n) -> (M.Map.find_exn state'.d n, n) :: acc)
            in  loop {state' with v_s} in
        loop (initial_state src g)

      let d state = M.Map.to_alist (state.d)

      let path state n =
        let rec loop acc x =
          if x = (state.src) then state.src :: acc
          else loop (x :: acc) (M.Map.find_exn state.pred x) in
        loop [] n

      let shortest_paths state =
        List.map (M.Map.keys state.g) ~f:(fun n -> (n, path state n))

    end
end

module G : Graph.S with
  type node = char and type extern_t = (char * (char * float) list) list
  =
  Graph.Make (Char)

let g : G.t =
  match G.of_adjacency
   [   's', ['u', 3.0; 'x', 5.0]
     ; 'u', ['x',  2.0; 'v', 6.0]
     ; 'x', ['v',  4.0; 'y', 6.0; 'u', 1.0]
     ; 'v', ['y',  2.0]
     ; 'y', ['v',  7.0]
    ] with
  | `Ok g -> g
  | `Duplicate_key c -> failwithf "of_adjacency : duplicate key '%c'" c ()

(* ;; Printf.printf "%s" (Sexp.to_string (G.to_sexp g)) *)
let s = (G.dijkstra 's' g)
;; G.d s
;; G.shortest_paths s
