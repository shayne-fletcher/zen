open Core

module type Graph_sig = sig
  type node
  type t
  type extern_t

  val of_adjacency : extern_t -> t
  val to_adjacency : t -> extern_t

  exception Error of [`Duplicate_node ][@@deriving sexp]

  module Dijkstra : sig
    type state

    exception Error of [ `Find_min | `Relax ][@@deriving sexp]

    val dijkstra : node -> t -> state
    val d : state -> (node * float) list
    val shortest_paths : state -> (node * node list) list
  end

end

module type GRAPH = sig
  module type Ord = sig
    type t
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
    type t
    include Comparable.S with type t := t
  end

  module type S = sig
    include Graph_sig
  end

  module Make : functor (M : Ord) ->
    S with type node = M.t
       and type extern_t = (M.t * (M.t * float) list) list
    =
    functor (M : Ord) -> struct
      module Map = M.Map
      module Set = M.Set

      type node = M.t
      type extern_t = (node * (node * float) list) list
      type t = (node * float) list Map.t

      exception Error of [`Duplicate_node ][@@deriving sexp]

      let to_adjacency g = Map.to_alist g
      let of_adjacency l =
        match Map.of_alist l with
        | `Ok t -> t
        | `Duplicate_key _ -> raise (Error `Duplicate_node)

      module Dijkstra = struct

        type state = {
          src    :                 node
        ; g      :                    t
        ; d      :          float Map.t
        ; pred   :           node Map.t
        ; s      :                Set.t
        ; v_s    :  (node * float) list
        }

        exception Error of [ `Find_min | `Relax ][@@deriving sexp]

        let init src g =
          let vs = Map.keys g in
          let init s x = if s = x then 0.0 else Float.infinity in
          let d = List.fold vs ~init:Map.empty
              ~f:(fun acc x -> Map.add acc ~key:x ~data:(init src x)) in
          {
            src
          ; g
          ; s = M.Set.empty
          ; d
          ; pred = Map.empty
          ; v_s = Map.to_alist d
          }

        let find_min v_s =
          match List.min_elt v_s
                  ~cmp:(fun (_, e1) (_, e2) -> Float.compare e1 e2)
          with
          | Some min -> min
          | None -> raise (Error `Find_min)

        let relax state u v w =
          let {d; pred; _} = state in
          let dv = Map.find_exn d v and du = Map.find_exn d u in
          if dv > du +. w then
            { state with
              d = Map.change d v
                  ~f:(function
                      | Some _ -> Some (du +. w)
                      | None -> raise (Error `Relax)
                    )
            ; pred = Map.add (Map.remove pred v) ~key:v ~data:u
            }
          else state

        let dijkstra src g =
          let rec loop ({s; v_s; _} as state) =
            match List.is_empty v_s with
            | true -> state
            | false ->
              let u, _ = find_min v_s in
              let state' =
                List.fold (Map.find_exn g u)
                  ~init:{
                    state with
                    s = M.Set.add s u
                  ; v_s = List.filter v_s ~f:(fun (x, _) -> x <> u)
                  }
                  ~f:(fun state (v, w) -> relax state u v w) in
              loop {
                state' with
                v_s = List.fold state'.v_s ~init:[]
                    ~f:(fun acc (n, _) -> (n, Map.find_exn state'.d n) :: acc)
              }
          in loop (init src g)

        let d state = M.Map.to_alist (state.d)

        let path state n =
          let rec loop acc x =
            if x = state.src then state.src :: acc
            else loop (x :: acc) (Map.find_exn state.pred x) in
          loop [] n

        let shortest_paths state =
          List.map (Map.keys state.g) ~f:(fun n -> (n, path state n))
      end
    end
end

module G : Graph.S with
  type node = char and type extern_t = (char * (char * float) list) list
  =
  Graph.Make (Char)

let g : G.t =
  G.of_adjacency
    [ 's', ['u',  3.0; 'x', 5.0]
    ; 'u', ['x',  2.0; 'v', 6.0]
    ; 'x', ['v',  4.0; 'y', 6.0; 'u', 1.0]
    ; 'v', ['y',  2.0]
    ; 'y', ['v',  7.0]
    ]

let s = (G.Dijkstra.dijkstra 's' g)
;; G.Dijkstra.d s
;; G.Dijkstra.shortest_paths s
