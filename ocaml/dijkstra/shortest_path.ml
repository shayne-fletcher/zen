open Core

(** Dijkstra's algorithm for the single source shortest paths
   problem. *)

module type Graph_sig = sig
  type vertex_t[@@deriving sexp]
  type t[@@deriving sexp]
  type extern_t

  type load_error = [ `Duplicate_vertex of vertex_t ] [@@deriving sexp]
  exception Load_error of load_error [@@deriving sexp]

  val of_adjacency : extern_t -> [ `Ok of t | `Load_error of load_error ]
  val to_adjacency : t -> extern_t

  module Dijkstra : sig
    type state

    type dijkstra_error = [
      | `Relax of vertex_t
      | `Find_min of (vertex_t * float) list
    ] [@@deriving sexp]
    exception Dijkstra_error of dijkstra_error[@@deriving sexp]

    val dijkstra : vertex_t -> t ->
      [ `Ok of state | `Dijkstra_error of dijkstra_error ]

    val d : state -> (vertex_t * float) list
    val shortest_paths : state -> (vertex_t * vertex_t list) list
  end

end

module type GRAPH = sig
  module type Vert = sig
    type t[@@deriving sexp]
    include Comparable.S with type t := t
  end

  module type S = sig
    include Graph_sig
  end

  module Make : functor (V : Vert) ->
    S with type vertex_t = V.t
       and type extern_t = (V.t * (V.t * float) list) list
end

module Graph : GRAPH = struct
  module type Vert = sig
    type t[@@deriving sexp]
    include Comparable.S with type t := t
  end

  module type S = sig
    include Graph_sig
  end

  module Make : functor (V : Vert) ->
    S with type vertex_t = V.t
       and type extern_t = (V.t * (V.t * float) list) list
    =
    functor (V : Vert) -> struct
      module Map = V.Map
      module Set = V.Set

      type vertex_t = V.t[@@deriving sexp]
      type extern_t = (vertex_t * (vertex_t * float) list) list
      type t = (vertex_t * float) list Map.t[@@deriving sexp]

      type load_error = [ `Duplicate_vertex of vertex_t][@@deriving sexp]
      exception Load_error of load_error [@@deriving sexp]

      let to_adjacency g = Map.to_alist g
      let of_adjacency_exn l =
        match Map.of_alist l with
        | `Ok t -> t
        | `Duplicate_key c -> raise (Load_error (`Duplicate_vertex c))

      let of_adjacency l =
        try
          `Ok (of_adjacency_exn l)
        with
        | Load_error err -> `Load_error err

      module Dijkstra = struct

        type state = {
          src    :                 vertex_t
        ; g      :                        t
        ; d      :              float Map.t
        ; pred   :           vertex_t Map.t
        ; s      :                    Set.t
        ; v_s    :  (vertex_t * float) list
        }

        type dijkstra_error = [
          | `Relax of vertex_t
          | `Find_min of (vertex_t * float) list
        ] [@@deriving sexp]
        exception Dijkstra_error of dijkstra_error [@@deriving sexp]

        let init src g =
          let vs = Map.keys g in
          let init s x = if s = x then 0.0 else Float.infinity in
          let d = List.fold vs ~init:Map.empty
              ~f:(fun acc x -> Map.add acc ~key:x ~data:(init src x)) in
          {
            src
          ; g
          ; s = Set.empty
          ; d
          ; pred = Map.empty
          ; v_s = Map.to_alist d
          }

        let find_min v_s =
          match List.min_elt v_s
                  ~cmp:(fun (_, e1) (_, e2) -> Float.compare e1 e2)
          with
          | Some min -> min
          | None -> raise (Dijkstra_error (`Find_min v_s))

        let relax state u v w =
          let {d; pred; _} = state in
          let dv = Map.find_exn d v and du = Map.find_exn d u in
          if dv > du +. w then
            { state with
              d = Map.change d v
                  ~f:(function
                      | Some _ -> Some (du +. w)
                      | None -> raise (Dijkstra_error (`Relax v))
                    )
            ; pred = Map.add (Map.remove pred v) ~key:v ~data:u
            }
          else state

        let dijkstra_exn src g =
          let rec loop ({s; v_s; _} as state) =
            match List.is_empty v_s with
            | true -> state
            | false ->
              let u, _ = find_min v_s in
              let state' =
                List.fold (Map.find_exn g u)
                  ~init:{
                    state with
                    s = Set.add s u
                  ; v_s = List.filter v_s ~f:(fun (x, _) -> x <> u)
                  }
                  ~f:(fun state (v, w) -> relax state u v w) in
              loop {
                state' with
                v_s = List.fold state'.v_s ~init:[]
                    ~f:(fun acc (n, _) -> (n, Map.find_exn state'.d n) :: acc)
              }
          in loop (init src g)

        let dijkstra src g =
          try
            `Ok (dijkstra_exn src g)
          with
          | Dijkstra_error err -> `Dijkstra_error err

        let d state = Map.to_alist (state.d)

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
  type vertex_t = char and type extern_t = (char * (char * float) list) list
  =
  Graph.Make (Char)

let g : G.t =
  match G.of_adjacency
    [ 's', ['u',  3.0; 'x', 5.0]
    ; 'u', ['x',  2.0; 'v', 6.0]
    ; 'x', ['v',  4.0; 'y', 6.0; 'u', 1.0]
    ; 'v', ['y',  2.0]
    ; 'y', ['v',  7.0]
    ] with
  | `Ok g -> g
  | `Load_error _ -> failwith "error loading graph"
;;
let s =
  match (G.Dijkstra.dijkstra 's' g) with
  | `Ok s -> s
  | `Dijkstra_error _ -> failwith "error running dijkstra"
;; G.Dijkstra.d s
;; G.Dijkstra.shortest_paths s
