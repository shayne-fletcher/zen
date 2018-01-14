open Core

module type Graph_sig = sig
  type node [@@deriving sexp]
  type t [@@deriving sexp]
  type extern_t

  type colors = [ | `White | `Gray | `Black ]
  type 'a state

  val to_sexp : t -> Sexp.t
  val of_sexp : Sexp.t -> t
  val of_adjacency : extern_t -> [ | `Ok of t | `Duplicate_key of node ]
  val to_adjacency : t -> extern_t

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
    S with type node = M.t and type extern_t = (M.t * M.t list) list
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
    with type node = M.t and
    type extern_t = (M.t * M.t list) list =

    functor (M : Ord) -> struct
      type node = M.t [@@deriving sexp]
      type extern_t = (node * node list) list

      type t = node list M.Map.t [@@deriving sexp]

      let to_sexp g = sexp_of_t g
      let of_sexp s = t_of_sexp s
      let to_adjacency g = M.Map.to_alist g
      let of_adjacency l = M.Map.of_alist l

      type colors = [ | `White | `Gray | `Black]
      type 'a state = {
        d : int M.Map.t  (* discovery time *)
      ; pred : node M.Map.t (* predecessor *)
      ; color : colors M.Map.t (* vertex colors*)
      ; acc : 'a (* user specified type for [fold] *)
      }

    end

end

module G :
  Graph.S with type node = char
           and type extern_t = (char * char list) list = Graph.Make (Char)

let g : G.t =
  match G.of_adjacency
    ['r', ['v'; 's']      ;
     'v', ['r']           ;
     's', ['r'; 'w']      ;
     'w', ['x'; 't']      ;
     't', ['w'; 'x'; 'u'] ;
     'x', ['w'; 't'; 'y'] ;
     'u', ['t'; 'y']      ;
     'y', ['x'; 'u']      ;
    ] with
  | `Ok g -> g
  | `Duplicate_key c -> failwithf "of_adjacency : duplicate key '%c'" c ()

;; Printf.printf "%s" (Sexp.to_string (G.to_sexp g))
