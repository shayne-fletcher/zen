(*A list interface*)
module type List_ops = sig
  type 'a t

  val nil : unit -> 'a t
  val hd : 'a t -> 'a
  val tl : 'a t -> 'a t
  val cons : 'a -> 'a t -> 'a t
  val empty : 'a t -> bool
  val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b

end

(*The type of a list. The type ['a] is exposed but the type [t] is
  abstract*)
module type List_t = sig
  type a
  include List_ops
  val list : a t
end

(*An alias for the type of lists*)
type 'a list_t = (module List_t with type a = 'a)

(*An implementation of [List_ops] using built-in lists *)
module Builtin_list : List_ops = struct
  type 'a t = 'a list

  let nil : unit -> 'a t = fun() -> []
  let hd : 'a t -> 'a = List.hd
  let tl : 'a t -> 'a t = List.tl
  let cons : 'a -> 'a t -> 'a t = fun x l -> x :: l
  let empty : 'a t -> bool = function | [] -> true | _ -> false
  let fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b = List.fold_left
end

(*An implementation of [List_ops] using "cons cells"*)
module Cons_list : List_ops = struct
  type 'a t = Nil | Cons of 'a * 'a t

  let nil : unit -> 'a t = 
    fun () -> Nil

  let hd : 'a t -> 'a = function 
    | Cons (h, _)  -> h 
    | _ -> failwith "Cons_list.hd"

  let tl : 'a t -> 'a t = function 
    | Cons (_, t)  -> t 
    | _ -> failwith "Cons_list.tl"

  let cons : 'a -> 'a t -> 'a t = 
    fun x l -> Cons (x, l)

  let empty : 'a -> bool = function 
    | Cons (_, _) -> false 
    | Nil -> true

  let rec fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b =
    fun f z l -> 
      let loop acc = function
        | Nil -> acc
        | Cons (e, tl) -> fold_left f (f acc e) tl in
      loop z l
end

(*List operations as stand-alone functions*)

let nil : 'a. (module List_ops) -> 'a list_t =
  fun (type s) l ->
    (module
      struct
        type a = s
        include (val l : List_ops)
        let list = nil ()
      end : List_t with type a = s)

let hd : 'a. 'a list_t -> 'a =
  fun (type s) l ->
    let module M = (val l : List_t with type a = s) in
    M.hd M.list

let tl : 'a. 'a list_t -> 'a list_t =
  fun (type s) l ->
    (module
     struct
       include (val l : List_t with type a = s)
       let list = tl list
     end : List_t  with type a = s
    )

let empty : 'a. 'a list_t -> bool =
  fun (type s) l ->
    let module M = (val l : List_t with type a = s) in
    M.empty M.list

let fold_left : 'a. ('b -> 'a -> 'b) -> 'b -> 'a list_t -> 'b =
  fun (type s) f z l ->
    let module M = (val l : List_t with type a = s) in
    M.fold_left f z (M.list)

let cons : 'a. 'a -> 'a list_t -> 'a list_t =
  fun (type s) e l ->
    (module
     struct
       include (val l : List_t with type a = s)
       let list = cons e list
     end : List_t with type a = s
    )

let to_list : 'a. 'a list_t -> 'a list =
  fun l -> 
    List.rev @@ fold_left (fun acc e -> e :: acc) [] l

(*Test*)

module type Test_sig = sig
  val l : int list
  val s : int
end

module Test (M : List_ops) : Test_sig = struct
  let y : int list_t = (cons 3 (cons 2 (cons 1 (nil (module M)))))
  let s : int = fold_left (fun x y -> x + y) 0 y
  let l : int list = to_list y
end

let test_builtin_list : (module Test_sig) = 
  (module Test (Builtin_list) : Test_sig)
let l : int list =
  let module M : Test_sig = (val test_builtin_list : Test_sig) in
  M.l

let x : int = 
  let module M : Test_sig = (val test_builtin_list : Test_sig) in 
  M.s

let test_cons_list : (module Test_sig) = 
  (module Test (Cons_list): Test_sig)
let y : int =
  let module M : Test_sig = (val test_cons_list : Test_sig) in
  M.s
