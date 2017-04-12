(*http://okmij.org/ftp/papers/lightweight-static-capabilities.pdf*)

(*Signature of the list datatype*)
module type LIST = sig
  type 'a cnt                            type 'a extern_cnt_t
  val make : 'a extern_cnt_t -> 'a cnt   val show : 'a cnt -> 'a extern_cnt_t
  val empty : 'a cnt                     val null : 'a cnt -> bool
  val cons : 'a -> 'a cnt -> 'a cnt      val head : 'a cnt -> 'a      
  val tail : 'a cnt -> 'a cnt
end;;

(*The signature of a module implementing operations on lists*)
module type LIST_OPS = sig
  type 'a cnt

  val rev : 'a cnt -> 'a cnt
end;;

(*Safe operations on lists*)
module Full_list_ops (L : LIST) : LIST_OPS with type 'a cnt = 'a L.cnt  =
struct
  (* Alias *)
  type 'a cnt = 'a L.cnt 

  (*Signature [FULL_LIST] extends and adapts [LIST] via modular
    inclusion and adaption*)
  module type FULL_LIST = sig
    include LIST
    (*[make l onn onf] takes two continuations, one to invoke if [l]
      is empty, the second to invoke if not*)
    val choose : 'a cnt -> (unit -> 'w) -> ('a cnt -> 'w) -> 'w
  end;;

  (*[Full_list] implements a functor producing modules satisfying
    [FULL_LIST] connecting the contained [type 'a cnt] with that of
    ['a L.cnt]*)
  module Full_list (L : LIST) : FULL_LIST with type 'a cnt = 'a L.cnt =
  struct
    include L
    let choose l on_null on_full = if null l then on_null () else on_full l
  end

  (*An instantiation of [FULL_LIST] specialized to [L]*)
  module F = Full_list (L)

  (*Finally, [rev]*)
  let rev (l : 'a cnt) : 'a cnt =
    let rec loop (l : 'a cnt) (acc : 'a cnt) : 'a cnt = 
      F.choose l 
        (fun () -> acc) (*Case [l] empty*)
        (fun l -> loop (F.tail l) (F.cons (F.head l) acc)) (*Case [l] full*)
        in
    loop l (F.empty)

end;;

(*A test functor for lists that use ['a list] for external value
  representation*)
module Test_list_ops (L : LIST with type 'a extern_cnt_t = 'a list) = 
struct
  (*Compute a [LIST_OPS] over [L]*)
  module List_ops : 
    LIST_OPS with type 'a cnt = 'a L.cnt = 
    Full_list_ops (L)

  (*Compute some list reversals*)
  let l = L.show (List_ops.rev (L.make []))
  let l' = L.show (List_ops.rev (L.make [1]))
  let l'' = L.show (List_ops.rev (L.make [1; 2]))
end;;

(*An implementation of [LIST] that uses ['a list]* for external value
  representation*)
module Functional_list : LIST with type 'a extern_cnt_t = 'a list =
struct
  type 'a extern_cnt_t = 'a list
  type 'a cnt = | Empty | Cons of 'a * 'a cnt

  let make xs =
    let rec loop acc xs =
      if xs = [] then acc
      else Cons (List.hd xs, loop acc (List.tl xs)) in
    loop Empty xs
  let show xs =
    let rec loop acc = function 
      | Empty -> List.rev acc
      | Cons (x, xs) -> loop (x :: acc) xs in
    loop [] xs

  let empty = Empty
  let null = function | Empty -> true | _ -> false
  let cons x xs = Cons (x, xs)

  (*Intentionally partial (we intend to rely on static capabilities to
    guarantee they will never called on empty lists)*)
  let head = function | Cons (x, _) -> x 
  let tail = function | Cons (_, xs) -> xs
     
end;;

module M = Test_list_ops (Functional_list);;
let () = assert (M.l = [])
let () = assert (M.l' = [1])
let () = assert (M.l'' = [2; 1])

