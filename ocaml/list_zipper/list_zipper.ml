module type Z_sig = sig
    type 'a t

    val empty : 'a t
    val from_list : 'a list -> 'a t
    val to_list : 'a t -> 'a list

    val go_left : 'a t -> 'a t
    val go_right : 'a t -> 'a t

    val insert : 'a -> 'a t -> 'a t
    val delete : 'a t -> 'a t

end

module Z : Z_sig = struct

  type 'a t = Z of 'a list * 'a list

  let empty : 'a t = Z ([], [])

  let from_list : 'a list -> 'a t = 
    fun l -> Z ([], l)

  let to_list : 'a t -> 'a list = 
    function | Z (ls, rs) -> List.rev ls @ rs

  let go_right : 'a t -> 'a t = function
    | Z (ls, h :: tl) -> Z (h :: ls, tl)
    | z -> z

  let go_left : 'a t -> 'a t = function
    | Z (h :: tl, rs) -> Z (tl, h :: rs)
    | z -> z

  let insert (a : 'a) : 'a t -> 'a t = function
    | Z (ls, rs) -> Z (ls, a :: rs)

  let delete : 'a t -> 'a t = function
    | Z (ls, _ :: rs) -> Z (ls, rs)
    | z -> z

end
