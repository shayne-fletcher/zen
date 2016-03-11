module type Univ = sig
  type t
  val embed : unit -> ('a -> t) * (t -> 'a option)
end

module U : Univ = struct

  type t = exn

  module type S = sig 
    type t 
    exception E of t 
  end

  type 'a prop = (module S with type t = 'a)

  let create : unit -> 'a prop =
    fun (type s) () ->
      let module M = struct 
        type t = s 
        exception E of t
      end in
      (module M : S with type t = s)

  let inj (type s) (p : s prop) (x : s) : t =
   let module M = (val p : S with type t = s) in
   M.E x

  let proj (type s) (p : s prop) (y : t) : s option =
    let module M = (val p : S with type t = s) in
    match y with
    | M.E x -> Some x 
    | _ -> None

  let embed () : ('a -> t) * (t -> 'a option) = 
    let p = create () in 
    inj p, proj p

end

module Test (M : Univ) = struct

  let () = 
    let ((of_int : int -> M.t), (to_int : M.t -> int option)) = 
      M.embed () in
    let ((of_string : string -> M.t), (to_string : M.t -> string option)) = 
      M.embed () in

    let r : M.t ref = ref (of_int 13) in
    begin
      assert (to_int !r = Some 13);
      assert (to_string !r = None);
      r := of_string "foo";
      assert (to_int !r = None);
      assert (to_string !r = Some "foo");
    end

end

module M = Test (U)
