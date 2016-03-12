module type Univ = sig
  type t
  val embed : unit -> ('a -> t) * (t -> 'a option)
end

module U : Univ = struct
  type t = (unit -> unit) * (unit -> unit)

  let embed () =
    let r = ref None in
    let put x = (fun () -> r := Some x), (fun () -> r := None) in
    let get (f, g) = f (); let res = !r in g (); res in
    put, get
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
