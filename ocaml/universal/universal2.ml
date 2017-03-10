module type UNIVERSAL = sig
  type t

  val embed : unit -> ('a -> t) * (t -> 'a option)
end;;

module type TEST = sig
  val run : unit -> unit
end;;

module type UNIVERSAL_TEST = 
  functor (U : UNIVERSAL) -> TEST;;

module Universal_exn : UNIVERSAL = struct

  type t = exn

  module type ANY = sig
    type c
    exception E of c
  end

  type 'a any = (module ANY with type c = 'a)

  module Detail = struct
  let mk : unit -> 'a any =
    fun (type s) () ->
      (module struct
        type c = s
        exception E of c
      end : ANY with type c = s)


  let inj (type s) (p : s any) (x : s) : t =
    let module Any = (val p : ANY with type c = s) in
    Any.E x

  let proj (type s) (p : s any) (y : t) : s option =
    let module Any = (val p : ANY with type c = s) in
    match y with
    | Any.E x -> Some x
    | _ ->  None
  end

  let embed () =
    let p = Detail.mk () in Detail.inj p, Detail.proj p

end;;

module Basic_usage : UNIVERSAL_TEST =
  functor (U : UNIVERSAL) -> struct
    let run () =
      let ((of_int : int -> U.t)
              , (to_int : U.t -> int option)) = U.embed () in
      let ((of_string : string -> U.t)
              , (to_string : U.t -> string option)) = U.embed () in
      let r : U.t ref = ref (of_int 13) in
      begin
        assert (to_int !r = Some 13);
        assert (to_string !r = None);
        r := of_string "foo";
        assert (to_string !r = Some "foo");
        assert (to_int !r = None);
      end
  end;;

module Hilight_abuse : UNIVERSAL_TEST =
  functor (U : UNIVERSAL) -> struct

  let run () =
    (*[of_int] and [to_int] are associated with a common module
      value 'M' say*)
    let of_int, to_int = U.embed () in
    (*[of_string] and [to_string] are associated with a different
      module value 'N' say*)
    let of_string, to_string = U.embed () in
    let r : U.t ref = ref (of_int 13) in
    (*[U.t] is an [exn], [!r] the case [M.E of int]*)
    begin
      (*This will work : [!r] matches [M.E of int]*)
      assert (to_int !r = Some 13);
      (*[!r] matches [M.E of int], it can't match [N.E of c] (note
        that at this point [c] is not fixed)*)
      ignore (try ignore (to_string !r = Some 13) with _ -> ());
      (*Unfortuantely, a side-effect of that last statement was to
        fix [c] in N to [int]! So, the following won't typecheck*)
      (*
      r := of_string "foo";
      *)
    end


end

module Highlight_more_abuse : UNIVERSAL_TEST =
  functor (U : UNIVERSAL) -> struct

  let run () =
    (*We make two calls to embed, making all types involved concrete
      along the way*)
    let ((of_int : int -> U.t)
       , (to_int : U.t -> int option)) = U.embed () in
    let ((of_int' : int -> U.t)
       , (to_int' : U.t -> int option)) = U.embed () in
    (*Use [of_int] to produce a universal value*)
    let r : U.t ref = ref (of_int 13) in
    begin
      (*Check that we can project it back out*)
      assert (to_int !r = Some 13);
      (*Uh oh, despite the types, used the wrong projection
        function*)
      assert (to_int' !r = None)
    end
end

let _ =
  let module Test = Mk_universal_test (Universal_exn) in
  Test.run ()
