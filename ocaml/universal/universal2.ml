(*The type of a module implementing a "universal" type*)
module type UNIVERSAL = sig

  (*The universal type itself*)
  type t

  (*[embed ()] returns a pair of functions : one for injection, one
    for projection*)
  val embed : unit -> ('a -> t) * (t -> 'a option)
end

(*The type of a simple module with a 'run' function*)
module type TEST = sig
  val run : unit -> unit
end

(*The type of a functor that produces a test given a module
  implementing a universal type*)
module type UNIVERSAL_TEST = functor (U : UNIVERSAL) -> TEST

(*An implementation of [UNIVERSAL] built on [exn], local modules and
  locally abstract types*)
module Universal_exn : UNIVERSAL = struct

  type t = exn

  (*Modules of the following type define a type [c] and extend [exn]
    with a constructor [E of c]*)
  module type ANY = sig
    type c
    exception E of c
  end

  (*A module value type alias*)
  type 'a any = (module ANY with type c = 'a)

  module Detail = struct
  (*[mk ()] computes a new module value*)
  let mk : unit -> 'a any =
    fun (type s) () ->
      (module struct
        type c = s
        exception E of c
      end : ANY with type c = s)

  (*[inj p x] "injects" [x] through [p] into a universal value*)
  let inj (type s) (p : s any) (x : s) : t =
    let module Any = (val p : ANY with type c = s) in
    Any.E x (*The type of this is global. 
    There are no issues with this type "escaping local scope"*)

  (*[proj p y] attempts to "project" from a univeral value [y] through
    [p] to a value of type [s]*)
  let proj (type s) (p : s any) (y : t) : s option =

    (*Unpack the module value*)
    let module Any = (val p : ANY with type c = s) in

    (*Match on the provided [exn] value*)
    match y with
    (*[y] has form [Any.E x] with [x] of type [s]*)
    | Any.E x -> Some x
      
    (*[y] is of a form [Any'.E x] (where [x] may or may not have type
      [s] - that's not relevant)*)
    | _ as e ->  None
  end

  (*[embed ()] creates a new module value and computes a pair of
    injection and projection functions from it. The returned [inj]
    function will be weakly polymoprhic : first application will "fix"
    the type [c] in the module)*)
  let embed () =
    let p = Detail.mk () in Detail.inj p, Detail.proj p

end

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

module Mk_universal_test : UNIVERSAL_TEST =
  functor (U : UNIVERSAL) -> struct

(* module Universal_test (U : UNIVERSAL) = struct *)

  (*This is how its supposed to work*)
  let test_0 () =
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

  (*A variant with a lot of problems*)
  let test_1 () =
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
        fix [c] in N to [int]!*)

      (*So, this won't typecheck*)
      (*
      r := of_string "foo";
      *)
    end

  (*Yet more problems*)
  let test_2 () =
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


  let run _ = 
    test_1 ();
    test_0 ();
    test_2 ()

end

let _ =
  let module Test = Mk_universal_test (Universal_exn) in
  Test.run ()
