(*The rules are as follows:

  - You can initialize a link set with a sink;
  - You can add links to a set such that:

    - The first link's sink must be of the set's sink type;
    - Additional links can have as sink types the set sink type or,
      the source types of previously added links.

  The approach below (credit Jeremy Yallop in
  https://sympa.inria.fr/sympa/arc/caml-list/2016-09/msg00079.html) is
  based on two ideas:

  1. Generating a fresh, unique type for each set;
  2. Encoding of a set as a collection of evidence about membership.
*)

module type S = sig

  (*Types for objects in the domain : sources, sinks, sets and links*)
  type _ sink
  type _ source
  type _ set = (string * string) list (*Why can't this be abstract?*)
  type ('source, 'sink) link

  (*A type for membership evidence. A value of type [('sink, 'set)
    accepts] is a witness that ['set] will accept a link with sink type
    ['sink]*)
  type ('sink, 'set) accepts 
  (*Abstract so that there is no means of faking evidence*)

  val mk_sink : string -> 'sink sink
  val mk_source : string -> 'source source
  val mk_link : 'source source * 'sink sink -> ('source, 'sink) link

  (*There are two operations on sets : creating a fresh set and adding
    a link to a set. In both cases the operation returns multiple
    values, accordingly we use records for the return
    types. Furthermore, both operations create fresh types (since sets
    have unique types), so we use records with existential types*)

  (*The result of the [create_set] operation*)
  type 'sink fresh_set = 
  | Fresh_set : {
    set : 'set set;
    accepts : ('sink, 'set) accepts;
  } -> 'sink fresh_set

  (*
    There's a type parameter ['sink] which will be instantiated with
    the type of the sink used to create the set and two fields,

    1. [set] is the new set;
    2. [accepts] is evidence that [set] will accept a link with sink
       type ['sink].

    ['sink] is a type parameter since we need to ensure that it is the
    same as another type in the program (the sink used to create the
    set), but, ['set] is existential, since we need to ensure that it
    is distinct from every other type in the program (since it
    uniquely identifies [set]).
  *)

  (*The [create_set] operation builds a fresh set from a sink*)
  val create_set : 'sink sink -> 'sink fresh_set

  (*The type of the result of the [add_link] operation*)
  type ('sink, 'parent) augmented_set =
  | Augmented_set : {
    set : 'set set;
    accepts: ('sink, 'set) accepts;
    cc : 's. ('s, 'parent) accepts -> ('s, 'set) accepts
  } -> ('sink, 'parent) augmented_set

  (*This time there are three elements to the result:

    1. [set] is the new set (as before);
    2. [accepts] is evidence that [set] will accept a link with sink
       type [sink] (as before)
    3. [cc] is a capability converter, that turns evidence that the
       "parent" set will accept a link into evidence that [set] will
        accept the same link.

    Another way of looking at this is that [augmented_set] bundles the
    new set up along with evidence that ['sink] is a member and
    evidence that every member of the parent set is also a member.

    It's also worth looking again at the scope of the type variables:
    ['sink] and ['parent] are parameters since they correspond to the
    types of inputs to the operation; ['set] is fresh and ['s] is
    universally-quantified since the capability must work for any
    piece of evidence involving the parent.
   *)

  (*The [insert_link] operation takes three arguments: the link to
    insert, the set into which the link is inserted and evidence that
    the insertion is acceptable *)
  val insert_link : 
    ('source, 'sink) link ->
    'parent set -> 
    ('sink, 'parent) accepts ->
    ('source, 'parent) augmented_set

end

module M : S = struct

  type 'sink sink = { name : string }
  type 'source source = { name : string }

  type 'set set = (string * string) list
  type ('source, 'sink) link = ('source source * 'sink sink)

  let mk_sink (name : string) : 'sink sink = {name}
  let mk_source (name : string) : 'source source = {name}
  let mk_link ((source, sink) : 'source source * 'sink sink) 
      : ('source, 'sink) link = (source, sink)

  type ('sink, 'set) accepts = 
  | Accepts : ('sink, 'set) accepts

  type 'sink fresh_set = 
  | Fresh_set : {
    set : 'set set;
    accepts : ('sink, 'set) accepts; 
    }                        -> 'sink fresh_set

  let create_set (s : 'sink sink) : 'sink fresh_set =
    Fresh_set { set = ([] : 'set set); 
                accepts = (Accepts : ('sink, 'set) accepts) }
  
  type ('sink, 'parent) augmented_set =
  | Augmented_set : {
    set : 't set;
    accepts: ('sink, 't) accepts;
    cc : 's. ('s, 'parent) accepts -> ('s, 't) accepts
  } -> ('sink, 'parent) augmented_set

  let insert_link 
      (l : ('source, 'sink) link) 
      (s : 'parent set)
      (a : ('sink, 'parent) accepts)  : ('source, 'parent) augmented_set =
    let {name = src} : 'source source = fst l in
    let {name = dst} : 'sink sink  = snd l in
    Augmented_set {
      set : 't set = (src, dst) :: s;
      accepts = (Accepts : ('source, 't) accepts);
      cc = fun (_ : (_, 'parent) accepts) -> (Accepts : (_, 'parent) accepts)
    }

end

module Test (E : S) = struct

  open E

  type t1 and t2 and t3 and t4

  let snk1 : t1 sink = mk_sink "sink1"
  let snk2 : t2 sink = mk_sink "sink2"
  let snk3 : t4 sink = mk_sink "sink3"

  let src1 : t2 source = mk_source "source1"
  let src2 : t3 source = mk_source "source2"

  let link1 : (t2,  t1) link = mk_link (src1, snk1) (*t2 src, t1 sink*)
  let link2 : (t3,  t1) link = mk_link (src2, snk1) (*t3 src, t1 sink*)
  let link3 : (t3,  t2) link = mk_link (src2, snk2) (*t3 src, t2 sink*)
  let link4 : (t3,  t4) link = mk_link (src2, snk3) (*t3 src, t4 sink*)

  let test () = 

    (*Create a fresh set from a sink of type [t1]*)
    let (Fresh_set {set; accepts = a} : t1 fresh_set) = 
      create_set snk1 in
    (*
      - [a] is evidence [set] accepts links with sink type [t1]
    *)

    (*Insert a [(t2, t1) link]*)
    let Augmented_set 
        {set = set1; accepts = a1; cc = cc1} = 
      insert_link link1 set a in
    (*
      - [a1] is evidence [set1] accepts links with sink type [t2] ([t2] is
        the source type of [link1])
      - [cc] says that [set1] accepts links with sink types that its
        parent [set] does:
        - [cc1 a] provides evidence that says that [set1] will accept
          [link2] which has sink type [t1] *)

    (*Insert a [(t3, t1)] link*)
    let Augmented_set
        {set = set2; accepts = a2; cc = cc2} =
      insert_link link2 set (cc1 a) in
    (*
      - [a2] says that [set2] accepts links with sink type [t3] ([t3] is
        the source type of [link2])
        - [cc2] says that [set2] accepts links with sink types that its
          parent does:
        - [cc2 a1] provides evidence that says that [set2] will accept
          [link3] which has sink type [t2]
    *)

    (*Insert a [(t3, t2)] link*)
    let (Augmented_set
        {set = set3; accepts = a3; cc = cc3} : (t3, _) augmented_set) =
      insert_link link3 set (cc2 a1) in
    (*
      - [a3] says that [set3] accepts links with sink type [t3] ([t3]is
        the source type of [link3])
      - [cc3] says that [set3] accepts links with sink types that its
        parent does (that is, any links with sink types [t1], [t2] or [t3])
    *)

    (*There is just no way we can get insert [link4] into [set3]. The
      is no evidence we can produce that will allow a link with sink
      type [t4]. Try the below with any of [a1], [a2], [a3])*)
    (*
    let (Augmented_set
       {set = set4; accepts = a4; cc = cc4} =
       insert_link link4 set (cc3 a3) : (t3, _) augmented_set) in
    *)

    ()
end

let _ = let module T = Test (M) in T.test ()

