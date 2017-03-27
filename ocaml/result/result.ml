(*Result value combinators. 

  A study of {{:https://github.com/dbuenzli/rresult/}Daniel C. Bünzli's
  program}.
*)

(*The type of result values in modular form*)
module type RESULT = sig

  (*Results*)

  (*Type of a result*)
  type ('a, 'b) result = 
    ('a, 'b) Pervasives.result = | Ok of 'a | Error of 'b

  (*[>>] is [R.( >>| ) ]*)
  val ( >>| ) : ('a, 'b) result -> ('a -> 'c)  -> ('c, 'b) result

  (*[>>=] is [R.( >>= ) ]*)
  val ( >>= ) : ('a, 'b) result -> ('a -> ('c, 'b) result)  -> ('c, 'b) result

  module R : sig
    (*Type alias*)
    type ('a, 'b) t = ('a, 'b) result

    (*[ok a] is [Ok a]*)
    val ok : 'a -> ('a, 'b) t

    (*[error b] is [Error b]*)
    val error : 'b -> ('a, 'b) t

    (*[return a] is [ok a]*)
    val return : 'a -> ('a, 'b) t

    (*[fail b] is [error b]*)
    val fail : 'b -> ('a, 'b) t

    (*[reword_error reword r] is [r] if [r = Ok _] else if [r = Error
      e] then [Error (reword e)] *)
    val reword_error : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t

    (*[get_ok r] is [v] if [r = Ok v] else @raise Invalid_argument*)
    val get_ok : ('a, 'b) t -> 'a

    (*[get_error r] is [b] if [r = Error b] else @raise Invalid_argument*)
    val get_error : ('a, 'b) t -> 'b

    (*[map f r] if [r = Ok x] is [Ok (f x)] else [r]*)
    val map : ('a -> 'c) -> ('a, 'b) t -> ('c, 'b) t

    (*[bind r f] if [r = Ok x] is [f x] else [r]*)
    val bind : ('a, 'b) t -> ('a -> ('c, 'b) t)  -> ('c, 'b) t

    (*[join r] if [r = Ok x] is [x] else [r]*)
    val join : (('a, 'b) t, 'b) t -> ('a, 'b) t

    (*[r >>| f] is [map f r]*)
    val ( >>| ) : ('a, 'b) t -> ('a -> 'c)  -> ('c, 'b) t
      
    (*[r >>= f] is [bind r f]*)
    val ( >>= ) : ('a, 'b) t -> ('a -> ('c, 'b) t)  -> ('c, 'b) t

  end;;

  (* -- *)

  (* Error messages *)

  (*A type for error messages*)
  type msg = [`Msg of string]

  (*[msg s] returns an error message of [s]*)
  val msg : string -> [> msg]

  (*[msgf fmt] computes a function according to a format.  ['a]
    unifies with the function type, [[> msg]] is the return type of
    the function computed*)
  val msgf : ('a, Format.formatter, unit, [>msg]) format4 -> 'a
  (*Example:
    {[
    # Result.msgf "Frobincation threshold : %d" 3;; 
    - : [> Result.msg ] = `Msg "Frobincation threshold : 3"
    ]}
  *)

  (*[pp_msg ppf m] prints [m] on [ppf]*)
  val pp_msg : Format.formatter -> msg -> unit

  (*[error_msg s] is [R.error (`Msg s)] *)
  val error_msg : string -> ('a, [> msg]) result

  (*[error_msgf fmt] computes a function. The function type unifies
    with ['a], the return type of the function is [('d, [> `Msg])
    result]*)
  val error_msgf : ('a, Format.formatter, unit, ('d, [> msg]) result) format4 
    -> 'a

  (*[reword_error_msg ~replace reword r] is like [reword_error]
    except if [replace] is [false] (the default), the result of
    [reword old_msg] is concatenated, on a new line to the old
    message*)
  val reword_error_msg : ?replace:bool -> (string -> msg) ->
    ('a, msg) result -> ('a, [> msg]) result

  (*[error_to_msg pp_error r] converts errors in [r] with [ppf_error]
    to an error message*)
  val error_to_msg : pp_error:(Format.formatter -> 'b -> unit) ->
    ('a, 'b) result -> ('a, [> msg]) result
  (*Example:
   {[
   # type err_t = {ctx:string; num:float};;
   # let pp_err_t ppf {ctx; num} = fprintf ppf "%s %12.6f" ctx num;;
   # let r : ('a, err_t) result = R.error {ctx="foo"; num=3.14};;
   # error_to_msg pp_err_t r;;

   - : ('a, [> Result.msg ]) Result.result = Error (`Msg "foo     3.140000")
   ]}
  *)

  (*[error_msg_to_invalid_arg r] is [a] if [r = Ok a] else @raise
    Invalid_argument*)
  val error_msg_to_invalid_arg : ('a, msg) result -> 'a
  (* Example (cont. from above):
   {[
   # error_msg_to_invalid_arg (error_to_msg pp_err_t r);;

   Exception: Invalid_argument "foo     3.140000".
   ]}
  *)

  (*[open_err_msg r] promotes its argument*)
  val open_error_msg : ('a, msg) result -> ('a, [> msg]) result

  (* -- *)

  (*Trapping unexpected exceptions*)

  (*The type of a trapped exception*)
  type exn_trap = [`Exn_trap of exn * Printexc.raw_backtrace]

  (*[pp_exn_trap ppf t] pretty-prints [t] on [ppf]*)
  val pp_exn_trap : Format.formatter -> exn_trap -> unit

  (*[trap_exn f a] is [f a] and traps any exception that may occur*)
  val trap_exn : ('a -> 'b) -> 'a -> ('b, [> exn_trap]) result
  (*Example:
   {[
   # Result.pp_exn_trap std_formatter 
     (R.get_error (Result.trap_exn (fun _ -> failwith "foo") ()));;

   Failure("foo")
   - : unit = ()
   ]}
   *)

  (*[error_exn_trap_to_msg r] converts trapped exception errors in
    [r] to a message *)
  val error_exn_trap_to_msg : ('a, exn_trap) result -> ('a, [> msg]) result
  (*Example:
   {[
   # Result.error_exn_trap_to_msg (
       Result.trap_exn (fun _ -> failwith "foo") ());;

   - : ('a, [> Result.msg ]) Result.result =
   Error (`Msg "Unexpected exception :\nFailure(\"foo\")\n")
   ]}
  *)

  (* -- *)

  (*Pretty printing*)

  (*[pp_result ok error ppf r] prints [r] on [pp] using [ok] or
    [error] depending on case*)
  val pp_result :
   ok : (Format.formatter -> 'a -> unit) ->
   error : (Format.formatter -> 'b -> unit) ->
    Format.formatter -> ('a, 'b) result -> unit

  (*[dump ok error ppf r] formats [r] on [pp] using [ok] or [error]
    depending on case*)
  val dump : 
    ok : (Format.formatter -> 'a -> unit) ->
    error : (Format.formatter -> 'b -> unit) ->
    Format.formatter -> ('a, 'b) result -> unit

  (* -- *)

  (*Predicates and comparisons*)

  (*[is_ok r] is [true] if [r = Ok _] else [false]*)
  val is_ok : ('a, 'b) result -> bool

  (*[is_error r] is [true] if [r = Error _] else [false]*)
  val is_error : ('a, 'b) result -> bool

  (*[compare ~ok ~error r r'] totally orders [r] and [r'] using [ok]
    and [error]*)
  val compare : 
    ok : ('a -> 'a -> int) -> 
    error : ('b -> 'b -> int) -> 
    ('a, 'b) result -> ('a, 'b) result -> int

  (* -- *)

  (*Conversions*)

  (*[to_option r] is [Some a] if [r = Ok a] else if [r = Err b] then
    [None]*)
  val to_option : ('a, 'b) result -> 'a option

  (*[of_option ~none r] is [Ok a] if [r = Some a] else if [r = None]
    then [none ()]*)
  val of_option :
    none : (unit -> ('a, 'b) result) -> 'a option -> ('a, 'b) result

  (*[to_presult r] is [r] reinterpreted as a polymorhpic variant
    result value*)
  val to_presult : ('a, 'b) result -> [> `Ok of 'a | `Error of 'b]

  (*[of_result pr] is [pr] reinterpred as a result value*)
  val of_presult : [< `Ok of 'a | `Error of 'b] -> ('a, 'b) result
    
  (* -- *)

  (*Ignoring errors*)

  (*[ignore_error ~use r] is [a] if [r = Ok a] else if [r = Error b]
    then [use b]*)
  val ignore_error : use : ('b -> 'a) -> ('a, 'b) result -> 'a

  (*[kignore_error ~use r] is [r] if [r = Ok _] else if [r = Error
    b] then [use b]*)
  val kignore_error :
    use : ('b -> ('a, 'c) result) -> ('a, 'b) result -> ('a, 'c) result

end;;

module Result : RESULT = struct

  type ('a, 'b) result =
    ('a, 'b) Pervasives.result = | Ok of 'a | Error of 'b

  module R = struct

    type ('a, 'b) t = ('a, 'b) result

    let err_ok = "Result is of case (Ok _)"
    let err_error = "Result is of case (Error _)"

    let ok a = Ok a
    let error b = Error b
    let return = ok
    let fail = error

    let get_ok = function 
      | Ok a -> a | _ -> invalid_arg err_error

    let get_error = function 
      | Error b -> b | _ -> invalid_arg err_ok

    let reword_error reword = function
      | Ok _ as r -> (r :> ('a, 'c) result)
      | Error e -> Error (reword e)

    let bind r f = match r with
      | Ok x -> f x | Error _ as e -> (e :> ('c, 'b) t)

    let join = function
      | Ok v -> v | Error _ as e -> (e :> ('a, 'b) t)

    let map f r = match r with
      | Ok x -> Ok (f x) | Error _ as e -> (e :> ('c, 'b) t)

    let ( >>| ) r f = map f r
    let ( >>= ) r f = bind r f

  end

  let ( >>| ) = R.( >>| )
  let ( >>= ) = R.( >>= )

  type msg = [`Msg of string]

  let msg s = `Msg s

  let msgf fmt = 
    let kmsg _ = `Msg (Format.flush_str_formatter()) in
    Format.kfprintf kmsg Format.str_formatter fmt

  (*[pp_lines ppf s] prints [s] on [ppf], a line at a time*)
  let pp_lines ppf s =
    let left = ref 0 
    and right = ref 0 
    and len = String.length s in
    let flush () =
      Format.pp_print_string ppf (String.sub s !left (!right - !left));
      incr right; left := !right in
    while (!right <> len) do
      if s.[!right] = '\n' then 
        (flush (); Format.pp_force_newline ppf ()) 
      else incr right;
    done;
    if !left <> len then flush ()

  let pp_msg ppf (`Msg m) = pp_lines ppf m

  let error_msg s = Error (`Msg s)

  let error_msgf fmt =
    let kerr _ = error_msg (Format.flush_str_formatter ()) in
    Format.kfprintf kerr Format.str_formatter fmt

  let reword_error_msg ?(replace = false) reword = function
    | Ok _ as r -> r
    | Error (`Msg e) ->
      let (`Msg e' as v) = reword e in
      if replace then Error v else error_msgf "%s\n%s" e e'

  let error_to_msg ~pp_error = function
    | Ok _ as a -> a
    | Error e -> error_msgf  "%a" pp_error e
      
  let error_msg_to_invalid_arg = function
    | Ok a -> a | Error (`Msg m) -> invalid_arg m

  let open_error_msg = function 
    | Ok _ as a -> (a :> ('a, [> msg]) result)
    | Error (`Msg _) as r -> (r :> ('a, [> msg]) result)
    
  type exn_trap = [`Exn_trap of exn * Printexc.raw_backtrace]

  let pp_exn_trap ppf (`Exn_trap (exn, trace)) =
    Format.fprintf ppf "%s@\n" (Printexc.to_string exn);
    pp_lines ppf (Printexc.raw_backtrace_to_string trace)

  let trap_exn f a = try Ok (f a)
    with
    | e -> Error (`Exn_trap (e, Printexc.get_raw_backtrace ()))

  let error_exn_trap_to_msg = function
    | Ok _ as a -> a
    | Error (`Exn_trap (_, _) as trap) ->
      error_msgf "Unexpected exception :@\n%a" pp_exn_trap trap

  let pp_result ~ok ~error ppf = function
    | Ok a -> ok ppf a
    | Error b -> error ppf b

  let dump ~ok ~error ppf = function
    | Ok a -> Format.fprintf ppf "@[<2>Ok@ @[%a@]@]" ok a
    | Error b -> Format.fprintf ppf "@[<2>Error@ @[%a@]@]" error b

  let is_ok = function | Ok _ -> true | _ -> false

  let is_error = function | Error _ -> true | _ -> false

  let compare ~ok ~error r r' = match (r, r') with
    | Ok v, Ok v' -> ok v v'
    | Error v, Error v' -> error v v'
    | Ok _, Error _ -> -1
    | Error _, Ok _ -> 1

  let to_option = function | Ok a -> Some a | _ -> None

  let of_option ~none = function | Some a -> Ok a | None -> none ()

  let to_presult = function | Ok a -> `Ok a | Error b -> `Error b

  let of_presult = function | `Ok a -> Ok a | `Error b -> Error b

  let ignore_error ~use = function | Ok a -> a | Error b -> use b

  let kignore_error ~use = function | Ok _ as a -> a | Error b -> use b

end;;

(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli
   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.
   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 ---------------------------------------------------------------------------*)
